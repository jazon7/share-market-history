#######################################################
# Charts are plotly.js figures built as plain lists (data, layout, config)
# and drawn in the browser by Plotly.react, which updates the chart in
# place. No plotly R package: its build and serialisation step was most of
# the time per click.
#######################################################
fig_config <- list(displayModeBar = FALSE, responsive = TRUE)

# add a trace; x and y always stay arrays in the JSON (I()), without
# missing points (e.g. before a rolling window fills), rounded to 7
# significant digits to keep the payload small
fig_trace <- function(fig, x, y, ...){
  num <- function(v) if(is.numeric(v)) signif(v, 7) else v
  ok <- !is.na(x) & !is.na(y)
  fig$data[[length(fig$data) + 1]] <- c(list(x = I(num(x[ok])), y = I(num(y[ok]))), list(...))
  fig
}
fig_layout <- function(fig, ...){
  fig$layout <- list(...); fig$config <- fig_config
  fig
}
# plotly.js treats {} as a value, so drop NULL entries (e.g. options
# switched off with if()) before sending
drop_nulls <- function(x){
  if(!is.list(x) || inherits(x, "AsIs")) return(x)
  lapply(x[!vapply(x, is.null, logical(1))], drop_nulls)
}

figureOutput <- function(outputId, height = "400px"){
  div(id = outputId, class = "figure-output", role = "img", `aria-label` = "Chart",
      style = paste0("width: 100%; height: ", height, ";"))
}
# The expression returns themed_figure(): drawn in the browser by www/app.js
renderFigure <- function(expr){
  func <- shiny::quoToFunction(rlang::enquo0(expr), "renderFigure")
  shiny::createRenderFunction(func, function(value, session, name, ...) value, figureOutput)
}

# Light and dark versions of a figure differ only in colours and opacities,
# so the browser gets the light figure plus the leaves that change for dark
# (path from the root, 0-based for arrays, and the dark value), and switches
# theme itself. `build` is a function(theme) returning a figure.
# `tall`: stacked rows, which get more height on phones.
themed_figure <- function(build, tall = FALSE){
  light <- drop_nulls(build("light")); dark <- drop_nulls(build("dark"))
  list(fig = light, dark = fig_patches(light, dark), tall = tall)
}
fig_patches <- function(a, b, path = list()){
  branch <- function(x) is.list(x) && !inherits(x, "AsIs")
  if(branch(a) && branch(b) && length(a) == length(b) && identical(names(a), names(b))){
    keys <- if(is.null(names(a))) as.list(seq_along(a) - 1L) else as.list(names(a))
    out <- list()
    for(i in seq_along(a)) out <- c(out, fig_patches(a[[i]], b[[i]], c(path, keys[i])))
    return(out)
  }
  if(identical(a, b)) list() else list(list(p = path, v = b))
}

# spread end labels so they don't collide: positions in axis units
spread_labels <- function(y, min_gap){
  o <- order(y); v <- y[o]
  for(k in 1:50){
    moved <- FALSE
    for(i in seq_along(v)[-1]) if(v[i] - v[i - 1] < min_gap){
      mid <- (v[i] + v[i - 1]) / 2
      v[i - 1] <- mid - min_gap / 2; v[i] <- mid + min_gap / 2; moved <- TRUE
    }
    if(!moved) break
  }
  y[o] <- v; y
}

dollar_ticks <- function(lo, hi, log = FALSE){
  v <- if(log){
    steps <- list(1, c(1, 2, 5), c(1, 1.5, 2, 3, 5, 7), c(1, 1.2, 1.4, 1.6, 1.8, 2, 2.5, 3, 4, 5, 6, 7, 8, 9))
    for(m in steps){
      v <- sort(outer(m, 10^(floor(log10(lo)):ceiling(log10(hi)))))
      v <- v[v >= lo & v <= hi]
      if(length(v) >= 3) break
    }
    if(length(v) >= 3) v else pretty(c(lo, hi), 4)
  } else pretty(c(lo, hi), 5)
  v <- v[v >= lo & v <= hi]
  list(tickmode = "array", tickvals = I(v), ticktext = I(fmt_tick(v)))
}

# line chart
#######################################################
line_plot <- 
  function(df,
           y_col,
           x_range,
           y_format = NULL,      # d3 format string for hover
           tick_format = y_format,
           log_y = FALSE,
           y_range = NULL,       # in data units
           hline = NULL,
           band = NULL,          # list(lo, hi, med, colour): typical-range band
           events = NULL,        # data frame(year, label): dotted markers with notes on top
           zero_label = NULL,    # note above the 0 line at the left (e.g. "New high")
           narrow = FALSE,       # phone width: label fewer events
           theme = "light"){
    ink <- inks[[theme]]
    
    df <- df %>% 
      inner_join(series, by = "market") %>% 
      mutate(label = factor(label, levels = series$label)) %>% 
      arrange(label, year)
    df$y <- df[[y_col]]
    
    # last point of each series: end dot + direct label
    ends <- df %>% 
      filter(!is.na(y)) %>% 
      group_by(label) %>% 
      slice_max(year, n = 1) %>% 
      ungroup()
    tr <- if(log_y) log10 else identity
    span <- diff(range(tr(df$y), na.rm = TRUE))
    
    pal <- series_colours(theme)
    {
      p <- list(data = list())
      for(lab in rev(levels(droplevels(df$label)))){     # lead series drawn last, on top
        d <- df[df$label == lab, ]
        lead <- lab == series$label[1]
        p <- p %>% 
          fig_trace(d$year, d$y, name = lab,
                    type = "scatter", mode = "lines",
                    line = list(color = pal[[lab]], width = if(lead) 2.25 else 1.6,
                                shape = "linear"),
                    hovertemplate = paste0("%{y:", y_format, "}"))
      }
      p <- p %>% 
        fig_trace(ends$year, ends$y, type = "scatter", mode = "markers",
                  marker = list(color = unname(pal[as.character(ends$label)]), size = 7,
                                line = list(color = ink$page, width = 2)),
                  cliponaxis = FALSE, hoverinfo = "skip", showlegend = FALSE)
    }
    
    tick_font <- list(size = 11.5, color = ink$muted)
    dollars <- identical(tick_format, "$~s")      # dollar axes get their own ticks
    y_axis <- list(title = "",
                   tickformat = if(!dollars) tick_format,
                   tickfont = tick_font,
                   gridcolor = ink$grid, gridwidth = 1,
                   zeroline = FALSE, showline = FALSE,
                   ticks = "", nticks = 7,
                   automargin = FALSE,
                   fixedrange = TRUE)
    if(log_y){
      y_axis$type <- "log"
      if(!is.null(y_range)) y_axis$range <- log10(y_range) + c(-0.05, 0.05)
      if(dollars){
        r <- if(!is.null(y_range)) y_range else range(df$y, na.rm = TRUE)
        y_axis <- c(y_axis, dollar_ticks(r[1] / 10^0.03, r[2] * 10^0.03, log = TRUE))
      } else y_axis$dtick <- 1                # one tick per power of 10
    } else if(!is.null(y_range)){
      y_axis$range <- y_range
    }
    
    # right-margin labels: series names at their end points, plus the band's
    # edges and median when a band is shown; spread together so none collide
    m <- nrow(ends)                       # 0 for single-series columns
    lab <- data.frame(y = tr(ends$y), text = ends$short, x = ends$year, xref = rep("x", m),
                      colour = rep(ink$secondary, m), size = rep(12, m))
    if(!is.null(band)) lab <- rbind(lab, data.frame(
      y = c(band$hi, band$med, band$lo), text = c("\u2191 Best 10%", "Median", "\u2193 Worst 10%"),
      x = 1, xref = "paper", colour = ink$muted, size = 11))
    lab$y_lab <- spread_labels(lab$y, max(span, 1e-6) * 0.055)
    labels <- lapply(seq_len(nrow(lab)), function(i) list(
      x = lab$x[i], xref = lab$xref[i], y = lab$y_lab[i], text = lab$text[i],
      xanchor = "left", xshift = 10, showarrow = FALSE,
      font = list(size = lab$size[i], color = lab$colour[i])))
    
    # events: a dotted line each; notes only where they don't crowd the last one
    ev <- if(is.null(events)) data.frame(year = numeric(), label = character(), short = character())
          else events[events$year > x_range[1] & events$year < x_range[2], ]
    ev_shapes <- lapply(ev$year, function(x) list(
      type = "line", xref = "x", x0 = x, x1 = x, yref = "paper", y0 = 0, y1 = 1, layer = "below",
      line = list(color = ink$axis, width = 1, dash = "dot")))
    gap <- diff(x_range) * if(narrow) 0.18 else 0.055; last <- -Inf; ev_notes <- list()
    for(i in seq_len(nrow(ev))) if(ev$year[i] - last >= gap){
      last <- ev$year[i]
      ev_notes <- c(ev_notes, list(list(
        x = ev$year[i], xref = "x", y = 1, yref = "paper", yanchor = "bottom", yshift = 2,
        text = if(narrow) ev$short[i] else ev$label[i], showarrow = FALSE,
        font = list(size = 10.5, color = ink$muted))))
    }
    
    p %>% 
      fig_layout(
        font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                    color = ink$primary),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,                   # the stat cards above are the legend
        xaxis = c(list(title = "",
                     automargin = TRUE,
                     range = x_range,
                     tickformat = "d",
                     tickangle = 0,
                     tickfont = tick_font,
                     showgrid = FALSE, zeroline = FALSE,
                     showline = TRUE, linecolor = ink$axis, linewidth = 1,
                     ticks = "outside", ticklen = 4, tickcolor = ink$axis, tickwidth = 1,
                     showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
                     spikethickness = 1, spikecolor = ink$axis, spikedash = "solid",
                     fixedrange = TRUE),
                   # short ranges: one tick per year; otherwise plotly picks spacing from the width
                   if(diff(x_range) <= 10) list(dtick = 1)),
        yaxis = y_axis,
        shapes = c(
          if(!is.null(band)) list(
            list(type = "rect", xref = "paper", x0 = 0, x1 = 1, layer = "below",
                 y0 = band$lo, y1 = band$hi, line = list(width = 0),
                 fillcolor = band$colour, opacity = ink$band),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, layer = "below",
                 y0 = band$med, y1 = band$med,
                 line = list(color = band$colour, width = 1), opacity = 0.45)),
          if(!is.null(hline)) list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1, layer = "below",
            y0 = hline, y1 = hline, line = list(color = ink$zero, width = 1))),
          ev_shapes),
        annotations = c(labels, ev_notes,
                        # zero-line note, inside the plot at the left, just above the line
                        if(!is.null(zero_label)) list(list(
                          x = 0, xref = "paper", xanchor = "left", xshift = 4, y = 0,
                          yanchor = "bottom", yshift = 2, showarrow = FALSE, text = zero_label,
                          font = list(size = 11, color = ink$muted)))),
        margin = list(t = if(nrow(ev)) 24 else 8, r = 84, b = 30, l = 52),  # l/r fixed so the slider lines up below
        bargap = 0.35,
        hovermode = "x unified",
        hoverdistance = -1,
        hoverlabel = list(bgcolor = ink$hover_bg, bordercolor = ink$hover_border,
                          font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                                      size = 12, color = ink$primary)))
  }


#######################################################
# stacked rows (small multiples): one row per series, shared timeline and
# shared y scale. Annual: columns + the compound rate. Rolling: lines + each
# series' own typical-range band and median.
#######################################################
stacked_plot <- function(df, y_col, x_range,
                         type = c("bar", "line", "area"),
                         ref = NULL,          # named by label: list(y, text) reference line per row
                         bands = NULL,        # named by label: list(lo, hi, med)
                         zero_label = NULL,   # right-margin note on each row's 0 line
                         y_format = ".1%", tick_format = ".0%",
                         theme = "light"){
  type <- match.arg(type)
  ink <- inks[[theme]]; pal <- series_colours(theme)
  df <- df %>% inner_join(series, by = "market")
  df$y <- df[[y_col]]
  df <- df %>% filter(!is.na(y))
  labs <- series$label[series$label %in% df$label]
  n <- length(labs)
  
  # one shared scale: data plus any bands, padded
  y_all <- c(df$y, unlist(lapply(bands, function(b) c(b$lo, b$hi))), unlist(lapply(ref, `[[`, "y")), 0)
  y_rng <- range(y_all, na.rm = TRUE); pad <- diff(y_rng) * 0.08
  y_rng <- y_rng + c(-pad, pad)
  tick_font <- list(size = 11.5, color = ink$muted)
  
  # one figure, one shared x axis and one y axis per row (top row = y).
  # (plotly R's subplot() built every row as its own chart first.)
  ax <- function(i) if(i == 1) "" else i
  p <- list(data = list())
  for(i in seq_len(n)){
    l <- labs[i]; d <- df[df$label == l, ]; yax <- paste0("y", ax(i))
    if(type == "bar"){
      p <- p %>% fig_trace(d$year, d$y, type = "bar", name = l, yaxis = yax,
                           marker = list(color = pal[[l]], line = list(width = 0)),
                           hovertemplate = paste0(l, ": %{y:", y_format, "}<extra></extra>"))
    } else {
      last <- d[which.max(d$year), ]
      p <- p %>%
        fig_trace(d$year, d$y, type = "scatter", mode = "lines", name = l, yaxis = yax,
                  fill = if(type == "area") "tozeroy" else "none",
                  fillcolor = if(type == "area") fade(pal[[l]], if(theme == "dark") 0.28 else 0.18),
                  line = list(color = pal[[l]], width = if(l == series$label[1]) 2.25 else 1.8),
                  hovertemplate = paste0(l, ": %{y:", y_format, "}<extra></extra>")) %>%
        fig_trace(last$year, last$y, type = "scatter", mode = "markers", yaxis = yax,
                  marker = list(color = pal[[l]], size = 7, line = list(color = ink$page, width = 2)),
                  cliponaxis = FALSE, hoverinfo = "skip")
    }
  }
  gap <- 0.07                                   # between rows, room for each row's name
  h <- (1 - gap * (n - 1)) / n
  shapes <- list(); notes <- list()
  for(i in seq_len(n)){
    l <- labs[i]; yref <- paste0("y", ax(i))
    hline <- function(y, colour, opacity = 1) list(
      type = "line", xref = "paper", x0 = 0, x1 = 1, yref = yref, y0 = y, y1 = y,
      line = list(color = colour, width = 1), opacity = opacity,
      layer = if(type == "bar") "above" else "below")
    if(!is.null(bands[[l]])){
      b <- bands[[l]]
      shapes <- c(shapes, list(
        list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = yref, y0 = b$lo, y1 = b$hi,
             line = list(width = 0), fillcolor = pal[[l]], opacity = ink$band, layer = "below"),
        hline(b$med, pal[[l]], 0.5)))
      notes <- c(notes, list(list(
        xref = "paper", x = 1, xanchor = "left", xshift = 10, yref = yref, y = b$med,
        showarrow = FALSE, text = sprintf("median %s", sprintf("%.1f%%", 100 * b$med)),
        font = list(size = 11, color = ink$muted))))
    }
    shapes <- c(shapes, list(hline(0, ink$zero)))
    if(!is.null(zero_label)) notes <- c(notes, list(list(
      xref = "paper", x = 1, xanchor = "left", xshift = 10, yref = yref, y = 0,
      showarrow = FALSE, text = zero_label, font = list(size = 11, color = ink$muted))))
    if(!is.null(ref[[l]])){
      shapes <- c(shapes, list(hline(ref[[l]]$y, ink$primary, 0.55)))
      notes <- c(notes, list(list(
        xref = "paper", x = 1, xanchor = "left", xshift = 10, yref = yref, y = ref[[l]]$y,
        showarrow = FALSE, text = ref[[l]]$text, font = list(size = 11, color = ink$muted))))
    }
    notes <- c(notes, list(list(
      xref = "paper", x = 0, xanchor = "left", yref = paste0(yref, " domain"), y = 1,
      yanchor = "bottom", showarrow = FALSE, align = "left",
      text = sprintf("<span style='color:%s'>━</span>  %s", pal[[l]], l),
      font = list(size = 12, color = ink$secondary))))
  }
  
  y_axis <- list(title = "", range = y_rng, tickformat = tick_format, tickfont = tick_font,
                 gridcolor = ink$grid, zeroline = FALSE, showline = FALSE,
                 ticks = "", nticks = 4, automargin = FALSE, fixedrange = TRUE)
  x_axis <- list(title = "", automargin = TRUE, range = if(type == "bar") x_range + c(-0.6, 0.6) else x_range,
                 tickformat = "d", tickangle = 0, tickfont = tick_font,
                 showgrid = FALSE, zeroline = FALSE, showline = FALSE, ticks = "",
                 showspikes = type != "bar", spikemode = "across", spikesnap = "cursor",
                 spikethickness = 1, spikecolor = ink$axis, spikedash = "solid",
                 fixedrange = TRUE)
  lay <- list(p,
              xaxis = c(x_axis, list(anchor = paste0("y", ax(n))),
                        if(diff(x_range) <= 10) list(dtick = 1)),
              shapes = Filter(Negate(is.null), shapes),
              font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                          color = ink$primary),
              paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
              showlegend = FALSE, bargap = 0.35, annotations = notes,
              margin = list(t = 22, r = 84, b = 30, l = 52),
              hovermode = "x",
              hoverlabel = list(bgcolor = ink$hover_bg, bordercolor = ink$hover_border,
                                font = list(size = 12, color = ink$primary)))
  for(i in seq_len(n)){
    top <- 1 - (i - 1) * (h + gap)
    lay[[paste0("yaxis", ax(i))]] <- c(y_axis, list(domain = c(top - h, top), anchor = "x"))
  }
  do.call(fig_layout, lay)
}

# switch a finished figure to a date x axis (so hovers read "Jun 1932")
date_axis <- function(fig){
  to_date <- function(x){
    p <- month_parts(unclass(x)); sprintf("%d-%02d-15", p$year, p$month)
  }
  is_x <- function(ref) !is.null(ref) && grepl("^x[0-9]*$", ref)
  fig$data <- lapply(fig$data, function(t){ if(!is.null(t$x)) t$x <- I(to_date(t$x)); t })
  for(n in grep("^xaxis", names(fig$layout), value = TRUE)){
    ax <- fig$layout[[n]]
    ax$type <- "date"; ax$range <- to_date(ax$range); ax$hoverformat <- "%b %Y"
    ax$tickformat <- NULL; ax$dtick <- NULL
    fig$layout[[n]] <- ax
  }
  fig$layout$annotations <- lapply(fig$layout$annotations, function(a){
    if(is_x(a$xref)) a$x <- to_date(a$x); a })
  fig$layout$shapes <- lapply(fig$layout$shapes, function(sh){
    if(is_x(sh$xref)){ sh$x0 <- to_date(sh$x0); sh$x1 <- to_date(sh$x1) }; sh })
  fig
}

# the calculator's chart: shares alone, plus the dividends paid out (shaded),
# plus what reinvesting them added on top (shaded)
dividend_plot <- function(path, x_range, narrow = FALSE, theme = "light"){
  ink <- inks[[theme]]; pal <- series_colours(theme)
  c_div <- pal[["Shares + dividends"]]; c_px <- pal[["Shares (price only)"]]
  a <- if(theme == "dark") 0.26 else 0.16
  taken <- path$shares + path$cash
  tick_font <- list(size = 11.5, color = ink$muted)
  hov <- function(lab) paste0(lab, ": %{y:$,.0f}<extra></extra>")
  p <- list(data = list()) %>%
    fig_trace(path$year, path$shares, type = "scatter", mode = "lines", name = "Shares alone",
              line = list(color = c_px, width = 1.4), hovertemplate = hov("Shares alone")) %>%
    fig_trace(path$year, taken, type = "scatter", mode = "lines", name = "Dividends taken as cash",
              fill = "tonexty", fillcolor = fade(c_px, a),
              line = list(color = c_px, width = 1.8, dash = "dash"),
              hovertemplate = hov("Shares + dividends taken as cash")) %>%
    fig_trace(path$year, path$reinvested, type = "scatter", mode = "lines", name = "Dividends reinvested",
              fill = "tonexty", fillcolor = fade(c_div, a),
              line = list(color = c_div, width = 2.25), hovertemplate = hov("Dividends reinvested"))
  n <- nrow(path); top <- max(path$reinvested)
  # the axis starts at the amount invested (marked by a line), or lower if
  # the value ever fell below it, so the chart's height shows what changed
  base <- path$reinvested[1]; low <- min(path$shares, base)
  span <- max(top - low, base * 0.01)
  p <- p %>% fig_trace(rep(path$year[n], 3), c(path$shares[n], taken[n], path$reinvested[n]),
                       type = "scatter", mode = "markers", hoverinfo = "skip",
                       marker = list(color = c(c_px, c_px, c_div), size = 7,
                                     line = list(color = ink$page, width = 2)), cliponaxis = FALSE)
  # names at the right, spread apart
  ys <- spread_labels(c(path$shares[n], taken[n], path$reinvested[n]), span * 0.06)
  labels <- lapply(1:3, function(i) list(
    x = path$year[n], xref = "x", y = ys[i], xanchor = "left", xshift = 10, showarrow = FALSE,
    text = c("Shares", "Cash taken", "Reinvested")[i], font = list(size = 12, color = ink$secondary)))
  # name each shaded area inside it, where it is tall enough to hold the words
  # (centred where the band stays widest across the width of the words,
  # about an eighth of the years, between 45% and 85% of the way along)
  area_note <- function(lo, hi, text){
    w <- max(1, ceiling(n * 0.07))
    cand <- seq(max(1 + w, ceiling(n * 0.45)), min(n - w, floor(n * 0.85)))
    if(n < 8 || !length(cand) || cand[1] > cand[length(cand)]) return(NULL)
    room <- sapply(cand, function(j){ i <- (j - w):(j + w); min(hi[i]) - max(lo[i]) })
    k <- cand[which.max(room)]
    mid <- (max(lo[(k - w):(k + w)]) + min(hi[(k - w):(k + w)])) / 2
    if(max(room) > span * 0.045) list(list(
    x = path$year[k], xref = "x", y = mid, xanchor = "center", showarrow = FALSE,
    text = text, font = list(size = 11.5, color = ink$secondary)))
  }
  notes <- if(!narrow) c(area_note(taken, path$reinvested, "Extra from reinvesting"),
             area_note(path$shares, taken, "Dividends paid out"))
  p %>% fig_layout(
    font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif", color = ink$primary),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE,
    xaxis = c(list(title = "", automargin = TRUE, range = x_range, tickformat = "d", tickangle = 0,
                   tickfont = tick_font, showgrid = FALSE, zeroline = FALSE,
                   showline = TRUE, linecolor = ink$axis, linewidth = 1,
                   ticks = "outside", ticklen = 4, tickcolor = ink$axis, tickwidth = 1,
                   showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
                   spikethickness = 1, spikecolor = ink$axis, spikedash = "solid", fixedrange = TRUE),
              if(diff(x_range) <= 10) list(dtick = 1)),
    yaxis = c(list(title = "", tickfont = tick_font,
                 range = c(low - span * 0.08, top + span * 0.04), gridcolor = ink$grid, gridwidth = 1,
                 zeroline = FALSE, showline = FALSE, ticks = "", nticks = 7,
                 automargin = FALSE, fixedrange = TRUE),
               dollar_ticks(low - span * 0.08, top + span * 0.04)),
    shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = base, y1 = base, layer = "below",
                       line = list(color = ink$zero, width = 1))),
    # its label goes under the line at the right end (the lines all start at
    # the left), unless the shares finish just under the line
    annotations = c(labels, notes, list(modifyList(
      list(xref = "paper", y = base, yanchor = "top", yshift = -2, showarrow = FALSE,
           text = paste(fmt_amount(base), "invested"), font = list(size = 11, color = ink$muted)),
      if(path$shares[n] < base && path$shares[n] > base - span * 0.12)
        list(x = 0, xanchor = "left", xshift = 4) else list(x = 1, xanchor = "right", xshift = -4)))),
    margin = list(t = 8, r = 84, b = 30, l = 52),
    hovermode = "x unified", hoverdistance = -1,
    hoverlabel = list(bgcolor = ink$hover_bg, bordercolor = ink$hover_border,
                      font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                                  size = 12, color = ink$primary)))
}

# now: today's CAPE, list(cape, year, month, when)
value_plot <- function(d, fifths, now, h = value_horizon, fit = NULL, narrow = FALSE, theme = "light"){
  ink <- inks[[theme]]; col <- series_colours(theme)[[series$label[1]]]
  accent <- series_colours(theme)[[series$label[2]]]          # today, and the starts most like it
  pts <- d[!is.na(d$fwd), ]
  near <- pts[pts$cape >= 0.8 * now$cape, ]                     # within 20% of today's CAPE
  hov <- "%{customdata}<br>CAPE at the start: %{x:.1f}<br>Return: %{y:.1%} a year<extra></extra>"
  tick_font <- list(size = 11.5, color = ink$muted)
  p <- list(data = list())
  if(!is.null(fit)) p <- p %>%
    fig_trace(c(fit$grid, rev(fit$grid)), c(fit$lo, rev(fit$hi)), type = "scatter", mode = "lines",
              fill = "toself", fillcolor = fade(ink$primary, if(theme == "dark") .10 else .06),
              line = list(width = 0), hoverinfo = "skip") %>%
    fig_trace(fit$grid, fit$yhat, type = "scatter", mode = "lines",
              line = list(color = ink$secondary, width = 1.5, dash = "dash"),
              hovertemplate = "Trend line: %{y:.1%} a year<br>at CAPE %{x:.1f}<extra></extra>")
  p <- p %>%
    fig_trace(pts$cape, pts$fwd, type = "scatter", mode = "markers",
              customdata = I(sprintf("End of %d to end of %d", pts$year, pts$year + h)),
              marker = list(color = fade(col, .5), size = 8, line = list(color = col, width = 1)),
              hovertemplate = hov)
  if(nrow(near)) p <- p %>%
    fig_trace(near$cape, near$fwd, type = "scatter", mode = "markers",
              customdata = I(sprintf("End of %d to end of %d", near$year, near$year + h)),
              marker = list(color = accent, size = 10, line = list(color = ink$page, width = 1.5)),
              hovertemplate = hov)
  for(i in seq_len(nrow(fifths))) p <- p %>%
    fig_trace(c(fifths$lo[i], fifths$hi[i]), rep(fifths$med[i], 2), type = "scatter", mode = "lines",
              line = list(color = ink$primary, width = 2.5), hoverinfo = "skip")
  ys <- range(c(pts$fwd, 0)); pad <- diff(ys) * 0.08
  notes <- c(
    lapply(seq_len(nrow(fifths)), function(i) list(
      x = (fifths$lo[i] + fifths$hi[i]) / 2, y = fifths$med[i], yshift = 11, showarrow = FALSE,
      text = sprintf("%.1f%%", 100 * fifths$med[i]), font = list(size = 11.5, color = ink$primary))),
    # on a phone the label is shorter and sits against the right edge, so it fits
    list(list(x = if(narrow) 1 else now$cape, xref = if(narrow) "paper" else "x", y = 1, yref = "paper",
              yanchor = "bottom", xanchor = "right", xshift = if(narrow) 0 else -6,
              showarrow = FALSE, align = "right",
              text = paste0(sprintf("<b>Today: CAPE %.1f</b> (%s)", now$cape,
                                    if(narrow) paste(month.abb[now$month], now$year) else now$when),
                            if(nrow(near)) sprintf("<br>\u25cf Closest past starts%s: %s to %s a year",
                                                   if(narrow) "" else paste0(" (", year_runs(near$year), ")"),
                                                   fmt_pct(min(near$fwd), 1), fmt_pct(max(near$fwd), 1)),
                            sprintf("<br><span style='color:%s'>%s: its %d years end in %s</span>", ink$muted,
                                    if(narrow) "No dot yet" else "No dot for today yet", h,
                                    if(narrow) now$year + h else paste(month.name[now$month], now$year + h))),
              font = list(size = 12, color = accent))),
    if(!is.null(fit)) list(list(x = now$cape - 1, y = sum(fit$b * c(1, log(now$cape - 1))), xanchor = "right", yanchor = "top",
              yshift = -6, showarrow = FALSE, text = sprintf("Trend line, R\u00b2 %.2f", fit$r2),
              font = list(size = 11.5, color = ink$secondary))))
  p %>% fig_layout(
    font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif", color = ink$primary),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE,
    hovermode = "closest",
    margin = list(t = 62, r = 16, b = 52, l = 52),
    xaxis = list(title = list(text = "CAPE at the start (price \u00f7 10-year average earnings)", font = list(size = 12, color = ink$muted)),
                 range = c(0, max(d$cape) * 1.04), tickfont = tick_font, showgrid = FALSE, zeroline = FALSE,
                 showline = TRUE, linecolor = ink$axis, ticks = "outside", ticklen = 4, tickcolor = ink$axis,
                 fixedrange = TRUE),
    yaxis = list(title = list(text = sprintf("Average yearly return, next %d years", h), font = list(size = 12, color = ink$muted)),
                 range = c(ys[1] - pad, ys[2] + pad), tickformat = ".0%", tickfont = tick_font,
                 gridcolor = ink$grid, zeroline = FALSE, showline = FALSE, ticks = "", fixedrange = TRUE),
    shapes = list(
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, layer = "below", line = list(color = ink$zero, width = 1)),
      list(type = "line", x0 = now$cape, x1 = now$cape, yref = "paper", y0 = 0, y1 = 1,
           line = list(color = accent, width = 2))),
    annotations = notes)
}
