library(shiny)
library(dplyr)
library(tidyr)
library(bslib)

options(warn=-1)
options(scipen = 999)

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#0b0b0b",
  primary = "#1f1f1d",      # controls stay neutral ink; colour is reserved for the data
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.5rem",
  "font-size-base" = "1rem",
  "link-color" = "#52514e"
)


series <- tibble::tribble(
  ~key,    ~market,             ~label,                 ~short,          ~colour,   ~colour_dark,
  "div",   "Stock Price + Div", "Shares + dividends",   "Shares + div",  "#2c5b9a", "#5b8ed4",
  "price", "Stock Price",       "Shares (price only)",  "Shares",        "#c4692f", "#cf7a42",
  "bonds", "Bonds 10yr",        "10-year govt bonds",   "Bonds",         "#2a8f73", "#35a384"
)
# palettes validated with dataviz validate_palette.js, all pairs: light on #fdfdfb and
# dark on #161615 both pass every check (dark steps are their own, not an inversion).
# Lines also carry direct end labels, so identity never relies on colour alone.

# chart chrome per theme (page colours mirror the CSS variables)
inks <- list(
  light = list(primary = "#111110", secondary = "#4f4e4a", muted = "#8a8882",
               grid = "#eeede8", axis = "#a9a79f", zero = "#8a8882", page = "#fcfcfa",
               hover_bg = "#ffffff", hover_border = "#d6d4cc", band = 0.08),
  dark  = list(primary = "#ecebe7", secondary = "#bdbcb5", muted = "#86847e",
               grid = "#252523", axis = "#4a4945", zero = "#6e6d67", page = "#141413",
               hover_bg = "#1d1d1c", hover_border = "#3a3a37", band = 0.13))
series_colours <- function(theme) setNames(if(theme == "dark") series$colour_dark else series$colour, series$label)

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
  div(id = outputId, class = "figure-output", style = paste0("width: 100%; height: ", height, ";"))
}
renderFigure <- function(expr){
  func <- shiny::quoToFunction(rlang::enquo0(expr), "renderFigure")
  shiny::createRenderFunction(func, function(value, session, name, ...) drop_nulls(value), figureOutput)
}

#######################################################
# dollar axis ticks: $100, $1k, $2.5M, $12B (plotly's own formats give "$1000"
# next to "$10k", or "G" for billions), with enough of them on short ranges
fmt_tick <- function(v) vapply(v, function(x){
  k <- findInterval(abs(x), c(1e3, 1e6, 1e9))             # 0: units, 1: k, 2: M, 3: B
  paste0("$", trimws(formatC(x / 1000^k, format = "fg", digits = 3)), c("", "k", "M", "B")[k + 1])
}, "")
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
  y_all <- c(df$y, unlist(lapply(bands, function(b) c(b$lo, b$hi))), 0)
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

# compound yearly rate over a range (years after the first), as on the cards
compound_rate <- function(r) if(length(r)) exp(mean(log1p(r))) - 1 else NA

# small flags for the market switch, drawn as inline SVG (flag emoji show as
# plain letters on Windows)
star_points <- function(cx, cy, r_out, r_in, n){
  a <- -pi / 2 + seq(0, by = pi / n, length.out = 2 * n)
  r <- rep(c(r_out, r_in), n)
  paste(sprintf("%.2f,%.2f", cx + r * cos(a), cy + r * sin(a)), collapse = " ")
}
flag_svg <- list(
  # Australia, 2:1: Union Jack, Commonwealth Star, Southern Cross
  au = paste0(
    '<svg class="cflag" viewBox="0 0 120 60" width="22" height="11" aria-hidden="true">',
    '<defs><clipPath id="cflag-au-c"><rect width="60" height="30"/></clipPath>',
    '<clipPath id="cflag-au-t"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath></defs>',
    '<rect width="120" height="60" fill="#012169"/>',
    '<g clip-path="url(#cflag-au-c)">',
    '<path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/>',
    '<path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#cflag-au-t)" stroke="#C8102E" stroke-width="4"/>',
    '<path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/>',
    '<path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g>',
    '<g fill="#fff">',
    '<polygon points="', star_points(30, 45, 9, 4.2, 7), '"/>',
    '<polygon points="', star_points(90, 50, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(90, 10, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(76, 24, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(103, 21, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(97, 32, 2.6, 1.2, 5), '"/></g></svg>'),
  # United States, 19:10: 13 stripes and a canton (stars as dots at this size)
  us = paste0(
    '<svg class="cflag" viewBox="0 0 190 100" width="21" height="11" aria-hidden="true">',
    '<rect width="190" height="100" fill="#fff"/>',
    paste0(sprintf('<rect y="%.3f" width="190" height="%.3f" fill="#B22234"/>',
                   (seq(0, 12, by = 2)) * 100 / 13, 100 / 13), collapse = ""),
    '<rect width="76" height="53.85" fill="#3C3B6E"/>',
    '<g fill="#fff">',
    paste0(apply(expand.grid(x = seq(7, 69, length.out = 6), y = seq(6, 48, length.out = 5)), 1,
                 function(p) sprintf('<circle cx="%.1f" cy="%.1f" r="2.8"/>', p[1], p[2])), collapse = ""),
    '</g></svg>'))

# Australia / United States (a shiny radio input); the page title follows it
country_switch <- function(id){
  tags$div(id = id, class = "shiny-input-radiogroup country-switch", role = "radiogroup",
           `aria-label` = "Market",
           lapply(names(countries), function(k){
             bid <- paste0(id, "_", k)
             tagList(tags$input(type = "radio", class = "btn-check", name = id, id = bid, value = k,
                                autocomplete = "off", checked = if(k == "au") NA),
                     tags$label(`for` = bid, HTML(flag_svg[[k]]), countries[[k]]))
           }),
           tags$script(HTML(sprintf("$(document).on('change', '#%s input', function(){ document.getElementById('app-title').textContent = this.value === 'us' ? 'US Stock Market' : 'Australian Stock Market'; });", id))))
}

# small icon switch for overlay / stacked (a shiny radio input)
layout_switch <- function(id, selected){
  icon_overlay <- HTML('<svg width="18" height="14" viewBox="0 0 18 14" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"><path d="M1 10 L6 5 L10 8 L17 2"/><path d="M1 12 L6 9 L11 11 L17 7"/></svg>')
  icon_stacked <- HTML('<svg width="18" height="14" viewBox="0 0 18 14" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><path d="M1 2.5 H17"/><path d="M1 7 H17"/><path d="M1 11.5 H17"/></svg>')
  opts <- list(list("overlay", "Overlay: all series on one chart", icon_overlay),
               list("stacked", "Stacked: one row per series", icon_stacked))
  tags$div(id = id, class = "shiny-input-radiogroup layout-switch", role = "radiogroup",
           `aria-label` = "Chart layout",
           lapply(opts, function(o){
             bid <- paste0(id, "_", o[[1]])
             tagList(tags$input(type = "radio", class = "btn-check", name = id, id = bid,
                                value = o[[1]], autocomplete = "off",
                                checked = if(o[[1]] == selected) NA),
                     tags$label(`for` = bid, title = o[[2]], `aria-label` = o[[2]], o[[3]]))
           }))
}

#######################################################
# falls from the peak
#######################################################
# "−45%" with a real minus sign
fmt_pct <- function(x, digits = 0) sub("^-", "−", sprintf(paste0("%.", digits, "f%%"), 100 * x))

# translucent version of a hex colour, for area fills
fade <- function(hex, a){ v <- col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }

# worst fall from a previous high: when it peaked, bottomed and recovered
fall_stats <- function(year, dollars){
  dd <- dollars / cummax(dollars) - 1
  # a dip that rounds to 0% counts as no fall
  if(!length(dd) || min(dd) > -0.005) return(list(worst = 0, now = dd[length(dd)]))
  i <- which.min(dd)
  peak_i <- which.max(dollars[seq_len(i)])
  back <- which(dollars >= dollars[peak_i] & seq_along(dollars) > i)[1]
  list(worst = dd[i], peak = year[peak_i], low = year[i],
       back = if(is.na(back)) NA else year[back], now = dd[length(dd)])
}

# Monthly values (US falls) sit on the same scale as the yearly ones: December
# of a year is at that year, month m at year - 1 + m/12.
month_parts <- function(x){
  k <- round(x * 12); m <- (k - 1) %% 12 + 1
  list(year = as.integer((k - m) / 12 + 1), month = as.integer(m))
}
fmt_when <- function(x, monthly){
  if(!monthly) return(as.character(x))
  p <- month_parts(x); paste(month.abb[p$month], p$year)
}
fmt_span <- function(d, monthly){
  if(!monthly) return(sprintf("%d years", d))
  mo <- round(d * 12)
  if(mo < 24) sprintf("%d months", mo) else sprintf("%d years", round(mo / 12))
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

# market events marked on the growth chart
events <- tibble::tribble(
  ~country, ~year, ~label,             ~short,
  "au",     1893,  "1890s bank crash", "Bank crash",
  "au",     1930,  "Depression",       "Depression",
  "au",     1974,  "1974 crash",       "1974",
  "au",     1987,  "1987 crash",       "1987",
  "au",     2008,  "GFC",              "GFC",
  "au",     2020,  "COVID",            "COVID",
  "us",     1907,  "1907 panic",       "1907",
  "us",     1930,  "Depression",       "Depression",
  "us",     1974,  "1974 crash",       "1974",
  "us",     2000,  "Dot-com bust",     "Dot-com",
  "us",     2008,  "GFC",              "GFC",
  "us",     2020,  "COVID",            "COVID")

# exact amount someone typed: $10,000, $2,500.50
fmt_amount <- function(x) paste0("$", formatC(x, format = "f", digits = if(x %% 1 == 0) 0 else 2, big.mark = ","))

# $12.4M, $148k, $1,234
fmt_dollars <- function(x){
  if(is.na(x)) return("–")
  if(x >= 1e9) return(sprintf("$%.1fB", x / 1e9))
  if(x >= 1e6) return(sprintf("$%.1fM", x / 1e6))
  if(x >= 1e4) return(sprintf("$%.0fk", x / 1e3))
  paste0("$", formatC(round(x), format = "d", big.mark = ","))
}

#######################################################
# dividend calculator
#######################################################
# `amount` put into shares at the end of y0, followed to the end of each later
# year two ways:
#   reinvested: every dividend buys more shares (the total-return series)
#   shares:     the same shares with dividends paid out (the price series)
#   cash:       the dividends paid out, added up (not invested, no interest)
# A year's dividend is the gap between the two returns applied to the value
# of the shares at the start of that year. With `real`, every amount is in
# y0 dollars: each dividend is deflated by prices at the end of its year.
dividend_path <- function(raw, y0, y1, amount, real = FALSE){
  d <- raw[raw$year > y0 & raw$year <= y1, ]; d <- d[order(d$year), ]
  defl <- if(real) cumprod(1 + d$inflation) else rep(1, nrow(d))
  price <- amount * cumprod(1 + d$stock_price)                 # before inflation
  paid  <- c(amount, head(price, -1)) * (d$stock_accumulation - d$stock_price)
  data.frame(year       = c(y0, d$year),
             reinvested = c(amount, amount * cumprod(1 + d$stock_accumulation) / defl),
             shares     = c(amount, price / defl),
             cash       = c(0, cumsum(paid / defl)))
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

######################################################################
#create final data frames
######################################################################
# one country's annual returns (year, stock_accumulation, stock_price, bonds,
# inflation) -> nominal and real returns and $100 indices in long format.
# Index: $100 the year before the first return, compounded by each year's return.
# Real return: (1 + r) / (1 + inflation) - 1.
prep_market <- function(raw, max_year = Inf){
  cols <- c("bonds", "inflation", "stock_accumulation", "stock_price")
  raw <- raw[raw$year <= max_year, c("year", cols)]
  raw <- raw[order(raw$year), ]
  start <- min(raw$year) - 1                 # the index starts at $100 the year before the first return
  years <- c(start, raw$year)
  nominal <- lapply(setNames(cols, cols), function(k) c(NA, raw[[k]]))
  real <- lapply(nominal[cols != "inflation"], function(r) (1 + r) / (1 + nominal$inflation) - 1)
  index <- function(r) c(100, 100 * cumprod(1 + r[-1]))
  # long format, rows by year then series (the column order of the source data)
  long <- function(vals, src_order, names, value){
    k <- length(src_order)
    out <- tibble::tibble(year = rep(years, each = k),
                          market = factor(rep(unname(names[src_order]), times = length(years)), levels = unname(names[sort(src_order)])),
                          v = as.vector(t(sapply(vals[src_order], identity))))
    names(out)[3] <- value; out
  }
  nm  <- c(bonds = "Bonds 10yr", inflation = "inflation", stock_accumulation = "Stock Price + Div", stock_price = "Stock Price")
  nmi <- c(bonds = "Bonds 10yr", inflation = "inflation_indx", stock_accumulation = "Stock Price + Div", stock_price = "Stock Price")
  src  <- c("stock_accumulation", "stock_price", "bonds", "inflation")   # column order in the source data
  srcr <- c("stock_accumulation", "stock_price", "bonds")
  list(returns_nom  = long(nominal, src, nm, "percent_yoy"),
       index_nom    = long(lapply(nominal, index), src, nmi, "dollars"),
       returns_real = long(real, srcr, nm, "percent_yoy"),
       index_real   = long(lapply(real, index), srcr, nm, "dollars"))
}

au_raw <- readRDS("data/australianstockmarket.RDS")
us_raw <- readRDS("data/usstockmarket.RDS")    # built by code/us_data.R
max_yr <- min(max(au_raw$year), max(us_raw$year))   # both countries cover the same years
min_yr <- min(au_raw$year) - 1
markets <- list(au = prep_market(au_raw, max_yr),
                us = prep_market(us_raw, max_yr))
raws <- list(au = au_raw[au_raw$year <= max_yr, ], us = us_raw[us_raw$year <= max_yr, ])
countries <- c(au = "Australia", us = "United States")

# US monthly indices for the falls tab (built by code/us_monthly.R), to the
# same last year as the annual data
us_monthly <- readRDS("data/usmonthly.RDS")
us_monthly <- us_monthly[us_monthly$year <= max_yr, ]
monthly_index <- function(m, real){
  x <- m$year + (m$month - 12) / 12
  defl <- if(real) m$cpi / 100 else 1
  lv <- c("Bonds 10yr", "Stock Price + Div", "Stock Price")
  tibble::tibble(year = rep(x, 3),
                 market = factor(rep(c("Stock Price + Div", "Stock Price", "Bonds 10yr"), each = length(x)), levels = lv),
                 dollars = c(m$tr, m$price, m$bonds) / defl)
}
us_falls_index <- list(nom = monthly_index(us_monthly, FALSE), real = monthly_index(us_monthly, TRUE))

# S&P 500 bear markets at daily closes (Yardeni Research, from S&P data):
# shown under the US falls chart, since monthly averages smooth the worst days
bears <- read.csv("data/sp500_bear_markets.csv")
bear_table <- local({
  d <- function(x){ x <- as.Date(x); paste(as.integer(format(x, "%d")), format(x, "%b %Y")) }
  len <- function(n) if(n < 60) sprintf("%d days", n) else sprintf("%d months", round(n / 30.44))
  tagList(
    tags$table(class = "method-table bear-table",
      tags$thead(tags$tr(tags$th("Peak"), tags$th("Low"), tags$th(class = "num", "Fall"), tags$th(class = "num", "Length"))),
      tags$tbody(lapply(seq_len(nrow(bears)), function(i) tags$tr(
        tags$td(d(bears$peak_date[i])), tags$td(d(bears$trough_date[i])),
        tags$td(class = "num", fmt_pct(bears$percent_loss[i] / 100, 1)),
        tags$td(class = "num", len(bears$days[i])))))),
    tags$p(class = "bear-notes",
      "Falls of 20% or more in the S&P 500 price index, peak to low at daily closes. ",
      "1929\u201332 counts as several bear markets because the rallies in between rose more than 20%: from the September 1929 peak to the June 1932 low the S&P 500 fell 86%, and the Dow Jones Industrial Average 89%. ",
      "Since this list was published, the S&P 500's largest fall was about 19% (19 February to 8 April 2025), just short of a bear market. ",
      "Source: Yardeni Research, ",
      tags$a(href = "https://old.yardeni.com/wp-content/uploads/BullBearTables.pdf", target = "_blank", rel = "noopener", "Stock Market Historical Tables: Bull & Bear Markets"),
      " (January 2024), from Standard & Poor's data."))
})

#######################################################
# valuation: CAPE each December and the US share return (with dividends)
# over the following 10 years
#######################################################
us_cape <- us_monthly[us_monthly$month == 12 & !is.na(us_monthly$cape), c("year", "cape")]
value_horizon <- 10
value_data <- function(real){
  idx <- (if(real) markets$us$index_real else markets$us$index_nom)
  idx <- idx[idx$market == "Stock Price + Div", ]; idx <- idx[order(idx$year), ]
  h <- value_horizon
  fwd <- c((idx$dollars[-(1:h)] / idx$dollars[1:(nrow(idx) - h)])^(1 / h) - 1, rep(NA, h))
  merge(us_cape, data.frame(year = idx$year, fwd = fwd), by = "year")
}
# fifths of the Decembers that have a full 10 years after them, by CAPE
value_fifths <- function(d){
  d <- d[!is.na(d$fwd), ]
  br <- quantile(d$cape, 0:5 / 5)
  d$q <- cut(d$cape, br, include.lowest = TRUE, labels = FALSE)
  do.call(rbind, lapply(1:5, function(i){
    s <- d[d$q == i, ]
    data.frame(q = i, lo = br[i], hi = br[i + 1], n = nrow(s), med = median(s$fwd),
               worst = min(s$fwd), best = max(s$fwd))
  }))
}
value_plot <- function(d, fifths, theme = "light"){
  ink <- inks[[theme]]; col <- series_colours(theme)[[series$label[1]]]
  pts <- d[!is.na(d$fwd), ]; now <- d[nrow(d), ]
  tick_font <- list(size = 11.5, color = ink$muted)
  p <- list(data = list()) %>%
    fig_trace(pts$cape, pts$fwd, type = "scatter", mode = "markers",
              customdata = I(sprintf("End of %d to end of %d", pts$year, pts$year + value_horizon)),
              marker = list(color = fade(col, .5), size = 8, line = list(color = col, width = 1)),
              hovertemplate = "%{customdata}<br>CAPE at the start: %{x:.1f}<br>Return: %{y:.1%} a year<extra></extra>")
  for(i in seq_len(nrow(fifths))) p <- p %>%
    fig_trace(c(fifths$lo[i], fifths$hi[i]), rep(fifths$med[i], 2), type = "scatter", mode = "lines",
              line = list(color = ink$primary, width = 2.5), hoverinfo = "skip")
  ys <- range(c(pts$fwd, 0)); pad <- diff(ys) * 0.08
  notes <- c(
    lapply(seq_len(nrow(fifths)), function(i) list(
      x = (fifths$lo[i] + fifths$hi[i]) / 2, y = fifths$med[i], yshift = 11, showarrow = FALSE,
      text = sprintf("%.1f%%", 100 * fifths$med[i]), font = list(size = 11.5, color = ink$primary))),
    list(list(x = now$cape, y = 1, yref = "paper", yanchor = "bottom", xanchor = "right", xshift = -4,
              showarrow = FALSE, text = sprintf("Now: %.1f (Dec %d)", now$cape, now$year),
              font = list(size = 11.5, color = ink$secondary))))
  p %>% fig_layout(
    font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif", color = ink$primary),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE,
    hovermode = "closest",
    margin = list(t = 26, r = 16, b = 52, l = 52),
    xaxis = list(title = list(text = "CAPE at the start (price \u00f7 10-year average earnings)", font = list(size = 12, color = ink$muted)),
                 range = c(0, max(d$cape) * 1.04), tickfont = tick_font, showgrid = FALSE, zeroline = FALSE,
                 showline = TRUE, linecolor = ink$axis, ticks = "outside", ticklen = 4, tickcolor = ink$axis,
                 fixedrange = TRUE),
    yaxis = list(title = list(text = "Average yearly return, next 10 years", font = list(size = 12, color = ink$muted)),
                 range = c(ys[1] - pad, ys[2] + pad), tickformat = ".0%", tickfont = tick_font,
                 gridcolor = ink$grid, zeroline = FALSE, showline = FALSE, ticks = "", fixedrange = TRUE),
    shapes = list(
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, layer = "below", line = list(color = ink$zero, width = 1)),
      list(type = "line", x0 = now$cape, x1 = now$cape, yref = "paper", y0 = 0, y1 = 1,
           line = list(color = ink$secondary, width = 1.2, dash = "dot"))),
    annotations = notes)
}






# rebase a precomputed index (from 1882) to $100 at the first year of a range.
# index[t] / index[start] * 100, unrounded (round for display).
rebase_index <- function(df, min_year, max_year){
  df %>% 
    filter(market != "inflation_indx",
           year >= min_year, year <= max_year) %>% 
    group_by(market) %>% 
    mutate(dollars = dollars / dollars[year == min_year] * 100) %>% 
    ungroup()
}

# rolling returns over the full history for one window length: the average
# yearly return over the no_years years ending in each year
rolling_full <- function(mk, real, no_years){
  idx <- if(real) mk$index_real else mk$index_nom
  idx %>%
    filter(market != "inflation_indx") %>%
    group_by(market) %>%
    mutate(percent = round((dollars / lag(dollars, no_years))^(1 / no_years) - 1, 4)) %>%
    ungroup() %>%
    transmute(year, market, roll_years = factor(as.character(no_years)), percent)
}

######################################################################
# sources
######################################################################
sources <- list(au = list(
  list("S&P Dow Jones Indices", "https://www.spglobal.com/spdji/en/indices/equity/all-ordinaries/#overview"),
  list("Reserve Bank of Australia", "https://www.rba.gov.au/statistics/tables"),
  list("Australian Bureau of Statistics", "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"),
  list("Brailsford et al. (2012)", "https://research.bond.edu.au/en/publications/the-historical-equity-risk-premium-in-australia-post-gfc-and-128-")),
  us = list(
  list("Robert J. Shiller, Online Data", "https://shillerdata.com/"),
  list("Aswath Damodaran, NYU Stern", "https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histret.html"),
  list("Yardeni Research (bear markets)", "https://old.yardeni.com/wp-content/uploads/BullBearTables.pdf")))
source_links <- function(x) tagList(lapply(seq_along(x), function(i){
  tagList(if(i > 1) " \u00b7 ",
          tags$a(href = x[[i]][[2]], target = "_blank", rel = "noopener", x[[i]][[1]]))
}))

app_css <- "
:root {
  --ink: #111110; --ink-2: #4f4e4a; --ink-3: #8a8882;
  --line: #e6e4de; --line-2: #d6d4cc; --page: #fcfcfa; --raised: #ffffff;
}
/* dark: its own steps (warm near-black page, off-white ink), not an inversion */
[data-bs-theme=dark] {
  --ink: #ecebe7; --ink-2: #bdbcb5; --ink-3: #86847e;
  --line: #262624; --line-2: #3a3a37; --page: #141413; --raised: #1d1d1c;
  --bs-body-bg: var(--page); --bs-body-color: var(--ink); --bs-border-color: var(--line-2);
  --bs-secondary-bg: var(--raised); --bs-tertiary-bg: var(--raised);
  color-scheme: dark;
}
[data-bs-theme=dark] .modal-content { background: var(--raised); }
[data-bs-theme=dark] .form-select { background-color: var(--raised); color: var(--ink); border-color: var(--line-2); }
.series-card { --c: var(--cl); }
[data-bs-theme=dark] .series-card { --c: var(--cd); }

/* light/dark toggle */
.header-actions { display: flex; align-items: center; gap: 16px; }
bslib-input-dark-mode { --text-1: var(--ink-3); --text-2: var(--ink); --vertical-correction: 0; display: inline-flex; font-size: 1.05rem; }
html, body { background: var(--page); color: var(--ink); -webkit-font-smoothing: antialiased; text-rendering: optimizeLegibility; }
.app { max-width: 1040px; margin: 0 auto; padding: 56px 28px 64px; }

/* header */
.app-header { display: flex; justify-content: space-between; align-items: flex-end; gap: 24px; padding-bottom: 20px; border-bottom: 1px solid var(--ink); margin-bottom: 4px; }
.app-header h1 { font-size: 1.85rem; font-weight: 600; letter-spacing: -0.025em; line-height: 1.15; margin: 0 0 8px; }
.app-header p { color: var(--ink-2); margin: 0; font-size: 1rem; }
.about-link { color: var(--ink-2); font-size: .88rem; text-decoration: none; white-space: nowrap; }
.about-link:hover { color: var(--ink); text-decoration: underline; text-underline-offset: 3px; }

/* view tabs + inflation switch */
.chart-bar { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 8px 24px; border-bottom: 1px solid var(--line); }
.chart-bar-left { display: flex; flex-wrap: wrap; align-items: center; gap: 4px 28px; }
.seg { display: inline-flex; gap: 26px; }
.seg .btn { border: 0; border-radius: 0 !important; padding: 14px 0 12px; margin-bottom: -1px; font-size: .92rem; font-weight: 500; color: var(--ink-3); background: transparent; border-bottom: 2px solid transparent; }
.seg .btn:hover { color: var(--ink); background: transparent; }
.seg .btn-check:checked + .btn { color: var(--ink); border-bottom-color: var(--ink); background: transparent; }
.seg .btn-check:focus-visible + .btn { outline: 2px solid var(--ink-3); outline-offset: 2px; }
.window { display: flex; align-items: center; }
.window .form-group { margin: 0; display: flex; align-items: center; gap: 8px; }
.window label { margin: 0; color: var(--ink-3); font-size: .88rem; font-weight: 400; }
.window select { width: auto; font-size: .88rem; padding: 3px 28px 3px 8px; border-color: var(--line); background-color: transparent; }
.chart-bar > .shiny-input-container { width: auto !important; margin: 0 0 0 auto; }
.chart-bar .bslib-input-switch { margin: 0; color: var(--ink-2); font-size: .88rem; }
.form-switch .form-check-input { cursor: pointer; }
.form-switch .form-check-input:focus { box-shadow: none; }

.country-switch { display: inline-flex; border: 1px solid var(--line-2); border-radius: 6px; overflow: hidden; }
.country-switch label { display: inline-flex; align-items: center; height: 28px; padding: 0 11px; margin: 0; cursor: pointer; color: var(--ink-3); font-size: .82rem; white-space: nowrap; }
.country-switch label + input + label { border-left: 1px solid var(--line-2); }
.country-switch .cflag { flex: none; margin-right: 7px; border-radius: 2px; box-shadow: 0 0 0 1px var(--line-2); opacity: .9; }
.country-switch .btn-check:checked + label .cflag { opacity: 1; }
.country-switch label:hover { color: var(--ink); }
.country-switch .btn-check:checked + label { color: var(--ink); background: var(--line); }
.country-switch .btn-check:focus-visible + label { outline: 2px solid var(--ink-3); outline-offset: -2px; }
.layout-switch { display: inline-flex; border: 1px solid var(--line-2); border-radius: 6px; overflow: hidden; }
.layout-switch label { display: inline-flex; align-items: center; justify-content: center; width: 34px; height: 26px; margin: 0; cursor: pointer; color: var(--ink-3); }
.layout-switch label + input + label { border-left: 1px solid var(--line-2); }
.layout-switch label:hover { color: var(--ink); }
.layout-switch .btn-check:checked + label { color: var(--ink); background: var(--line); }
.layout-switch .btn-check:focus-visible + label { outline: 2px solid var(--ink-3); outline-offset: -2px; }
.chart-title { margin: 28px 0 0; }
.chart-title h2 { font-size: 1.15rem; font-weight: 600; margin: 0; letter-spacing: -0.012em; }
.chart-title p { color: var(--ink-3); font-size: .88rem; margin: 4px 0 0; }

/* series legend: name + headline figures, click to show/hide. No boxes. */
.series { display: grid; grid-template-columns: repeat(3, 1fr); margin: 22px 0 6px; }
.series-card { display: block; margin: 0; padding: 2px 20px 4px; border-left: 1px solid var(--line); cursor: pointer; user-select: none; transition: opacity .15s; }
.series-card:first-of-type { padding-left: 0; border-left: 0; }
.series .btn-check:not(:checked) + .series-card { opacity: .38; }
.series .btn-check:focus-visible + .series-card { outline: 2px solid var(--ink-3); outline-offset: 4px; }
.series-card:hover .name { color: var(--ink); }
.series-card .name { display: flex; align-items: center; gap: 9px; color: var(--ink-2); font-size: .86rem; }
.series-card .dot { width: 16px; height: 3px; border-radius: 2px; background: var(--c); flex: none; }
.series-card .figs { display: flex; align-items: baseline; flex-wrap: wrap; gap: 2px 10px; margin-top: 6px; }
.series-card .value { font-size: 1.6rem; font-weight: 500; letter-spacing: -0.01em; color: var(--ink); line-height: 1.1; }
.series-card .rate { font-size: .84rem; color: var(--ink-2); }
.series-card .lbl-short { display: none; }
.series-card .ctx { color: var(--ink-3); font-size: .78rem; margin-top: 4px; line-height: 1.4; }
.series-card .ctx:empty { display: none; }
.series-card .ctx-line { display: block; margin-top: 2px; }
.series-card .flag { display: inline-block; margin-left: 6px; padding: 0 5px; border: 1px solid var(--line-2); border-radius: 3px; color: var(--ink); font-size: .68rem; font-weight: 600; letter-spacing: .04em; text-transform: uppercase; white-space: nowrap; }
.stats-note { color: var(--ink-3); font-size: .8rem; margin: 10px 0 18px; }
/* dividend calculator */
.calc-q { display: flex; flex-wrap: wrap; align-items: center; gap: 8px 10px; margin: 22px 0 0; font-size: 1.02rem; color: var(--ink-2); }
.calc-q .amount-wrap { position: relative; display: inline-flex; align-items: center; }
.calc-q .cur { position: absolute; left: 10px; color: var(--ink-3); pointer-events: none; }
.calc-q input { width: 130px; padding: 5px 10px 5px 22px; font-size: 1rem; font-weight: 500; color: var(--ink); border-color: var(--line-2); background-color: var(--raised); font-variant-numeric: tabular-nums; }
.calc-q select { width: auto; padding: 5px 32px 5px 10px; font-size: 1rem; font-weight: 500; color: var(--ink); border-color: var(--line-2); background-color: var(--raised); font-variant-numeric: tabular-nums; }
.calc-q input:focus, .calc-q select:focus { border-color: var(--ink-3); box-shadow: 0 0 0 3px var(--line); }
.period { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 12px; margin: 0 0 4px; font-size: .86rem; }
.period-label { color: var(--ink-3); }
.period-range { color: var(--ink); font-weight: 600; font-variant-numeric: tabular-nums; }
.year-presets { display: inline-flex; border: 1px solid var(--line-2); border-radius: 6px; overflow: hidden; }
.year-presets button { border: 0; background: transparent; color: var(--ink-2); padding: 3px 9px; font-variant-numeric: tabular-nums; font-size: .8rem; line-height: 1.5; }
.year-presets button + button { border-left: 1px solid var(--line-2); }
.year-presets button:hover { color: var(--ink); }
.year-presets button.active { color: var(--ink); background: var(--line); font-weight: 500; }
.year-presets button:focus-visible { outline: 2px solid var(--ink-3); outline-offset: -2px; }
.period-hint { color: var(--ink-3); font-size: .78rem; }
.calc .series-card { cursor: default; }
.calc .series-card:hover .name { color: var(--ink-2); }
.calc .series-card.gain .dot { background: linear-gradient(90deg, var(--c) 50%, transparent 50%); background-size: 6px 3px; }

/* year range slider under the chart: hairline track, fine handles */
.years { padding: 0 77px 0 45px; }   /* plot margins (52 / 84) minus half a handle */
.years .form-group { margin: 0; }
.irs--shiny { height: 42px; }
.irs--shiny .irs-line { height: 3px; top: 25px; background: var(--line-2); border: 0; border-radius: 2px; }
.irs--shiny .irs-bar { height: 3px; top: 25px; background: var(--ink-2); border: 0; border-radius: 2px; }
.irs--shiny .irs-handle { width: 18px; height: 18px; top: 17.5px; background: var(--raised); border: 2px solid var(--ink-2); box-shadow: 0 1px 3px rgba(0,0,0,.18); cursor: grab; }
.irs--shiny .irs-handle:active { cursor: grabbing; }
.irs--shiny .irs-handle:hover, .irs--shiny .irs-handle.state_hover { background: var(--page); border-color: var(--ink); }
.irs--shiny .irs-handle > i:first-child { display: none; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: transparent; color: var(--ink-2); font-size: .8rem; font-weight: 500; top: -1px; padding: 0 2px; font-variant-numeric: tabular-nums; }
.irs--shiny .irs-from:before, .irs--shiny .irs-to:before, .irs--shiny .irs-single:before { display: none; }
.irs--shiny .irs-min, .irs--shiny .irs-max { display: none; }

/* footer of the chart */
.chart-foot { display: flex; flex-wrap: wrap; justify-content: space-between; align-items: flex-start; gap: 12px; border-top: 1px solid var(--line); padding-top: 12px; margin-top: 10px; }
.chart-foot #download { margin-left: auto; }
.btn-quiet { color: var(--ink-2); font-size: .86rem; padding: 2px 0; border: 0; background: transparent; }
.btn-quiet:hover, .btn-quiet:focus { color: var(--ink); background: transparent; text-decoration: underline; text-underline-offset: 3px; }
.btn-quiet .fa, .btn-quiet svg { opacity: .7; }

.chart-foot > div:first-child { flex: 1 1 320px; }
details.how summary { color: var(--ink-3); font-size: .86rem; cursor: pointer; user-select: none; list-style: none; }
details.how summary::-webkit-details-marker { display: none; }
details.how summary::before { content: '+'; display: inline-block; width: 1em; color: var(--ink-3); }
details.how[open] summary::before { content: '−'; }
details.how summary:hover { color: var(--ink); }
details.how ul { margin: 10px 0 4px; padding-left: 1em; max-width: 680px; color: var(--ink-2); font-size: .86rem; line-height: 1.6; }
details.how li { margin-bottom: 6px; padding-left: 2px; }
details.how li::marker { color: var(--ink-3); }

.app-footer { color: var(--ink-3); font-size: .8rem; margin-top: 40px; padding-top: 14px; border-top: 1px solid var(--line); line-height: 1.7; }
.app-footer a { color: var(--ink-2); text-decoration: none; }
.app-footer a:hover { color: var(--ink); text-decoration: underline; text-underline-offset: 3px; }

.modal-content { border: 0; border-radius: 6px; }
.modal-header { border-bottom-color: var(--line); }
.modal-title { font-weight: 600; font-size: 1.15rem; letter-spacing: -0.01em; }
.modal-body { color: var(--ink-2); line-height: 1.65; }
.modal-body h3 { font-size: 1.1rem; font-weight: 600; color: var(--ink); margin-top: .5rem; }
.modal-body h4 { font-size: 1rem; font-weight: 600; color: var(--ink); }
.modal-body h5 { font-size: .95rem; font-weight: 600; color: var(--ink); }
.modal-body a { color: var(--ink); }
.modal-body hr { border-color: var(--line); opacity: 1; }
.method-table { width: 100%; font-size: .86rem; margin: 4px 0 14px; font-variant-numeric: tabular-nums; }
.method-table th { font-weight: 600; color: var(--ink); border-bottom: 1px solid var(--line); padding: 4px 10px 4px 0; }
.method-table td { border-bottom: 1px solid var(--line); padding: 4px 10px 4px 0; vertical-align: top; }
details.bears { margin-top: 8px; }
.bear-table { width: auto; min-width: min(100%, 460px); margin-top: 10px; }
.bear-table .num { text-align: right; }
.bear-notes { max-width: 680px; color: var(--ink-3); font-size: .8rem; line-height: 1.55; }
.modal-footer { border-top-color: var(--line); }

@media (max-width: 640px) {
  .app { padding: 28px 16px 40px; }
  .app-header { flex-direction: column; align-items: flex-start; gap: 6px; padding-bottom: 14px; }
  .app-header h1 { font-size: 1.45rem; }
  .header-actions { display: grid; grid-template-columns: auto auto 1fr; gap: 10px 16px; align-items: center; }
  .header-actions .country-switch { grid-column: 1 / -1; justify-self: start; }
  .chart-bar, .chart-bar-left { max-width: 100%; min-width: 0; }
  .chart-bar-left > * { max-width: 100%; min-width: 0; }
  .seg { gap: 18px; max-width: 100%; overflow-x: auto; scrollbar-width: none; }
  .seg::-webkit-scrollbar { display: none; }
  .seg .btn { white-space: nowrap; }
  .seg .btn { font-size: .86rem; padding: 12px 0 10px; }
  .chart-bar > .shiny-input-container { margin: 6px 0 10px; }
  .series-card { padding: 0 10px; }
  .series-card .name { font-size: .76rem; gap: 6px; }
  .series-card .dot { width: 10px; }
  .series-card .lbl-full { display: none; }
  .series-card .lbl-short { display: inline; }
  .series-card .figs { flex-direction: column; gap: 0; margin-top: 4px; }
  .series-card .value { font-size: 1.15rem; }
  .series-card .rate { font-size: .74rem; }
  .series-card .ctx { font-size: .7rem; }
  .series-card .flag { margin: 2px 0 0; }
  .years { padding: 0 8px; }
  #chart { height: 640px !important; }   /* room for stacked rows on phones */
}
"

# quick year ranges ending in the latest year (handled in the browser)
year_presets <- function(){
  div(class = "year-presets", role = "group", `aria-label` = "Quick year ranges",
      lapply(c(1, 3, 5, 10, 20, 50), function(n)
        tags$button(type = "button", `data-span` = n, `aria-pressed` = "false",
                    title = if(n == 1) "Last year" else paste("Last", n, "years"),
                    `aria-label` = if(n == 1) "Last year" else paste("Last", n, "years"),
                    paste0(n, "Y"))),
      tags$button(type = "button", `data-span` = 0, `aria-pressed` = "false", "All"))
}
calc_start <- 2000           # the calculator opens on calc_start to the latest year

# a plain year <select> (kept in step with the years slider in the browser)
year_select <- function(id, years, selected, label){
  tags$select(id = id, class = "form-select", `aria-label` = label,
              lapply(years, function(y) tags$option(value = y, selected = if(y == selected) NA, y)))
}

# browser side of the years controls: presets, the range text, the
# calculator's year selects and amount box all follow the slider (input$years)
years_js <- sprintf("
(function(){
  var MIN = %d, MAX = %d, CALC_START = %d;
  // the chart tabs and the calculator each keep their own years
  var ranges = {charts: [MIN, MAX], calc: [CALC_START, MAX]}, group = 'charts';
  function slider(){ return $('#years').data('ionRangeSlider'); }
  function sync(){
    var s = slider(); if(!s) return;
    var a = s.result.from, b = s.result.to;
    ranges[group] = [a, b];
    $('#years-range').text(a === b ? a : a + '\u2013' + b);
    $('.year-presets button').each(function(){
      var n = +this.dataset.span, on = n ? (b === MAX && a === MAX - n) : (a === MIN && b === MAX);
      this.classList.toggle('active', on); this.setAttribute('aria-pressed', on);
    });
    $('#calc_from').val(Math.min(a, MAX - 1)); $('#calc_to').val(Math.max(b, MIN + 1));
  }
  function setYears(a, b){
    var s = slider(); if(!s) return;
    s.update({from: a, to: b}); $('#years').trigger('change'); sync();
    // send now, not after the slider's usual delay, so a tab switch and its
    // years reach the server together (the slider's own later send is a repeat)
    if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('years', [a, b]);
  }
  $(document).on('change', '#years', sync);
  // switching between the charts and the calculator restores that side's
  // years (sent to the server together with the tab, so one redraw)
  $(document).on('change', '#view input', function(){
    var g = this.value === 'calc' ? 'calc' : 'charts';
    if(g === group) return;
    group = g; var r = ranges[g]; setYears(r[0], r[1]);
  });
  $(document).on('shiny:sessioninitialized', function(){ setTimeout(sync, 0); });
  $(document).on('click', '.year-presets button', function(){
    var n = +this.dataset.span; setYears(n ? MAX - n : MIN, MAX);
  });
  $(document).on('change', '#calc_from, #calc_to', function(){
    var a = +$('#calc_from').val(), b = +$('#calc_to').val();
    if(a >= b){ if(this.id === 'calc_from') b = Math.min(a + 1, MAX); else a = Math.max(b - 1, MIN); }
    setYears(a, b);
  });
  // amount: show thousands separators once typing stops
  $(document).on('blur', '#amount', function(){
    var v = parseFloat(this.value.replace(/[^0-9.]/g, ''));
    if(isFinite(v) && v > 0){ var c = v %% 1 ? 2 : 0; this.value = v.toLocaleString('en-AU', {minimumFractionDigits: c, maximumFractionDigits: c}); }
  });
  $(document).on('keydown', '#amount', function(e){ if(e.key === 'Enter') this.blur(); });
})();", min_yr, max_yr, calc_start)

# segmented control that behaves as a shiny radio input (input$view)
segmented <- function(id, choices, selected){
  tags$div(
    id = id, class = "shiny-input-radiogroup seg", role = "radiogroup",
    lapply(names(choices), function(lab){
      val <- choices[[lab]]
      bid <- paste0(id, "_", val)
      tagList(
        tags$input(type = "radio", class = "btn-check", name = id, id = bid,
                   value = val, autocomplete = "off",
                   checked = if(val == selected) NA),
        tags$label(class = "btn", `for` = bid, lab)
      )
    })
  )
}

# Define UI for application ############################# 

# series toggles: a shiny checkbox group (input$show) styled as legend cards
series_toggles <- function(id){
  tags$div(
    id = id, class = "shiny-input-checkboxgroup series", role = "group",
    `aria-label` = "Series shown on the chart",
    lapply(seq_len(nrow(series)), function(i){
      bid <- paste0(id, "_", series$key[i])
      tagList(
        tags$input(type = "checkbox", class = "btn-check", name = id, id = bid,
                   value = series$key[i], autocomplete = "off", checked = NA),
        tags$label(class = "series-card", `for` = bid,
                   style = paste0("--cl:", series$colour[i], ";--cd:", series$colour_dark[i]),
                   title = "Click to show or hide",
                   div(class = "name", span(class = "dot"),
                       span(class = "lbl-full", series$label[i]),
                       span(class = "lbl-short", series$short[i])),
                   div(class = "figs",
                       span(class = "value", textOutput(paste0("val_", series$key[i]), inline = TRUE)),
                       span(class = "rate", textOutput(paste0("rate_", series$key[i]), inline = TRUE))),
                   uiOutput(paste0("ctx_", series$key[i]), class = "ctx")))
    }),
    # never let the last visible series be switched off
    tags$script(HTML(sprintf("
      document.getElementById('%s').addEventListener('click', function(e){
        var el = e.target;
        if (el.tagName !== 'INPUT') return;
        if (!el.checked && !this.querySelector('input:checked')) e.preventDefault();
      });", id)))
  )
}

ui <- page(
  theme = app_theme,
  title = "Australian & US Stock Markets",
  tags$head(tags$style(HTML(app_css)),
            # plotly.js, shipped with the app (MIT licence: www/plotly-LICENSE.txt)
            tags$script(src = "plotly-2.25.2.min.js"),
            # output binding for figureOutput(): update the chart in place
            tags$script(HTML("
(function(){
  var binding = new Shiny.OutputBinding();
  $.extend(binding, {
    find: function(scope){ return $(scope).find('.figure-output'); },
    renderValue: function(el, fig){
      if(!fig || !fig.data){ Plotly.purge(el); return; }
      Plotly.react(el, fig.data, fig.layout, fig.config);
    }
  });
  Shiny.outputBindings.register(binding, 'figure');
})();"))),
  
  div(class = "app",
      
      div(class = "app-header",
          div(tags$h1(id = "app-title", "Australian Stock Market"),
              tags$p(paste0("Shares, dividends and government bonds, ", min_yr, "–", max_yr, "."))),
          div(class = "header-actions",
              country_switch("country"),
              actionLink("about", "About the data", class = "about-link"),
              input_dark_mode(id = "mode"))),
      
      div(class = "chart-panel",
          div(class = "chart-bar",
              div(class = "chart-bar-left",
                  segmented("view",
                            c("Growth of $100" = "growth",
                              "Annual" = "annual",
                              "Rolling" = "rolling",
                              "Falls" = "falls",
                              "Dividends" = "calc",
                              "Valuation" = "value"),
                            selected = "growth"),
                  conditionalPanel("input.view == 'rolling'", class = "window",
                                   selectInput("window", "Window",
                                               choices = setNames(3:21, paste(3:21, "years")),
                                               selected = 10, width = "auto", selectize = FALSE)),
                  # each tab keeps its own layout choice
                  conditionalPanel("input.view == 'annual'", layout_switch("layout_annual", "stacked")),
                  conditionalPanel("input.view == 'rolling'", layout_switch("layout_rolling", "stacked")),
                  conditionalPanel("input.view == 'falls'", layout_switch("layout_falls", "stacked"))),
              input_switch("real", "Adjust for inflation", value = FALSE)),
          
          # calculator: the question as a sentence; its years follow the slider
          conditionalPanel("input.view == 'calc'", div(class = "calc-q",
                           span("Invest"),
                           span(class = "amount-wrap", span(class = "cur", "$"),
                                tags$input(id = "amount", type = "text", class = "form-control", value = "10,000",
                                           inputmode = "decimal", autocomplete = "off",
                                           `aria-label` = "Amount invested, in dollars")),
                           span("at the end of"),
                           year_select("calc_from", min_yr:(max_yr - 1), calc_start, "First year"),
                           span("until the end of"),
                           year_select("calc_to", (min_yr + 1):max_yr, max_yr, "Last year"),
                           year_presets())),

          uiOutput("chart_title"),
          conditionalPanel("['calc', 'value'].indexOf(input.view) < 0", series_toggles("show")),
          conditionalPanel("input.view == 'calc'", uiOutput("calc_cards")),
          conditionalPanel("input.view == 'value'", uiOutput("value_cards")),
          uiOutput("stats_note"),
          
          # years: current range and quick picks; the slider under the chart does any range
          conditionalPanel("['calc', 'value'].indexOf(input.view) < 0", div(class = "period",
                           span(class = "period-label", "Years"),
                           span(id = "years-range", class = "period-range", paste0(min_yr, "\u2013", max_yr)),
                           year_presets(),
                           span(class = "period-hint", "or drag the handles under the chart"))),

          figureOutput("chart", height = "540px"),
          # tell the server about phone widths (fewer event notes on the growth chart)
          tags$script(HTML("$(document).on('shiny:connected', function(){ Shiny.setInputValue('narrow', window.innerWidth < 640); });
" , years_js, "
$(document).on('change', '#view input', function(){ $('.years').toggle(this.value !== 'value'); });
// the Valuation tab uses US data only: show it for the US, and leave it for Growth when switching to Australia
function syncValueTab(){
  var us = $('#country input:checked').val() === 'us';
  $('label[for=view_value]').toggle(us);
  if(!us && $('#view_value').is(':checked')) $('label[for=view_growth]').click();
}
$(document).on('change', '#country input', syncValueTab); $(syncValueTab);
$(document).on('change', '#view input', function(){ var l = document.querySelector('label[for=' + this.id + ']'); if(l) l.scrollIntoView({block: 'nearest', inline: 'nearest', behavior: 'smooth'}); });")),
          
          div(class = "years",
              sliderInput("years", NULL,
                          min = min_yr, max = max_yr,
                          value = c(min_yr, max_yr),
                          sep = "", ticks = FALSE, width = "100%")),
          
          div(class = "chart-foot",
              div(conditionalPanel("['rolling', 'falls', 'calc', 'value'].indexOf(input.view) >= 0",
                                   tags$details(class = "how",
                                                tags$summary("How to read this chart"),
                                                uiOutput("how_to"))),
                  conditionalPanel("input.view == 'falls' && input.country == 'us'",
                                   tags$details(class = "how bears",
                                                tags$summary(sprintf("Bear markets at daily closes: %d S&P 500 falls of 20%% or more since 1928",
                                                                     nrow(bears))),
                                                bear_table))),
              downloadButton("download", "Download data (CSV)", class = "btn-quiet"))),
      
      div(class = "app-footer",
          div("Sources, Australia: ", source_links(sources$au)),
          div("Sources, United States: ", source_links(sources$us)))
  )
)


# Define server logic #############################
server <- function(input, output, session) {

  real <- reactive(isTRUE(input$real))
  stacked <- reactive(switch(input$view,
                             annual  = !identical(input$layout_annual, "overlay"),
                             rolling = !identical(input$layout_rolling, "overlay"),
                             falls   = !identical(input$layout_falls, "overlay"),
                             FALSE))
  theme <- reactive(if(identical(input$mode, "dark")) "dark" else "light")
  country <- reactive(if(identical(input$country, "us")) "us" else "au")
  mk <- reactive(markets[[country()]])

  #### dividend calculator ####
  amount <- reactive({
    a <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", if(is.null(input$amount)) "" else input$amount)))
    if(length(a) != 1 || is.na(a) || a <= 0) 10000 else min(a, 1e12)
  }) %>% debounce(350)
  calc_df <- reactive({
    req(input$years)
    dividend_path(raws[[country()]], input$years[1], input$years[2], amount(), real())
  })
  output$calc_cards <- renderUI({
    p <- calc_df(); n <- nrow(p); yrs <- p$year[n] - p$year[1]
    re <- p$reinvested[n]; sh <- p$shares[n]; ca <- p$cash[n]; taken <- sh + ca
    rate <- function(v) if(yrs > 0) sprintf("%.1f%% a year", 100 * ((v / amount())^(1 / yrs) - 1)) else ""
    card <- function(key, name, value, rate, ctx, cls = NULL){
      i <- match(key, series$key)
      div(class = paste("series-card", cls),
          style = paste0("--cl:", series$colour[i], ";--cd:", series$colour_dark[i]),
          div(class = "name", span(class = "dot"), span(name)),
          div(class = "figs", span(class = "value", value), span(class = "rate", rate)),
          div(class = "ctx", ctx))
    }
    div(class = "series calc",
        card("div", "Dividends reinvested", fmt_dollars(re), rate(re),
             "Every dividend bought more shares"),
        card("price", "Dividends taken as cash", fmt_dollars(taken), rate(taken),
             sprintf("Shares worth %s, plus %s of dividends paid out", fmt_dollars(sh), fmt_dollars(ca))),
        card("div", "Extra from reinvesting", paste0("+", fmt_dollars(re - taken)),
             if(taken > 0) sprintf("%.1f\u00d7 as much", re / taken) else "",
             if(yrs > 0) sprintf("Reinvested, the %s of dividends grew to %s", fmt_dollars(ca), fmt_dollars(re - sh))
             else "", cls = "gain"))
  })

  # index rebased to $100 at the first selected year (unrounded)
  index_df <- reactive({
    req(input$years)
    rebase_index(if(real()) mk()$index_real else mk()$index_nom,
                 input$years[1], input$years[2])
  })

  returns_df <- reactive({
    req(input$years)
    (if(real()) mk()$returns_real else mk()$returns_nom) %>%
      filter(market != "inflation",
             between(year, input$years[1], input$years[2]))
  })

  # full-history rolling returns: depend only on window and real/nominal,
  # so slider moves just filter. Cached app-wide on first use.
  rolling_full_df <- reactive({
    rolling_full(mk(), real(), as.numeric(input$window))
  }) %>% bindCache(country(), real(), input$window)

  rolling_df <- reactive({
    req(input$years)
    rolling_full_df() %>%
      filter(between(year, input$years[1], input$years[2]))
  })

  # fall below the highest value so far (since the first selected year)
  # US falls use monthly data; Australia has year-end values only
  monthly_falls <- reactive(country() == "us")
  falls_df <- reactive({
    req(input$years)
    src <- if(monthly_falls()){
      us_falls_index[[if(real()) "real" else "nom"]] %>%
        filter(year >= input$years[1], year <= input$years[2])
    } else index_df()
    src %>% group_by(market) %>% arrange(year, .by_group = TRUE) %>%
      mutate(dd = dollars / cummax(dollars) - 1) %>% ungroup()
  })
  falls_stats <- reactive({
    df <- falls_df()
    lapply(setNames(series$market, series$key), function(m){
      d <- df[df$market == m, ]; fall_stats(d$year, d$dollars)
    })
  })
  
  #### headline figures in the series cards ####
  shown <- reactive({
    s <- input$show
    if(length(s) == 0) series$key else s
  })
  
  # where each series' k-year return (ending at the selected end year) sits
  # among all k-year periods in the full history
  rolling_ctx <- reactive({
    df <- rolling_full_df(); y1 <- input$years[2]; k <- as.numeric(input$window)
    idx <- if(real()) mk()$index_real else mk()$index_nom
    lapply(setNames(seq_len(nrow(series)), series$key), function(i){
      h <- df[df$market == series$market[i] & !is.na(df$percent), ]
      d <- idx[idx$market == series$market[i], ]; d <- d[order(d$year), ]
      n_yrs <- d$year[nrow(d)] - d$year[1]
      v <- h$percent[h$year == y1]
      v <- if(length(v)) v else NA
      list(value = v, k = k, y1 = y1,
           pct_below = if(is.na(v)) NA else mean(h$percent < v),
           lo = unname(quantile(h$percent, .1)), hi = unname(quantile(h$percent, .9)),
           med = median(h$percent), first = min(h$year), n = nrow(h),
           # compound growth over the whole history, which long periods settle towards
           cagr = (d$dollars[nrow(d)] / d$dollars[1])^(1 / n_yrs) - 1,
           span = c(d$year[1], d$year[nrow(d)]))
    })
  })
  
  card_figs <- reactive({
    if(identical(input$view, "rolling")){
      return(lapply(rolling_ctx(), function(c){
        if(is.na(c$value)) return(list(value = "–", rate = "", ctx = NULL))
        p <- round(100 * c$pct_below)
        flag <- if(c$pct_below <= .10) "Near historic low" else if(c$pct_below >= .90) "Near historic high"
        list(value = sprintf("%.1f%%", 100 * c$value),
             rate = sprintf("a year, %d–%d", c$y1 - c$k + 1, c$y1),
             ctx = tagList(sprintf("Higher than %d%% of %d-year periods", p, c$k),
                           if(!is.null(flag)) span(class = "flag", flag),
                           span(class = "ctx-line", sprintf("Median %.1f%% \u00b7 %d\u2013%d: %.1f%% a year",
                                                            100 * c$med, c$span[1], c$span[2], 100 * c$cagr))))
      }))
    }
    if(identical(input$view, "falls")){
      return(lapply(falls_stats(), function(f){
        if(f$worst == 0) return(list(value = "None", rate = "never below an earlier high",
                                     ctx = NULL))
        mo <- monthly_falls()
        list(value = fmt_pct(f$worst),
             rate = sprintf("%s peak to %s low", fmt_when(f$peak, mo), fmt_when(f$low, mo)),
             ctx = tagList(if(is.na(f$back)) "Not yet back above that peak"
                           else sprintf("Back above the peak by %s, %s after it", fmt_when(f$back, mo), fmt_span(f$back - f$peak, mo)),
                           if(f$now < -0.05) span(class = "flag", sprintf("%s below peak now", fmt_pct(f$now)))))
      }))
    }
    df <- index_df()
    y0 <- input$years[1]; y1 <- input$years[2]; n <- y1 - y0
    lapply(setNames(seq_len(nrow(series)), series$key), function(i){
      end <- df$dollars[df$market == series$market[i] & df$year == y1]
      end <- if(length(end)) end else NA
      rate <- if(n > 0 && !is.na(end)) (end / 100)^(1 / n) - 1 else NA
      list(value = fmt_dollars(end),
           rate = if(is.na(rate)) "" else sprintf("%.1f%% a year", rate * 100),
           ctx = NULL)
    })
  })
  
  lapply(series$key, function(k){
    output[[paste0("val_", k)]] <- renderText(card_figs()[[k]]$value)
    output[[paste0("rate_", k)]] <- renderText(card_figs()[[k]]$rate)
    output[[paste0("ctx_", k)]] <- renderUI(card_figs()[[k]]$ctx)
    # hidden while empty (CSS :empty), so it must still update when hidden
    outputOptions(output, paste0("ctx_", k), suspendWhenHidden = FALSE)
  })
  
  #### valuation ####
  value_df <- reactive(value_data(real()))
  value_q  <- reactive(value_fifths(value_df()))
  output$value_cards <- renderUI({
    d <- value_df(); q <- value_q(); now <- d[nrow(d), ]
    card <- function(name, value, rate, ctx){
      div(class = "series-card",
          style = paste0("--cl:", series$colour[1], ";--cd:", series$colour_dark[1]),
          div(class = "name", span(class = "dot"), span(name)),
          div(class = "figs", span(class = "value", value), span(class = "rate", rate)),
          div(class = "ctx", ctx))
    }
    fifth <- function(i, name) card(name, sprintf("%.1f%%", 100 * q$med[i]), "a year, median of the next 10 years",
                                    sprintf("Range %s to %s across %d starting years", fmt_pct(q$worst[i], 1), fmt_pct(q$best[i], 1), q$n[i]))
    div(class = "series calc",
        card("CAPE now", sprintf("%.1f", now$cape), sprintf("December %d", now$year),
             sprintf("Higher than %d%% of Decembers since %d (median %.1f)",
                     floor(100 * mean(d$cape < now$cape)), d$year[1], median(d$cape))),
        fifth(5, sprintf("Dearest fifth: CAPE above %.0f", q$lo[5])),
        fifth(1, sprintf("Cheapest fifth: CAPE below %.0f", q$hi[1])))
  })

  output$stats_note <- renderUI({
    infl <- if(real()) "after inflation" else "before inflation"
    txt <- if(identical(input$view, "value")){
      d <- value_df(); s <- d[!is.na(d$fwd), ]
      paste0(sprintf("US shares with dividends, %s. Starting points from the end of %d to the end of %d; each bar is the median of one fifth of them. ",
                     infl, min(s$year), max(s$year)),
             "Hover over a dot for its years.")
    } else if(identical(input$view, "calc")){
      sprintf("%s invested in %s shares at the end of %s, valued at the end of %s, %s.",
              fmt_amount(amount()), if(country() == "us") "US" else "Australian",
              input$years[1], input$years[2],
              if(real()) paste0("after inflation (", input$years[1], " dollars)") else "before inflation")
    } else if(identical(input$view, "falls")){
      sprintf("Each %s against the highest value since the end of %s, %s. Click a series to show or hide it.",
              if(monthly_falls()) "month's value" else "year-end value", input$years[1], infl)
    } else if(identical(input$view, "rolling")){
      first <- min(sapply(rolling_ctx(), `[[`, "first"))
      sprintf("Average yearly return over the %s years to %s, %s, ranked against every %s-year period since %s. Click a series to show or hide it.",
              input$window, input$years[2], if(real()) "after inflation" else "before inflation",
              input$window, first)
    } else {
      sprintf("$100 invested at the end of %s, valued at the end of %s, %s. Click a series to show or hide it.",
              input$years[1], input$years[2],
              if(real()) paste0("after inflation (", input$years[1], " dollars)") else "before inflation")
    }
    div(class = "stats-note", txt)
  })
  
  #### rolling: how to read ####
  output$how_to <- renderUI({
    if(identical(input$view, "value")){
      d <- value_df(); q <- value_q(); now <- d[nrow(d), ]
      near <- d[!is.na(d$fwd) & d$cape >= 0.8 * now$cape, ]
      return(tags$ul(
        tags$li("CAPE (the cyclically adjusted price-to-earnings ratio, or Shiller P/E) is the share price divided by the average of the last 10 years of company earnings, after inflation. Averaging over a business cycle smooths out booms and recessions, so it measures how expensive shares are against their normal earning power."),
        tags$li(sprintf("Each dot is one December: the CAPE then (across) and the average yearly return of US shares with dividends over the next %d years (up). The bars are the median return for each fifth of the starting points, cheapest to dearest.", value_horizon)),
        tags$li(sprintf("Dearer starts have usually been followed by lower returns: a median of %.1f%% a year from the dearest fifth against %.1f%% from the cheapest.",
                        100 * q$med[5], 100 * q$med[1])),
        tags$li(sprintf("This is not a forecast. The spread at any CAPE is wide (%s to %s a year from the dearest fifth), neighbouring dots share 9 of their 10 years so the %d dots hold only about %d separate decades, and CAPE has stayed above its long-run median for most of the last 30 years.",
                        fmt_pct(q$worst[5], 1), fmt_pct(q$best[5], 1), sum(q$n), round(sum(q$n) / value_horizon))),
        tags$li(if(nrow(near)) sprintf("Today's %.1f is near the top of the range. The only starting points within 20%% of it are %s.",
                                       now$cape, paste(sprintf("%d (%s a year)", near$year, fmt_pct(near$fwd, 1)), collapse = ", "))
                else sprintf("Today's %.1f is above every starting point with 10 years of results.", now$cape)),
        tags$li("CAPE and earnings: Robert J. Shiller, Online Data. Returns: the same US series as the other tabs.")))
    }
    if(identical(input$view, "calc")) return(tags$ul(
      tags$li("Set the amount and the years in the sentence above the chart, or drag the handles under it. The money goes in at the end of the first year."),
      tags$li("Dividends reinvested follows the total-return index: each dividend buys more shares, which then pay dividends of their own. That snowball is the compounding."),
      tags$li("Dividends taken as cash holds the same shares and adds up the dividends they paid, kept as cash that earns nothing. The gap between the two is what reinvesting added."),
      tags$li("A year's dividend is the difference between the total return and the price return, applied to the shares' value at the start of that year."),
      tags$li("No tax, fees or franking credits are included. With inflation on, every amount is in first-year dollars."),
      # opens the About window, then scrolls to the method once it has loaded
      tags$li(HTML(paste0("The full method, with a worked example, is in ",
              tags$a(href = "#", onclick = "$(document).one('shown.bs.modal', function(){ var s = document.getElementById('dividend-method'); if(s) s.scrollIntoView({block: 'start'}); }); document.getElementById('about').click(); return false;",
                     "About the data"), ".")))))
    if(identical(input$view, "falls")) return(tags$ul(
      tags$li("Each point shows how far the value sat below its highest value so far. Zero means a new high."),
      if(monthly_falls())
        tags$li("US values are monthly: each month's average of daily closes (Shiller's data). Falls that recover within a year, such as 1987 and 2020, show, but the single worst days don't: in 2020 the S&P 500 fell 34% from its February high to its 23 March close, about 19% month to month.")
      else
        tags$li("Australian values are at year end, so falls and rebounds within a year (such as October 1987) don't show in full. Monthly history isn't available for Australia; the US version of this chart is monthly."),
      tags$li("The cards give the worst fall in the selected years, the years of the peak and the low, and when the earlier peak was regained."),
      tags$li(if(monthly_falls()) "Bonds earn the 10-year yield each month, so before inflation they never fall. Turn on inflation to see losses of buying power."
              else "Bonds use each December's 10-year yield as that year's return, so before inflation they never fall. Turn on inflation to see losses of buying power.")))
    k <- as.numeric(input$window); y1 <- input$years[2]
    first <- min(sapply(rolling_ctx(), `[[`, "first"))
    tags$ul(
      tags$li(sprintf("Each point is the average yearly return from holding for %d years, ending that year. The point at %d is what you would have earned per year from the end of %d to the end of %d.",
                      k, y1, y1 - k, y1)),
      tags$li("Neighbouring points share most of their years, so the line moves in smooth waves. These are not reliable cycles: the same returns shuffled into random order produce similar waves."),
      tags$li(sprintf("Switch to stacked rows, or show a single series, to see the typical range. The shaded band holds the middle 80%% of all %d-year periods since %d: above it are the best 10%%, below it the worst 10%%, and the line inside is the median.",
                      k, first)),
      tags$li(sprintf("Why the median differs between period lengths: ups and downs drag compound growth below the average year, so a short period's typical return sits nearer a typical single year, and longer periods settle towards the whole-history growth rate shown in the cards (%d\u2013%d). Try the 3-year and 20-year windows to see it.",
                      rolling_ctx()[[1]]$span[1], rolling_ctx()[[1]]$span[2])),
      tags$li("The cards rank the period ending at the slider's end year against the full history. \u201cHistoric low\u201d and \u201chistoric high\u201d describe the past; they are not forecasts."),
      tags$li("Bonds use each December's 10-year yield as that year's return, so gains or losses from changing interest rates are not included.")
    )
  })
  outputOptions(output, "how_to", suspendWhenHidden = FALSE)   # lives in a closed <details>
  
  #### chart ####
  output$chart_title <- renderUI({
    real_txt <- if(real()) ", after inflation" else ""
    switch(input$view,
           value = div(class = "chart-title",
                       tags$h2(paste0("Valuation and what came next", real_txt)),
                       tags$p("How expensive US shares were at each year end (CAPE), and the average yearly return over the next 10 years.")),
           calc = div(class = "chart-title",
                      tags$h2(paste0("What ", fmt_amount(amount()), " became, dividends reinvested or taken as cash", real_txt)),
                      tags$p("The lower shading is the dividends paid out; the upper shading is what reinvesting them added.")),
           growth = div(class = "chart-title",
                        tags$h2(paste0("Growth of $100 invested in ", input$years[1], real_txt)),
                        tags$p("Log scale: each gridline is ten times the one below, so equal slopes mean equal growth rates.")),
           annual = div(class = "chart-title",
                        tags$h2(paste0("Calendar-year returns", real_txt)),
                        tags$p(if(stacked()) "Return in each year, one row per series on a shared scale. The line in each row is the steady yearly rate that gives the same result over the period."
                               else "Return in each year.")),
           falls = div(class = "chart-title",
                       tags$h2(paste0("Falls from the previous high", real_txt)),
                       tags$p(if(stacked() && length(shown()) > 1) "How far each series sat below its highest value so far. The line in each row marks its worst fall."
                              else "How far each series sat below its highest value so far.")),
           rolling = div(class = "chart-title",
                         tags$h2(paste0("Rolling ", input$window, "-year returns", real_txt)),
                         tags$p(paste0("Average yearly return over the ", input$window,
                                       " years ending in each year. ",
                                       if(stacked() && length(shown()) > 1) sprintf("Each row shades where 80%% of that series' %s-year periods since %s landed.",
                                                             input$window, min(sapply(rolling_ctx(), `[[`, "first")))
                                       else if(length(shown()) > 1) "Show a single series, or switch to stacked rows, to see the typical range."
                                       else sprintf("The shaded band is where 80%% of all %s-year periods since %s landed.",
                                                    input$window, min(sapply(rolling_ctx(), `[[`, "first")))))))
  })
  
  output$chart <- renderFigure({
    req(input$mode)                        # wait for the browser's light/dark mode
    yrs <- input$years
    keep <- series$market[series$key %in% shown()]
    switch(input$view,
           calc = dividend_plot(calc_df(), yrs, narrow = isTRUE(input$narrow), theme = theme()),
           value = value_plot(value_df(), value_q(), theme = theme()),
           growth = {
             df <- index_df() %>% filter(market %in% keep)
             line_plot(df %>% mutate(dollars = round(dollars)),
                       y_col = "dollars",
                       x_range = yrs,
                       y_format = "$,.0f",
                       tick_format = "$~s",
                       log_y = TRUE,
                       y_range = c(min(c(df$dollars, 100), na.rm = TRUE),
                                   max(df$dollars, na.rm = TRUE)),
                       events = events[events$country == country(), ],
                       narrow = isTRUE(input$narrow),
                       theme = theme())
           },
           annual = {
             df <- returns_df() %>% filter(market %in% keep)
             if(stacked()){
               ref <- lapply(setNames(series$label, series$label), function(l){
                 r <- compound_rate(df$percent_yoy[df$market == series$market[series$label == l] &
                                                    df$year > yrs[1]])
                 if(is.na(r)) NULL else list(y = r, text = sprintf("%.1f%% a year", 100 * r))
               })
               stacked_plot(df, "percent_yoy", yrs, type = "bar", ref = ref, theme = theme())
             } else {
               line_plot(df, y_col = "percent_yoy", x_range = yrs,
                         y_format = ".1%", tick_format = ".0%", hline = 0, theme = theme())
             }
           },
           falls = {
             df <- falls_df() %>% filter(market %in% keep)
             fig <- if(stacked() && length(keep) > 1){
               ref <- lapply(setNames(series$key, series$label), function(k){
                 f <- falls_stats()[[k]]
                 if(f$worst == 0) NULL else list(y = f$worst, text = paste("worst", fmt_pct(f$worst)))
               })
               stacked_plot(df, "dd", yrs, type = "area", ref = ref, zero_label = "New high",
                            theme = theme())
             } else {
               lo <- min(c(df$dd, -0.01), na.rm = TRUE)
               line_plot(df, y_col = "dd", x_range = yrs, y_format = ".1%", tick_format = ".0%",
                         y_range = c(lo * 1.04, -lo * 0.07),    # headroom above 0 for the note
                         hline = 0, zero_label = "New high", theme = theme())
             }
             if(monthly_falls()) date_axis(fig) else fig
           },
           rolling = {
             # one series has nothing to stack: use the fully labelled single chart
             if(stacked() && length(keep) > 1){
               bands <- lapply(setNames(seq_len(nrow(series)), series$label), function(i){
                 c <- rolling_ctx()[[series$key[i]]]; list(lo = c$lo, hi = c$hi, med = c$med)
               })
               stacked_plot(rolling_df() %>% filter(market %in% keep), "percent", yrs,
                            type = "line", bands = bands, theme = theme())
             } else {
             band <- NULL
             if(length(keep) == 1){
               i <- match(keep, series$market); c <- rolling_ctx()[[series$key[i]]]
               band <- list(lo = c$lo, hi = c$hi, med = c$med,
                            colour = series_colours(theme())[[series$label[i]]])
             }
             line_plot(rolling_df() %>% filter(market %in% keep),
                       y_col = "percent",
                       x_range = yrs,
                       y_format = ".1%",
                       tick_format = ".0%",
                       hline = 0,
                       band = band,
                       theme = theme())
             }
           })
  }) %>% bindCache(country(), input$view, real(), input$years, shown(), theme(), stacked(),
                   if(identical(input$view, "rolling")) input$window,
                   if(identical(input$view, "calc")) amount(),
                   if(input$view %in% c("growth", "calc")) isTRUE(input$narrow))
  
  #### download: the data behind the current chart ####
  output$download <- downloadHandler(
    filename = function(){
      if(input$view == "value") return(paste0("us-cape-next-10-years", if(real()) "-real", ".csv"))
      paste0(if(country() == "us") "us" else "australian", "-market-", input$view,
             if(input$view == "rolling") paste0("-", input$window, "yr"),
             if(input$view == "calc") paste0("-", format(amount(), scientific = FALSE)),
             "-", input$years[1], "-", input$years[2],
             if(real()) "-real", ".csv")
    },
    content = function(file){
      if(identical(input$view, "value")){
        d <- value_df()
        return(write.csv(data.frame(year = d$year, cape = round(d$cape, 2),
                                    next_10yr_return = round(d$fwd, 4)),
                         file, row.names = FALSE, na = ""))
      }
      if(identical(input$view, "calc")){
        p <- calc_df()
        return(write.csv(data.frame(year = p$year,
                                    dividends_reinvested = round(p$reinvested, 2),
                                    shares_dividends_taken = round(p$shares, 2),
                                    dividends_paid_to_date = round(p$cash, 2)),
                         file, row.names = FALSE))
      }
      df <- switch(input$view,
                   falls   = falls_df() %>% mutate(value = dd,
                                                   year = if(monthly_falls()) { p <- month_parts(year); sprintf("%d-%02d", p$year, p$month) } else year),
                   growth  = index_df() %>% mutate(value = round(dollars, 2)),
                   annual  = returns_df() %>% mutate(value = percent_yoy),
                   rolling = rolling_df() %>% mutate(value = percent))
      df %>%
        inner_join(series, by = "market") %>%
        mutate(label = factor(label, levels = series$label)) %>%
        select(year, label, value) %>%
        arrange(label) %>%
        pivot_wider(names_from = label, values_from = value) %>%
        arrange(year) %>%
        { if(identical(input$view, "falls") && monthly_falls()) rename(., month = year) else . } %>%
        write.csv(file, row.names = FALSE, na = "")
    })

  #### about ####
  observeEvent(input$about, {
    showModal(modalDialog(includeHTML("about.Rhtml"),
                          title = "About the data", size = "l",
                          easyClose = TRUE, footer = modalButton("Close")))
  })
}


# Run the application ##########################################################
shinyApp(ui = ui, server = server)
