library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
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

####################################
#convert data to tidy format (pivot longer)
####################################
convert_to_tidy <- 
  function(df, nam_to_name = 'market', val_to_name){
    
    columns <- 
      df %>% 
      select(-year) %>% 
      colnames()
    
    df <- df %>% 
      pivot_longer(cols = columns,
                   names_to = nam_to_name,
                   values_to = val_to_name) %>% 
      mutate_if(is.character,as.factor) %>%
      #mutate(year = ymd(year, truncated = 2L))
      
      return(df)
  }

####################################
#function for creating new index column
####################################
calc_index <- 
  function(col_1, col_2, lag){
    
    out <- 
      lag({{col_1}}[lag] * cumprod(lead({{col_2}} + lag)))
    
    return(out)
  }

####################################
#function for creating new real return column
####################################
calc_real_return <- 
  function(col_1, col_2){
    
    out <- 
      (((1+{{col_1}})/(1+{{col_2}}))-1)
    
    return(out)
  }

####################################
#function for creating new rolling returns column
####################################
create_rr_col <- 
  function(df, group_by_col, calc_col, roll_years) {
    
    name_to_pass <- 
      paste0(roll_years)
    
    out <- 
      df %>%
      group_by({{group_by_col}}) %>%
      mutate(!!name_to_pass := ({{calc_col}} / 
                                  lag({{calc_col}},roll_years)) ^ 
               (1/roll_years) -1 )
    
    return(out)
  }

####################################
#function for creating rolling returns dataframe
####################################
create_rr_df <- function(df, filter = F, no_years = NULL){
  
  out <- 
    df %>% 
    create_rr_col(.,market,dollars,no_years) %>%
    # create_rr_col(.,market,dollars,5) %>%
    # create_rr_col(.,market,dollars,8) %>%
    # create_rr_col(.,market,dollars,13) %>%
    # create_rr_col(.,market,dollars,21) %>% 
    pivot_longer(cols = !c(year,market,dollars), 
                 names_to = 'roll_years', 
                 values_to = 'percent') %>% 
    mutate_if(is.character,as.factor) %>% 
    select(-dollars)
  
  
  if(filter == T){
    out <- 
      out %>%  
      filter(roll_years == no_years)
  }
  return(out)
  
}

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
# line chart, native plotly (ggplotly was ~6x slower per render)
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
      p <- plot_ly()
      for(lab in rev(levels(droplevels(df$label)))){     # lead series drawn last, on top
        d <- df[df$label == lab, ]
        lead <- lab == series$label[1]
        p <- p %>% 
          add_trace(data = d, x = ~year, y = ~y, name = lab,
                    type = "scatter", mode = "lines",
                    line = list(color = pal[[lab]], width = if(lead) 2.25 else 1.6,
                                shape = "linear"),
                    hovertemplate = paste0("%{y:", y_format, "}"))
      }
      p <- p %>% 
        add_trace(data = ends, x = ~year, y = ~y, type = "scatter", mode = "markers",
                  marker = list(color = unname(pal[as.character(ends$label)]), size = 7,
                                line = list(color = ink$page, width = 2)),
                  cliponaxis = FALSE, hoverinfo = "skip", showlegend = FALSE)
    }
    
    tick_font <- list(size = 11.5, color = ink$muted)
    y_axis <- list(title = "",
                   tickformat = tick_format,
                   tickfont = tick_font,
                   gridcolor = ink$grid, gridwidth = 1,
                   zeroline = FALSE, showline = FALSE,
                   ticks = "", nticks = 7,
                   automargin = FALSE,
                   fixedrange = TRUE)
    if(log_y){
      y_axis$type <- "log"
      y_axis$dtick <- 1                       # one tick per power of 10
      if(!is.null(y_range)) y_axis$range <- log10(y_range) + c(-0.05, 0.05)
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
    gap <- diff(x_range) * if(narrow) 0.18 else 0.07; last <- -Inf; ev_notes <- list()
    for(i in seq_len(nrow(ev))) if(ev$year[i] - last >= gap){
      last <- ev$year[i]
      ev_notes <- c(ev_notes, list(list(
        x = ev$year[i], xref = "x", y = 1, yref = "paper", yanchor = "bottom", yshift = 2,
        text = if(narrow) ev$short[i] else ev$label[i], showarrow = FALSE,
        font = list(size = 10.5, color = ink$muted))))
    }
    
    p %>% 
      layout(
        font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                    color = ink$primary),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,                   # the stat cards above are the legend
        xaxis = c(list(title = "",
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
                                      size = 12, color = ink$primary))) %>% 
      config(displayModeBar = FALSE)
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
  
  panels <- lapply(labs, function(l){
    d <- df[df$label == l, ]
    if(type == "bar"){
      plot_ly(d, x = ~year, y = ~y, type = "bar", name = l,
              marker = list(color = pal[[l]], line = list(width = 0)),
              hovertemplate = paste0(l, ": %{y:", y_format, "}<extra></extra>"))
    } else {
      last <- d[which.max(d$year), ]
      plot_ly(d, x = ~year, y = ~y, type = "scatter", mode = "lines", name = l,
              fill = if(type == "area") "tozeroy" else "none",
              fillcolor = if(type == "area") fade(pal[[l]], if(theme == "dark") 0.28 else 0.18),
              line = list(color = pal[[l]], width = if(l == series$label[1]) 2.25 else 1.8),
              hovertemplate = paste0(l, ": %{y:", y_format, "}<extra></extra>")) %>% 
        add_trace(data = last, x = ~year, y = ~y, type = "scatter", mode = "markers",
                  marker = list(color = pal[[l]], size = 7, line = list(color = ink$page, width = 2)),
                  cliponaxis = FALSE, hoverinfo = "skip", inherit = FALSE)
    }
  })
  p <- subplot(panels, nrows = n, shareX = TRUE, margin = 0.035)
  
  ax <- function(i) if(i == 1) "" else i
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
  
  y_axis <- list(range = y_rng, tickformat = tick_format, tickfont = tick_font,
                 gridcolor = ink$grid, zeroline = FALSE, showline = FALSE,
                 ticks = "", nticks = 4, automargin = FALSE, fixedrange = TRUE)
  x_axis <- list(title = "", range = if(type == "bar") x_range + c(-0.6, 0.6) else x_range,
                 tickformat = "d", tickangle = 0, tickfont = tick_font,
                 showgrid = FALSE, zeroline = FALSE, showline = FALSE, ticks = "",
                 showspikes = type != "bar", spikemode = "across", spikesnap = "cursor",
                 spikethickness = 1, spikecolor = ink$axis, spikedash = "solid",
                 fixedrange = TRUE)
  lay <- list(p,
              font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                          color = ink$primary),
              paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
              showlegend = FALSE, bargap = 0.35, annotations = notes,
              margin = list(t = 22, r = 84, b = 30, l = 52),
              hovermode = "x",
              hoverlabel = list(bgcolor = ink$hover_bg, bordercolor = ink$hover_border,
                                font = list(size = 12, color = ink$primary)))
  for(i in seq_len(n)){
    lay[[paste0("yaxis", ax(i))]] <- y_axis
    lay[[paste0("xaxis", ax(i))]] <- c(x_axis, if(i == n) list(
      showline = TRUE, linecolor = ink$axis, ticks = "outside", ticklen = 4,
      tickcolor = ink$axis), if(diff(x_range) <= 10) list(dtick = 1))
  }
  p <- plotly_build(do.call(layout, lay) %>% config(displayModeBar = FALSE))
  # subplot() drops layout shapes added before the build, so set them after it
  p$x$layout$shapes <- Filter(Negate(is.null), shapes)
  p
}

# compound yearly rate over a range (years after the first), as on the cards
compound_rate <- function(r) if(length(r)) exp(mean(log1p(r))) - 1 else NA

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
  if(!length(dd) || min(dd) > -1e-9) return(list(worst = 0, now = 0))
  i <- which.min(dd)
  peak_i <- which.max(dollars[seq_len(i)])
  back <- which(dollars >= dollars[peak_i] & seq_along(dollars) > i)[1]
  list(worst = dd[i], peak = year[peak_i], low = year[i],
       back = if(is.na(back)) NA else year[back], now = dd[length(dd)])
}

# market events marked on the growth chart
events <- tibble::tribble(
  ~year, ~label,             ~short,
  1893,  "1890s bank crash", "Bank crash",
  1930,  "Depression",       "Depression",
  1974,  "1974 crash",       "1974",
  1987,  "1987 crash",       "1987",
  2008,  "GFC",              "GFC",
  2020,  "COVID",            "COVID")

# $12.4M, $148k, $1,234
fmt_dollars <- function(x){
  if(is.na(x)) return("–")
  if(x >= 1e9) return(sprintf("$%.1fB", x / 1e9))
  if(x >= 1e6) return(sprintf("$%.1fM", x / 1e6))
  if(x >= 1e4) return(sprintf("$%.0fk", x / 1e3))
  paste0("$", formatC(round(x), format = "d", big.mark = ","))
}

######################################################################
#create final data frames
######################################################################
australianstockmarket <- 
  readRDS("data/australianstockmarket.RDS") %>% 
  rbind(data.frame(
    year = 1882,
    stock_accumulation = NA, 
    stock_price = NA, 
    bonds = NA, 
    inflation = NA  
  ), .)

#create new real return column and index columns for nominal and real returns
stock_temp <- 
  australianstockmarket %>% 
  mutate(across(-c(year,inflation), 
                ~calc_real_return(., inflation), .names = '{.col}_rr')) %>% 
  mutate(across(c(stock_accumulation,stock_price,bonds,inflation), 
                ~ifelse(year == 1882, 100, NA), .names = '{.col}_indx')) %>% 
  mutate(stock_accumulation_indx = 
           calc_index(stock_accumulation_indx, stock_accumulation , 1)) %>% 
  mutate(stock_price_indx = 
           calc_index(stock_price_indx, stock_price , 1))  %>% 
  mutate(bonds_indx = 
           calc_index(bonds_indx, bonds , 1)) %>% 
  mutate(inflation_indx = 
           calc_index(inflation_indx, inflation , 1)) %>%
  mutate(across(c(stock_accumulation_rr,stock_price_rr,bonds_rr), 
                ~ifelse(year == 1882, 100, NA), .names = '{.col}_indx')) %>% 
  mutate(stock_accumulation_rr_indx = 
           calc_index(stock_accumulation_rr_indx, stock_accumulation_rr , 1)) %>% 
  mutate(stock_price_rr_indx = 
           calc_index(stock_price_rr_indx, stock_price_rr , 1))  %>% 
  mutate(bonds_rr_indx = 
           calc_index(bonds_rr_indx, bonds_rr , 1))


min_yr <- min(australianstockmarket$year)
max_yr <- max(australianstockmarket$year)


#create nominal and real dataframes for returns and index in long format    
stock_returns_nom <-
  stock_temp %>% select(-contains("rr")) %>%   
  select(year, !ends_with('indx')) %>% 
  convert_to_tidy(val_to_name = 'percent_yoy') %>% 
  mutate(market = recode(market,'bonds' = 'Bonds 10yr',
                         'stock_price' = 'Stock Price',
                         'stock_accumulation' = 'Stock Price + Div'))

stock_index_nom <- 
  stock_temp %>% select(-contains("rr")) %>% 
  select(year, ends_with('indx')) %>% 
  mutate(across(c(-year), 
                ~ifelse(year == 1882, 100, .))) %>% 
  convert_to_tidy(val_to_name = 'dollars') %>% 
  mutate(market = recode(market,'bonds_indx' = 'Bonds 10yr',
                         'stock_price_indx' = 'Stock Price',
                         'stock_accumulation_indx' = 'Stock Price + Div'))

stock_returns_real <-
  stock_temp %>% select(year, contains("rr")) %>% 
  select(year, !ends_with('indx'))  %>% 
  convert_to_tidy(val_to_name = 'percent_yoy') %>% 
  mutate(market = recode(market,'bonds_rr' = 'Bonds 10yr',
                         'stock_price_rr' = 'Stock Price',
                         'stock_accumulation_rr' = 'Stock Price + Div'))

stock_index_real <- 
  stock_temp %>% select(year, contains("rr")) %>% 
  select(year, ends_with('indx')) %>% 
  mutate(across(c(-year), 
                ~ifelse(year == 1882, 100, .))) %>% 
  convert_to_tidy(val_to_name = 'dollars') %>% 
  mutate(market = recode(market,'bonds_rr_indx' = 'Bonds 10yr',
                         'stock_price_rr_indx' = 'Stock Price',
                         'stock_accumulation_rr_indx' = 'Stock Price + Div'))






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

# rolling returns over the full history for one window length
rolling_full <- function(real, no_years){
  create_rr_df(if(real) stock_index_real else stock_index_nom, 
               filter = T, no_years = no_years) %>% 
    ungroup() %>% 
    filter(market != "inflation_indx") %>% 
    mutate(percent = round(percent, 4))   # 0.01%; 2 dp made the lines stair-step
}

######################################################################
# sources
######################################################################
sources <- list(
  list("S&P Dow Jones Indices", "https://www.spglobal.com/spdji/en/indices/equity/all-ordinaries/#overview"),
  list("Reserve Bank of Australia", "https://www.rba.gov.au/statistics/tables"),
  list("Australian Bureau of Statistics", "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"),
  list("Brailsford et al. (2012)", "https://research.bond.edu.au/en/publications/the-historical-equity-risk-premium-in-australia-post-gfc-and-128-")
)

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
.series-card .flag { display: inline-block; margin-left: 6px; padding: 0 5px; border: 1px solid var(--line-2); border-radius: 3px; color: var(--ink); font-size: .68rem; font-weight: 600; letter-spacing: .04em; text-transform: uppercase; white-space: nowrap; }
.stats-note { color: var(--ink-3); font-size: .8rem; margin: 10px 0 18px; }

/* year range slider under the chart: hairline track, fine handles */
.years { padding: 0 77px 0 45px; }   /* plot margins (52 / 84) minus half a handle */
.years .form-group { margin: 0; }
.irs--shiny { height: 42px; }
.irs--shiny .irs-line { height: 1px; top: 26px; background: var(--line-2); border: 0; border-radius: 0; }
.irs--shiny .irs-bar { height: 2px; top: 25.5px; background: var(--ink-2); border: 0; }
.irs--shiny .irs-handle { width: 14px; height: 14px; top: 19px; background: var(--page); border: 1.5px solid var(--ink-2); box-shadow: none; cursor: grab; }
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
.modal-footer { border-top-color: var(--line); }

@media (max-width: 640px) {
  .app { padding: 28px 16px 40px; }
  .app-header { flex-direction: column; align-items: flex-start; gap: 6px; padding-bottom: 14px; }
  .app-header h1 { font-size: 1.45rem; }
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
  title = "Australian Stock Market",
  tags$head(tags$style(HTML(app_css))),
  
  div(class = "app",
      
      div(class = "app-header",
          div(tags$h1("Australian Stock Market"),
              tags$p(paste0("Shares, dividends and government bonds, ", min_yr, "–", max_yr, "."))),
          div(class = "header-actions",
              actionLink("about", "About the data", class = "about-link"),
              input_dark_mode(id = "mode"))),
      
      div(class = "chart-panel",
          div(class = "chart-bar",
              div(class = "chart-bar-left",
                  segmented("view",
                            c("Growth of $100" = "growth",
                              "Annual returns" = "annual",
                              "Rolling returns" = "rolling",
                              "Falls & recoveries" = "falls"),
                            selected = "growth"),
                  conditionalPanel("input.view == 'rolling'", class = "window",
                                   selectInput("window", "Window",
                                               choices = setNames(3:21, paste(3:21, "years")),
                                               selected = 10, width = "auto", selectize = FALSE)),
                  # each tab keeps its own layout choice
                  conditionalPanel("input.view == 'annual'", layout_switch("layout_annual", "stacked")),
                  conditionalPanel("input.view == 'rolling'", layout_switch("layout_rolling", "overlay")),
                  conditionalPanel("input.view == 'falls'", layout_switch("layout_falls", "stacked"))),
              input_switch("real", "Adjust for inflation", value = FALSE)),
          
          uiOutput("chart_title"),
          series_toggles("show"),
          uiOutput("stats_note"),
          
          plotlyOutput("chart", height = "540px"),
          # tell the server about phone widths (fewer event notes on the growth chart)
          tags$script(HTML("$(document).on('shiny:connected', function(){ Shiny.setInputValue('narrow', window.innerWidth < 640); });
$(document).on('change', '#view input', function(){ var l = document.querySelector('label[for=' + this.id + ']'); if(l) l.scrollIntoView({block: 'nearest', inline: 'nearest', behavior: 'smooth'}); });")),
          
          div(class = "years",
              sliderInput("years", NULL,
                          min = min_yr, max = max_yr,
                          value = c(min_yr, max_yr),
                          sep = "", ticks = FALSE, width = "100%")),
          
          div(class = "chart-foot",
              div(conditionalPanel("['rolling', 'falls'].indexOf(input.view) >= 0",
                                   tags$details(class = "how",
                                                tags$summary("How to read this chart"),
                                                uiOutput("how_to")))),
              downloadButton("download", "Download data (CSV)", class = "btn-quiet"))),
      
      div(class = "app-footer",
          "Sources: ",
          tagList(lapply(seq_along(sources), function(i){
            tagList(if(i > 1) " · ",
                    tags$a(href = sources[[i]][[2]], target = "_blank", rel = "noopener", sources[[i]][[1]]))
          })))
  )
)


# Define server logic #############################
server <- function(input, output, session) {

  real <- reactive(isTRUE(input$real))
  stacked <- reactive(switch(input$view,
                             annual  = !identical(input$layout_annual, "overlay"),
                             rolling = identical(input$layout_rolling, "stacked"),
                             falls   = !identical(input$layout_falls, "overlay"),
                             FALSE))
  theme <- reactive(if(identical(input$mode, "dark")) "dark" else "light")

  # index rebased to $100 at the first selected year (unrounded)
  index_df <- reactive({
    req(input$years)
    rebase_index(if(real()) stock_index_real else stock_index_nom,
                 input$years[1], input$years[2])
  })

  returns_df <- reactive({
    req(input$years)
    (if(real()) stock_returns_real else stock_returns_nom) %>%
      filter(market != "inflation",
             between(year, input$years[1], input$years[2]))
  })

  # full-history rolling returns: depend only on window and real/nominal,
  # so slider moves just filter. Cached app-wide on first use.
  rolling_full_df <- reactive({
    rolling_full(real(), as.numeric(input$window))
  }) %>% bindCache(real(), input$window)

  rolling_df <- reactive({
    req(input$years)
    rolling_full_df() %>%
      filter(between(year, input$years[1], input$years[2]))
  })

  # fall below the highest value so far (since the first selected year)
  falls_df <- reactive({
    index_df() %>% group_by(market) %>% arrange(year, .by_group = TRUE) %>%
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
    lapply(setNames(seq_len(nrow(series)), series$key), function(i){
      h <- df[df$market == series$market[i] & !is.na(df$percent), ]
      v <- h$percent[h$year == y1]
      v <- if(length(v)) v else NA
      list(value = v, k = k, y1 = y1,
           pct_below = if(is.na(v)) NA else mean(h$percent < v),
           lo = unname(quantile(h$percent, .1)), hi = unname(quantile(h$percent, .9)),
           med = median(h$percent), first = min(h$year), n = nrow(h))
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
                           if(!is.null(flag)) span(class = "flag", flag)))
      }))
    }
    if(identical(input$view, "falls")){
      return(lapply(falls_stats(), function(f){
        if(f$worst == 0) return(list(value = "None", rate = "never below an earlier high",
                                     ctx = NULL))
        list(value = fmt_pct(f$worst),
             rate = sprintf("%d peak to %d low", f$peak, f$low),
             ctx = tagList(if(is.na(f$back)) "Not yet back above that peak"
                           else sprintf("Back above the peak by %d, %d years after it", f$back, f$back - f$peak),
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
  
  output$stats_note <- renderUI({
    infl <- if(real()) "after inflation" else "before inflation"
    txt <- if(identical(input$view, "falls")){
      sprintf("Each year-end value against the highest value since %s, %s. Click a series to show or hide it.",
              input$years[1], infl)
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
    if(identical(input$view, "falls")) return(tags$ul(
      tags$li("Each point shows how far the value sat below its highest year-end value so far. Zero means a new high."),
      tags$li("Values are at year end, so falls and rebounds within a year (such as October 1987) don't show in full."),
      tags$li("The cards give the worst fall in the selected years, the years of the peak and the low, and when the earlier peak was regained."),
      tags$li("Bonds use each December's 10-year yield as that year's return, so before inflation they never fall. Turn on inflation to see losses of buying power.")))
    k <- as.numeric(input$window); y1 <- input$years[2]
    first <- min(sapply(rolling_ctx(), `[[`, "first"))
    tags$ul(
      tags$li(sprintf("Each point is the average yearly return from holding for %d years, ending that year. The point at %d is what you would have earned per year from the end of %d to the end of %d.",
                      k, y1, y1 - k, y1)),
      tags$li("Neighbouring points share most of their years, so the line moves in smooth waves. These are not reliable cycles: the same returns shuffled into random order produce similar waves."),
      tags$li(sprintf("Switch to stacked rows, or show a single series, to see the typical range. The shaded band holds the middle 80%% of all %d-year periods since %d: above it are the best 10%%, below it the worst 10%%, and the line inside is the median.",
                      k, first)),
      tags$li("The cards rank the period ending at the slider's end year against the full history. \u201cHistoric low\u201d and \u201chistoric high\u201d describe the past; they are not forecasts."),
      tags$li("Bonds use each December's 10-year yield as that year's return, so gains or losses from changing interest rates are not included.")
    )
  })
  outputOptions(output, "how_to", suspendWhenHidden = FALSE)   # lives in a closed <details>
  
  #### chart ####
  output$chart_title <- renderUI({
    real_txt <- if(real()) ", after inflation" else ""
    switch(input$view,
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
  
  output$chart <- renderPlotly({
    req(input$mode)                        # wait for the browser's light/dark mode
    yrs <- input$years
    keep <- series$market[series$key %in% shown()]
    switch(input$view,
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
                       events = events, narrow = isTRUE(input$narrow),
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
             if(stacked() && length(keep) > 1){
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
  }) %>% bindCache(input$view, real(), input$years, shown(), theme(), stacked(),
                   if(identical(input$view, "rolling")) input$window,
                   if(identical(input$view, "growth")) isTRUE(input$narrow))
  
  #### download: the data behind the current chart ####
  output$download <- downloadHandler(
    filename = function(){
      paste0("australian-market-", input$view,
             if(input$view == "rolling") paste0("-", input$window, "yr"),
             "-", input$years[1], "-", input$years[2],
             if(real()) "-real", ".csv")
    },
    content = function(file){
      df <- switch(input$view,
                   falls   = falls_df() %>% mutate(value = dd),
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
