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
  ~key,    ~market,             ~label,                 ~short,          ~colour,
  "div",   "Stock Price + Div", "Shares + dividends",   "Shares + div",  "#2c5b9a",
  "price", "Stock Price",       "Shares (price only)",  "Shares",        "#c4692f",
  "bonds", "Bonds 10yr",        "10-year govt bonds",   "Bonds",         "#2a8f73"
)
# palette validated with dataviz validate_palette.js (light, all pairs): all checks pass.
# Lines also carry direct end labels, so identity never relies on colour alone.

ink <- list(primary = "#111110", secondary = "#4f4e4a", muted = "#8a8882",
            grid = "#eeede8", axis = "#a9a79f", zero = "#8a8882")

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
           bars = FALSE,         # columns instead of lines (single series)
           band = NULL){         # list(lo, hi, med, colour): typical-range band
    
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
    ends$label_y <- spread_labels(tr(ends$y), span * 0.055)
    
    pal <- setNames(series$colour, series$label)
    if(bars){
      p <- 
        plot_ly(df, x = ~year, y = ~y, color = ~label, colors = pal,
                type = "bar",
                marker = list(line = list(width = 0), opacity = 0.9),
                hovertemplate = paste0("%{y:", y_format, "}"))
      ends <- ends[0, ]                     # single series: the title names it
    } else {
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
                                line = list(color = "#fcfcfa", width = 2)),
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
    
    labels <- lapply(seq_len(nrow(ends)), function(i) list(
      x = ends$year[i], y = ends$label_y[i], text = ends$short[i],
      xanchor = "left", xshift = 10, showarrow = FALSE,
      font = list(size = 12, color = ink$secondary)))
    
    p %>% 
      layout(
        font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                    color = ink$primary),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,                   # the stat cards above are the legend
        xaxis = c(list(title = "",
                     range = if(bars) x_range + c(-0.6, 0.6) else x_range,
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
                 fillcolor = band$colour, opacity = 0.08),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, layer = "below",
                 y0 = band$med, y1 = band$med,
                 line = list(color = band$colour, width = 1), opacity = 0.45)),
          if(!is.null(hline)) list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1, layer = "below",
            y0 = hline, y1 = hline, line = list(color = ink$zero, width = 1)))),
        annotations = labels,
        margin = list(t = 8, r = 84, b = 30, l = 52),  # fixed so the slider lines up below
        bargap = 0.35,
        hovermode = "x unified",
        hoverdistance = -1,
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d6d4cc",
                          font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
                                      size = 12, color = ink$primary))) %>% 
      config(displayModeBar = FALSE)
  }


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
  --line: #e6e4de; --line-2: #d6d4cc; --page: #fcfcfa;
}
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
.irs--shiny .irs-handle { width: 14px; height: 14px; top: 19px; background: #fff; border: 1.5px solid var(--ink-2); box-shadow: none; cursor: grab; }
.irs--shiny .irs-handle:hover, .irs--shiny .irs-handle.state_hover { background: #fff; border-color: var(--ink); }
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
  .seg { gap: 18px; }
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
                   style = paste0("--c:", series$colour[i]),
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
          actionLink("about", "About the data", class = "about-link")),
      
      div(class = "chart-panel",
          div(class = "chart-bar",
              div(class = "chart-bar-left",
                  segmented("view",
                            c("Growth of $100" = "growth",
                              "Annual returns" = "annual",
                              "Rolling returns" = "rolling"),
                            selected = "growth"),
                  conditionalPanel("input.view == 'rolling'", class = "window",
                                   selectInput("window", "Window",
                                               choices = setNames(3:21, paste(3:21, "years")),
                                               selected = 10, width = "auto", selectize = FALSE))),
              input_switch("real", "Adjust for inflation", value = FALSE)),
          
          uiOutput("chart_title"),
          series_toggles("show"),
          uiOutput("stats_note"),
          
          plotlyOutput("chart", height = "540px"),
          
          div(class = "years",
              sliderInput("years", NULL,
                          min = min_yr, max = max_yr,
                          value = c(min_yr, max_yr),
                          sep = "", ticks = FALSE, width = "100%")),
          
          div(class = "chart-foot",
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
    txt <- if(identical(input$view, "rolling")){
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
  
  #### chart ####
  output$chart_title <- renderUI({
    real_txt <- if(real()) ", after inflation" else ""
    switch(input$view,
           growth = div(class = "chart-title",
                        tags$h2(paste0("Growth of $100 invested in ", input$years[1], real_txt)),
                        tags$p("Log scale: each gridline is ten times the one below, so equal slopes mean equal growth rates.")),
           annual = div(class = "chart-title",
                        tags$h2(paste0("Calendar-year returns", real_txt)),
                        tags$p("Return in each year. Bonds show the 10-year yield at December.")),
           rolling = div(class = "chart-title",
                         tags$h2(paste0("Rolling ", input$window, "-year returns", real_txt)),
                         tags$p(paste0("Average yearly return over the ", input$window,
                                       " years ending in each year. ",
                                       if(length(shown()) > 1) "Show a single series to see its typical range."
                                       else sprintf("Shaded: the middle 80%% of all %s-year periods since %s; line: the median.",
                                                    input$window, min(sapply(rolling_ctx(), `[[`, "first")))))))
  })
  
  output$chart <- renderPlotly({
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
                                   max(df$dollars, na.rm = TRUE)))
           },
           annual = line_plot(returns_df() %>% filter(market %in% keep),
                              y_col = "percent_yoy",
                              x_range = yrs,
                              y_format = ".1%",
                              tick_format = ".0%",
                              hline = 0,
                              bars = length(keep) == 1),   # one series reads better as columns
           rolling = {
             band <- NULL
             if(length(keep) == 1){
               i <- match(keep, series$market); c <- rolling_ctx()[[series$key[i]]]
               band <- list(lo = c$lo, hi = c$hi, med = c$med, colour = series$colour[i])
             }
             line_plot(rolling_df() %>% filter(market %in% keep),
                       y_col = "percent",
                       x_range = yrs,
                       y_format = ".1%",
                       tick_format = ".0%",
                       hline = 0,
                       band = band)
           })
  }) %>% bindCache(input$view, real(), input$years, shown(),
                   if(identical(input$view, "rolling")) input$window)
  
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
