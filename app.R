#######################################################
# Australian and US share market history: a Shiny app.
#
#   R/constants.R  theme, series, colours, events, sources
#   R/format.R     number and date formatting
#   R/market.R     returns, indices, falls, dividends, CAPE fit
#   R/figures.R    the plotly.js figures (built as lists)
#   R/controls.R   custom inputs
#   www/app.css    styles; www/app.js  browser side
#
# Shiny sources R/ before this file. Data: data/*.RDS, built by code/.
#######################################################
library(shiny)
library(dplyr)
library(tidyr)
library(bslib)

options(scipen = 999)
# errors reach the browser as a generic message, never R's own text
options(shiny.sanitize.errors = TRUE)

######################################################################
# data
######################################################################
au_raw <- readRDS("data/australianstockmarket.RDS")
us_raw <- readRDS("data/usstockmarket.RDS")    # built by code/us_data.R
max_yr <- min(max(au_raw$year), max(us_raw$year))   # both countries cover the same years
min_yr <- min(au_raw$year) - 1
markets <- list(au = prep_market(au_raw, max_yr),
                us = prep_market(us_raw, max_yr))
raws <- list(au = au_raw[au_raw$year <= max_yr, ], us = us_raw[us_raw$year <= max_yr, ])

# US monthly indices for the falls tab (built by code/us_monthly.R), to the
# same last year as the annual data
us_monthly_all <- readRDS("data/usmonthly.RDS")      # runs past the last complete year
us_monthly <- us_monthly_all[us_monthly_all$year <= max_yr, ]
monthly_index <- function(m, real){
  x <- m$year + (m$month - 12) / 12
  defl <- if(real) m$cpi / 100 else 1
  lv <- c("Bonds 10yr", "Stock Price + Div", "Stock Price")
  tibble::tibble(year = rep(x, 3),
                 market = factor(rep(c("Stock Price + Div", "Stock Price", "Bonds 10yr"), each = length(x)), levels = lv),
                 dollars = c(m$tr, m$price, m$bonds) / defl)
}
us_falls_index <- list(nom = monthly_index(us_monthly, FALSE), real = monthly_index(us_monthly, TRUE))

# US shares with dividends at daily closes from July 1926 (code/us_daily.R,
# Kenneth R. French Data Library), monthly before that: used for the worst-fall
# figures on the falls cards, which the monthly chart line would understate.
# x is on the same year scale as the chart (31 December of a year = that year).
us_daily <- readRDS("data/usdaily.RDS")
daily_start <- min(us_daily$date)
us_daily <- us_daily[us_daily$date <= as.Date(sprintf("%d-12-31", max_yr)), ]
us_div_exact <- local({
  m <- us_monthly[us_monthly$year * 100 + us_monthly$month < as.integer(format(daily_start, "%Y%m")), ]
  yr <- as.integer(format(us_daily$date, "%Y"))
  doy <- as.integer(format(us_daily$date, "%j"))
  len <- ifelse((yr %% 4 == 0 & yr %% 100 != 0) | yr %% 400 == 0, 366, 365)
  d <- data.frame(x = c(m$year - 1 + m$month / 12, yr - 1 + doy / len),
                  date = c(as.Date(sprintf("%d-%02d-15", m$year, m$month)), us_daily$date),
                  dollars = c(m$tr, m$tr[nrow(m)] * us_daily$tr / 100))   # joined at June 1926
  # after inflation: each day deflated by its month's CPI
  cpi <- us_monthly$cpi[match(format(d$date, "%Y%m"), sprintf("%d%02d", us_monthly$year, us_monthly$month))]
  stopifnot(!anyNA(cpi))
  list(nom = d, real = transform(d, dollars = dollars / (cpi / 100)))
})
# "3 Sep 1929" for daily dates, "Oct 1907" for the monthly years before
fmt_day <- function(d) if(d >= daily_start) paste(as.integer(format(d, "%d")), format(d, "%b %Y")) else format(d, "%b %Y")

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
# today's CAPE: the latest month in Shiller's data, which runs past the last
# complete year (the dots stay at year ends)
cape_now <- local({
  m <- us_monthly_all[!is.na(us_monthly_all$cape), ]; m <- m[nrow(m), ]
  list(cape = m$cape, year = m$year, month = m$month, when = paste(month.name[m$month], m$year))
})

######################################################################
# UI
######################################################################
# www/ files are linked with a hash of their contents, so browsers cache them
# and pick up a new version as soon as it is deployed
asset <- function(f) paste0(f, "?v=", substr(unname(tools::md5sum(file.path("www", f))), 1, 10))

# a function of the request, so a bookmarked link (?_inputs_&view=...)
# opens on the state it records (restored by the controls in R/controls.R)
ui <- function(request) page(
  theme = app_theme,
  title = "Australian & US Stock Markets",
  tags$head(tags$link(rel = "stylesheet", href = asset("app.css")),
            # plotly.js, shipped with the app (MIT licence: www/plotly-LICENSE.txt)
            tags$script(src = "plotly-basic-2.25.2.min.js"),
            tags$script(HTML(sprintf("window.appConfig = {min: %d, max: %d, calcStart: %d};",
                                     min_yr, max_yr, calc_start))),
            tags$script(src = asset("app.js"))),
  # a spinner on the chart while the server works (only if it takes a while),
  # and a thin bar along the top of the page
  useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = FALSE),
  busyIndicatorOptions(spinner_selector = "#chart", spinner_delay = "400ms"),
  
  div(class = "app",
      
      div(class = "app-header",
          div(tags$h1(id = "app-title", if(restored("country", "au", names(countries)) == "us") "US Stock Market"
                                         else "Australian Stock Market"),
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
                            extras = list(value = list(before = HTML(sub('class="cflag"', 'class="cflag tab-flag"', flag_svg$us)),
                                                       title = "US only: uses Shiller's CAPE for US shares")),
                            selected = "growth"),
                  conditionalPanel("input.view == 'rolling'", class = "window",
                                   selectInput("window", "Window",
                                               choices = setNames(3:21, paste(3:21, "years")),
                                               selected = 10, width = "auto", selectize = FALSE)),
                  conditionalPanel("input.view == 'value'", class = "window",
                                   selectInput("horizon", "Returns over the next",
                                               choices = setNames(c(5, 10, 15, 20), paste(c(5, 10, 15, 20), "years")),
                                               selected = value_horizon, width = "auto", selectize = FALSE)),
                  # each tab keeps its own layout choice
                  conditionalPanel("input.view == 'annual'", layout_switch("layout_annual", "stacked")),
                  conditionalPanel("input.view == 'rolling'", layout_switch("layout_rolling", "stacked")),
                  conditionalPanel("input.view == 'falls'", layout_switch("layout_falls", "stacked"))),
              input_switch("real", "Adjust for inflation", value = FALSE)),
          
          # calculator: the question as a sentence; its years follow the slider
          conditionalPanel("input.view == 'calc'", div(class = "calc-q",
                           span("Invest"),
                           span(class = "amount-wrap", span(class = "cur", "$"),
                                tags$input(id = "amount", type = "text", class = "form-control",
                                           value = local({ v <- restoreInput("amount", "10,000")
                                             if(is.character(v) && length(v) == 1) substr(v, 1, 20) else "10,000" }),
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
          conditionalPanel("input.view == 'value'", div(class = "period",
                           text_switch("measure", "Starting point",
                                       c("CAPE" = "cape", "Return in the years before" = "past"), "cape"),
                           input_switch("fit", "Show trend line (regression)", value = FALSE))),
          conditionalPanel("['calc', 'value'].indexOf(input.view) < 0", div(class = "period",
                           span(class = "period-label", "Years"),
                           span(id = "years-range", class = "period-range", paste0(min_yr, "\u2013", max_yr)),
                           year_presets(),
                           span(class = "period-hint", "or drag the handles under the chart"))),

          figureOutput("chart", height = "540px"),
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
                  conditionalPanel("input.view == 'value' && input.fit",
                                   tags$details(class = "how bears",
                                                tags$summary("Regression details: estimates, standard errors, confidence intervals"),
                                                uiOutput("value_reg"))),
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

  #### inputs, checked ####
  # Shiny takes whatever value a browser sends (Shiny.setInputValue can send
  # anything, whatever the controls offer), so every input is read through
  # one of these: an allowed value or the default, never an error or a huge
  # allocation.
  view <- reactive({
    v <- input$view
    if(is.character(v) && length(v) == 1 && v %in% c("growth", "annual", "rolling", "falls", "calc", "value")) v
    else "growth"
  })
  # whole years within the data, in order (from == to is allowed: one year)
  yrs <- reactive({
    y <- suppressWarnings(as.numeric(input$years))
    req(length(y) == 2, all(is.finite(y)))
    sort(pmin(pmax(round(y), min_yr), max_yr))
  })
  win <- reactive({                       # rolling window, 3 to 21 years
    w <- suppressWarnings(as.integer(input$window))
    if(length(w) == 1 && !is.na(w) && w >= 3 && w <= 21) w else 10L
  })
  real <- reactive(isTRUE(input$real))
  stacked <- reactive(switch(view(),
                             annual  = !identical(input$layout_annual, "overlay"),
                             rolling = !identical(input$layout_rolling, "overlay"),
                             falls   = !identical(input$layout_falls, "overlay"),
                             FALSE))
  country <- reactive(if(identical(input$country, "us")) "us" else "au")
  mk <- reactive(markets[[country()]])

  #### dividend calculator ####
  amount <- reactive({
    a <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", if(is.null(input$amount)) "" else input$amount)))
    if(length(a) != 1 || is.na(a) || a <= 0) 10000 else min(a, 1e12)
  }) %>% debounce(350)
  calc_df <- reactive({
    req(yrs())
    dividend_path(raws[[country()]], yrs()[1], yrs()[2], amount(), real())
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
    req(yrs())
    rebase_index(if(real()) mk()$index_real else mk()$index_nom,
                 yrs()[1], yrs()[2])
  })

  returns_df <- reactive({
    req(yrs())
    (if(real()) mk()$returns_real else mk()$returns_nom) %>%
      filter(market != "inflation",
             between(year, yrs()[1], yrs()[2]))
  })

  # full-history rolling returns: depend only on window and real/nominal,
  # so slider moves just filter. Cached app-wide on first use.
  rolling_full_df <- reactive({
    rolling_full(mk(), real(), as.numeric(win()))
  }) %>% bindCache(country(), real(), win())

  rolling_df <- reactive({
    req(yrs())
    rolling_full_df() %>%
      filter(between(year, yrs()[1], yrs()[2]))
  })

  # fall below the highest value so far (since the first selected year)
  # US falls use monthly data; Australia has year-end values only
  monthly_falls <- reactive(country() == "us")
  falls_df <- reactive({
    req(yrs())
    src <- if(monthly_falls()){
      us_falls_index[[if(real()) "real" else "nom"]] %>%
        filter(year >= yrs()[1], year <= yrs()[2])
    } else index_df()
    src %>% group_by(market) %>% arrange(year, .by_group = TRUE) %>%
      mutate(dd = dollars / cummax(dollars) - 1) %>% ungroup()
  })
  falls_stats <- reactive({
    df <- falls_df()
    out <- lapply(setNames(series$market, series$key), function(m){
      d <- df[df$market == m, ]; fall_stats(d$year, d$dollars)
    })
    if(monthly_falls()){
      d <- us_div_exact[[if(real()) "real" else "nom"]]
      d <- d[d$x >= yrs()[1] & d$x <= yrs()[2], ]
      out$div <- c(fall_stats(d$date, d$dollars), exact = TRUE)
    }
    out
  })
  
  #### headline figures in the series cards ####
  shown <- reactive({
    s <- intersect(as.character(unlist(input$show)), series$key)
    if(length(s) == 0) series$key else s
  })
  
  # where each series' k-year return (ending at the selected end year) sits
  # among all k-year periods in the full history
  rolling_ctx <- reactive({
    df <- rolling_full_df(); y1 <- yrs()[2]; k <- as.numeric(win())
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
    if(identical(view(), "rolling")){
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
    if(identical(view(), "falls")){
      return(lapply(falls_stats(), function(f){
        if(f$worst == 0) return(list(value = "None", rate = "never below an earlier high",
                                     ctx = NULL))
        mo <- monthly_falls(); exact <- isTRUE(f$exact)
        when <- function(x) if(exact) fmt_day(x) else fmt_when(x, mo)
        gap  <- function(a, b) if(exact) fmt_span(as.numeric(b - a) / 365.25, TRUE) else fmt_span(b - a, mo)
        list(value = fmt_pct(f$worst),
             rate = sprintf("%s peak to %s low", when(f$peak), when(f$low)),
             ctx = tagList(if(is.na(f$back)) "Not yet back above that peak"
                           else sprintf("Back above the peak by %s, %s after it", when(f$back), gap(f$peak, f$back)),
                           if(exact && f$low >= daily_start)
                             span(class = "ctx-line", "At daily closes; the chart line uses monthly averages."),
                           if(f$now < -0.05) span(class = "flag", sprintf("%s below peak now", fmt_pct(f$now)))))
      }))
    }
    df <- index_df()
    y0 <- yrs()[1]; y1 <- yrs()[2]; n <- y1 - y0
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
  horizon  <- reactive({                 # 5, 10, 15 or 20 years
    h <- suppressWarnings(as.integer(input$horizon))
    if(length(h) == 1 && !is.na(h) && h %in% c(5L, 10L, 15L, 20L)) h else as.integer(value_horizon)
  })
  # the same for every visitor with the same choices: cached app-wide
  value_df <- reactive(value_data(if(real()) markets$us$index_real else markets$us$index_nom, us_cape, horizon())) %>% bindCache(real(), horizon())
  # what the starting points are sorted by: CAPE, or the return over the
  # h years before (the same length as the h years after)
  measure <- reactive({
    m <- input$measure
    if(is.character(m) && length(m) == 1 && m %in% names(value_measures)) m else "cape"
  })
  value_pts <- reactive(value_points(value_df(), measure())) %>% bindCache(real(), horizon(), measure())
  value_q  <- reactive(value_fifths(value_pts())) %>% bindCache(real(), horizon(), measure())
  # today's value of the measure, and the words and axis that go with it
  value_now <- reactive({
    h <- horizon(); d <- value_df(); pts <- value_pts()
    if(measure() == "cape"){
      x <- cape_now$cape
      list(x = x, year = cape_now$year, month = cape_now$month, x_all = d$cape,
           label = sprintf("CAPE %.1f", x), value = sprintf("%.1f", x), when = cape_now$when,
           when_short = paste(month.abb[cape_now$month], cape_now$year),
           near = pts[pts$x >= 0.8 * x, ],                       # within 20% of today's CAPE
           x_hover = "CAPE at the start: %{x:.1f}", trend_hover = "at CAPE %{x:.1f}",
           x_title = "CAPE at the start (price \u00f7 10-year average earnings)", x_format = NULL,
           x_range = function(v) c(0, max(v) * 1.04),
           trend_x = function(fit) x - 1, trend_anchor = "right")        # just left of today
    } else {
      x <- d$past[d$year == max_yr]; x_all <- d$past[!is.na(d$past)]
      list(x = x, year = max_yr, month = 12L, x_all = x_all, first = min(d$year[!is.na(d$past)]),
           label = sprintf("%.1f%% a year", 100 * x), value = sprintf("%.1f%%", 100 * x),
           when = sprintf("end of %d to end of %d", max_yr - h, max_yr),
           when_short = sprintf("%d\u2013%d", max_yr - h + 1, max_yr),
           near = local({ k <- order(abs(pts$x - x))[seq_len(min(5, nrow(pts)))]   # the five closest
                          pts[sort(k), ] }),
           x_hover = sprintf("Previous %d years: %%{x:.1%%} a year", h), trend_hover = "after %{x:.1%} a year",
           x_title = sprintf("Average yearly return over the previous %d years", h), x_format = ".0%",
           x_range = function(v){ r <- range(v); r + c(-1, 1) * diff(r) * 0.04 },
           trend_x = function(fit) min(fit$grid) + 0.005, trend_anchor = "left")   # at its left end
    }
  })
  value_reg <- reactive(value_fit(value_pts(), horizon(), value_now()$x, value_measures[[measure()]]$ln)) %>%
    bindCache(real(), horizon(), measure())
  output$value_reg <- renderUI({
    req(isTRUE(input$fit), identical(view(), "value"))      # only worked out when asked for
    f <- value_reg(); pp <- function(x, d = 1) sub("^-", "\u2212", sprintf(paste0("%.", d, "f"), 100 * x))
    cape <- f$ln                                     # CAPE (on ln CAPE), or the past return
    # the CAPE slope is in points a year per unit of ln(CAPE); the past-return
    # slope is points of the next return per point of the past one
    est <- list(function(x) pp(x, 2), if(cape) function(x) pp(x, 2) else function(x) pp(x / 100, 2))
    term <- c("Intercept (a)", if(cape) "ln(CAPE) (b)" else sprintf("Past %d-year return (b)", f$h))
    tagList(
      tags$p(class = "bear-notes",
             if(cape) sprintf("Model: average yearly return over the next %d years = a + b \u00d7 ln(CAPE). Fitted by least squares to %d starting years (end of %d to end of %d), US shares with dividends, %s. Estimates are in percentage points a year.",
                              f$h, f$n, f$years[1], f$years[2], if(real()) "after inflation" else "before inflation")
             else sprintf("Model: average yearly return over the next %d years = a + b \u00d7 the average yearly return over the previous %d years. Fitted by least squares to %d starting years (end of %d to end of %d), US shares with dividends, %s. The intercept is in percentage points a year; b is the change in the next return for each extra point a year in the past one.",
                          f$h, f$h, f$n, f$years[1], f$years[2], if(real()) "after inflation" else "before inflation")),
      tags$table(class = "method-table bear-table",
        tags$thead(tags$tr(tags$th(""), tags$th(class = "num", "Estimate"), tags$th(class = "num", "Std error (OLS)"),
                           tags$th(class = "num", "Std error (Newey\u2013West)"), tags$th(class = "num", "95% CI (Newey\u2013West)"), tags$th(class = "num", "t (Newey\u2013West)"))),
        tags$tbody(lapply(1:2, function(i) tags$tr(
          tags$td(term[i]), tags$td(class = "num", est[[i]](f$b[i])), tags$td(class = "num", est[[i]](f$se_ols[i])),
          tags$td(class = "num", est[[i]](f$se_nw[i])),
          tags$td(class = "num", sprintf("%s to %s", est[[i]](f$b[i] - 1.96 * f$se_nw[i]), est[[i]](f$b[i] + 1.96 * f$se_nw[i]))),
          tags$td(class = "num", sub("^-", "\u2212", sprintf("%.1f", f$b[i] / f$se_nw[i]))))))),
      tags$table(class = "method-table bear-table",
        tags$tbody(
          tags$tr(tags$td("R\u00b2"), tags$td(class = "num", sprintf("%.2f", f$r2))),
          if(cape) tags$tr(tags$td("R\u00b2 using 1/CAPE (the earnings yield) instead"), tags$td(class = "num", sprintf("%.2f", f$r2_inv))),
          tags$tr(tags$td("Residual standard deviation"), tags$td(class = "num", paste(pp(f$sigma), "points"))),
          tags$tr(tags$td("Starting years / separate periods"), tags$td(class = "num", sprintf("%d / about %d", f$n, round(f$n / f$h)))),
          if(cape) tags$tr(tags$td("A doubling of CAPE goes with"), tags$td(class = "num", sprintf("%s points a year", pp(f$b[2] * log(2)))))
          else tags$tr(tags$td("5 more points a year in the past goes with"), tags$td(class = "num", sprintf("%s points a year", pp(f$b[2] * 0.05)))),
          tags$tr(tags$td(if(cape) sprintf("Trend line at today's CAPE (%.1f)", f$x_now)
                          else sprintf("Trend line at today's past return (%s%% a year)", pp(f$x_now))),
                  tags$td(class = "num", sprintf("%s%% a year", pp(f$f0)))),
          tags$tr(tags$td("95% CI for that average (Newey\u2013West)"), tags$td(class = "num", sprintf("%s%% to %s%%", pp(f$ci0[1]), pp(f$ci0[2])))),
          tags$tr(tags$td("Rough 95% range for a single outcome"), tags$td(class = "num", sprintf("%s%% to %s%%", pp(f$pi0[1]), pp(f$pi0[2])))))),
      tags$ul(class = "bear-notes",
        tags$li(sprintf("Newey\u2013West standard errors: neighbouring dots share %d of their %d years, so their errors are correlated and ordinary (OLS) standard errors look too precise. Newey\u2013West, with a lag of %d years, allows for that; it is the column to use.", f$h - 1, f$h, f$h - 1)),
        if(cape) tags$li("ln(CAPE) rather than CAPE: the link flattens at high valuations, and the log fits that better. The earnings-yield version (1/CAPE) is the other common choice; its R\u00b2 is shown for comparison."),
        if(cape) tags$li("This is an in-sample fit: the line is drawn through the same history it describes. Studies of real-time forecasting find valuation ratios do noticeably worse out of sample (Goyal and Welch, 2008), and overlapping periods flatter R\u00b2 at long horizons (Boudoukh, Richardson and Whitelaw, 2008).")
        else tags$li("This is an in-sample fit: the line is drawn through the same history it describes, and overlapping periods flatter R\u00b2 at long horizons (Boudoukh, Richardson and Whitelaw, 2008)."),
        if(cape) tags$li("Today's CAPE is near the edge of the history, where few dots constrain the line, so the trend value there is especially uncertain.")))
  })
  outputOptions(output, "value_reg", suspendWhenHidden = FALSE)   # lives in a closed <details>
  output$value_cards <- renderUI({
    d <- value_df(); q <- value_q(); now <- value_now()
    card <- function(name, value, rate, ctx){
      div(class = "series-card",
          style = paste0("--cl:", series$colour[1], ";--cd:", series$colour_dark[1]),
          div(class = "name", span(class = "dot"), span(name)),
          div(class = "figs", span(class = "value", value), span(class = "rate", rate)),
          div(class = "ctx", ctx))
    }
    fifth <- function(i, name) card(name, sprintf("%.1f%%", 100 * q$med[i]), sprintf("a year, median of the next %d years", horizon()),
                                    sprintf("Range %s to %s across %d starting years", fmt_pct(q$worst[i], 1), fmt_pct(q$best[i], 1), q$n[i]))
    if(measure() == "past") return(div(class = "series calc",
        card(sprintf("Past %d years now", horizon()), now$value, paste("a year,", now$when_short),
             sprintf("Higher than %d%% of year-ends since %d (median %.1f%%)",
                     floor(100 * mean(now$x_all < now$x)), now$first, 100 * median(now$x_all))),
        fifth(5, sprintf("Strongest fifth: past return above %.1f%%", 100 * q$lo[5])),
        fifth(1, sprintf("Weakest fifth: past return below %.1f%%", 100 * q$hi[1]))))
    div(class = "series calc",
        card("CAPE now", now$value, now$when,
             sprintf("Higher than %d%% of year-ends since %d (median %.1f)",
                     floor(100 * mean(d$cape < now$x)), d$year[1], median(d$cape))),
        fifth(5, sprintf("Dearest fifth: CAPE above %.0f", q$lo[5])),
        fifth(1, sprintf("Cheapest fifth: CAPE below %.0f", q$hi[1])))
  })

  output$stats_note <- renderUI({
    infl <- if(real()) "after inflation" else "before inflation"
    txt <- if(identical(view(), "value")){
      s <- value_pts()
      paste0(sprintf("US shares with dividends, %s. Starting points from the end of %d to the end of %d; each bar is the median of one fifth of them. ",
                     infl, min(s$year), max(s$year)),
             "Hover over a dot for its years.")
    } else if(identical(view(), "calc")){
      sprintf("%s invested in %s shares at the end of %s, valued at the end of %s, %s.",
              fmt_amount(amount()), if(country() == "us") "US" else "Australian",
              yrs()[1], yrs()[2],
              if(real()) paste0("after inflation (", yrs()[1], " dollars)") else "before inflation")
    } else if(identical(view(), "falls")){
      sprintf("Each %s against the highest value since the end of %s, %s. Click a series to show or hide it.",
              if(monthly_falls()) "month's value" else "year-end value", yrs()[1], infl)
    } else if(identical(view(), "rolling")){
      first <- min(sapply(rolling_ctx(), `[[`, "first"))
      sprintf("Average yearly return over the %s years to %s, %s, ranked against every %s-year period since %s. Click a series to show or hide it.",
              win(), yrs()[2], if(real()) "after inflation" else "before inflation",
              win(), first)
    } else {
      sprintf("$100 invested at the end of %s, valued at the end of %s, %s. Click a series to show or hide it.",
              yrs()[1], yrs()[2],
              if(real()) paste0("after inflation (", yrs()[1], " dollars)") else "before inflation")
    }
    div(class = "stats-note", txt)
  })
  
  #### rolling: how to read ####
  output$how_to <- renderUI({
    if(identical(view(), "value") && measure() == "past"){
      d <- value_df(); pts <- value_pts(); q <- value_q(); now <- value_now(); h <- horizon()
      both <- d[!is.na(d$past), ]
      return(tags$ul(
        tags$li(sprintf("Each dot is one December: the average yearly return of US shares with dividends over the previous %d years (across) and over the next %d years (up). The bars are the median return for each fifth of the starting points, weakest past to strongest.", h, h)),
        tags$li("It asks whether a strong or weak run says anything about the next one. Choose 5, 10, 15 or 20 years above the chart; the same length is used before and after."),
        tags$li(sprintf("After the strongest fifth of past periods, the next %d years returned a median of %.1f%% a year; after the weakest fifth, %.1f%%; across all %d starting points, %.1f%%.",
                        h, 100 * q$med[5], 100 * q$med[1], nrow(pts), 100 * median(pts$fwd))),
        tags$li(sprintf("This is not a forecast. The spread is wide (%s to %s a year after the strongest fifth), and neighbouring dots share %d of their %d years before and after, so the %d dots hold only about %d separate %d-year periods.",
                        fmt_pct(q$worst[5], 1), fmt_pct(q$best[5], 1), h - 1, h, nrow(pts), round(nrow(pts) / h), h)),
        tags$li(sprintf("A strong run usually leaves shares dearer: across year ends, the past %d-year return and CAPE have a correlation of %.2f. Switch the starting point to CAPE to compare valuation directly.",
                        h, cor(both$past, both$cape))),
        tags$li(sprintf("Today's point is the %d years to the end of %d: %s, higher than %d%% of year ends since %d. ",
                        h, max_yr, now$label, floor(100 * mean(now$x_all < now$x)), now$first),
                sprintf("The five starting points closest to it (highlighted) were followed by %s.",
                        paste(sprintf("%s a year from %d", fmt_pct(now$near$fwd, 1), now$near$year), collapse = ", "))),
        tags$li("Returns: the same US series as the other tabs.")))
    }
    if(identical(view(), "value")){
      d <- value_df(); q <- value_q(); now <- cape_now
      near <- d[!is.na(d$fwd) & d$cape >= 0.8 * now$cape, ]
      return(tags$ul(
        tags$li("CAPE (the cyclically adjusted price-to-earnings ratio, or Shiller P/E) is the share price divided by the average of the last 10 years of company earnings, after inflation. Averaging over a business cycle smooths out booms and recessions, so it measures how expensive shares are against their normal earning power."),
        tags$li(sprintf("Each dot is one December: the CAPE then (across) and the average yearly return of US shares with dividends over the next %d years (up). The bars are the median return for each fifth of the starting points, cheapest to dearest.", horizon())),
        tags$li("Choose 5, 10, 15 or 20 years above the chart. Over 5 years the link with CAPE is loose, since markets can stay cheap or dear for years; over longer periods it is usually clearer, but there are fewer dots."),
        tags$li(sprintf("Dearer starts have usually been followed by lower returns: a median of %.1f%% a year from the dearest fifth against %.1f%% from the cheapest.",
                        100 * q$med[5], 100 * q$med[1])),
        tags$li(sprintf("This is not a forecast. The spread at any CAPE is wide (%s to %s a year from the dearest fifth), neighbouring dots share %d of their %d years so the %d dots hold only about %d separate %d-year periods, and CAPE has stayed above its long-run median for most of the last 30 years.",
                        fmt_pct(q$worst[5], 1), fmt_pct(q$best[5], 1), horizon() - 1, horizon(), sum(q$n), round(sum(q$n) / horizon()), horizon())),
        tags$li(sprintf("Today's CAPE is the latest month in Shiller's data (%s); the dots are year ends, and only years with %d full years after them have a dot.", now$when, horizon())),
        tags$li(if(nrow(near)) sprintf("Today's %.1f is near the top of the range. The only starting points within 20%% of it are %s.",
                                       now$cape, paste(sprintf("%d (%s a year)", near$year, fmt_pct(near$fwd, 1)), collapse = ", "))
                else sprintf("Today's %.1f is above every starting point with 10 years of results.", now$cape)),
        tags$li("CAPE and earnings: Robert J. Shiller, Online Data. Returns: the same US series as the other tabs.")))
    }
    if(identical(view(), "calc")) return(tags$ul(
      tags$li("Set the amount and the years in the sentence above the chart, or drag the handles under it. The money goes in at the end of the first year."),
      tags$li("Dividends reinvested follows the total-return index: each dividend buys more shares, which then pay dividends of their own. That snowball is the compounding."),
      tags$li("Dividends taken as cash holds the same shares and adds up the dividends they paid, kept as cash that earns nothing. The gap between the two is what reinvesting added."),
      tags$li("A year's dividend is the difference between the total return and the price return, applied to the shares' value at the start of that year."),
      tags$li("No tax, fees or franking credits are included. With inflation on, every amount is in first-year dollars."),
      # opens the About window, then scrolls to the method once it has loaded
      tags$li(HTML(paste0("The full method, with a worked example, is in ",
              tags$a(href = "#", onclick = "$(document).one('shown.bs.modal', function(){ var s = document.getElementById('dividend-method'); if(s) s.scrollIntoView({block: 'start'}); }); document.getElementById('about').click(); return false;",
                     "About the data"), ".")))))
    if(identical(view(), "falls")) return(tags$ul(
      tags$li("Each point shows how far the value sat below its highest value so far. Zero means a new high."),
      if(monthly_falls())
        tags$li("US values are monthly: each month's average of daily closes (Shiller's data). Falls that recover within a year, such as 1987 and 2020, show, but averaging hides the single worst days: in 2020 US shares fell 34% from the 19 February close to the 23 March close, about 19% month to month."),
      if(monthly_falls())
        tags$li("So the shares + dividends card gives the worst fall at daily closes from July 1926 (the whole US market with dividends, from the Kenneth R. French Data Library), marked \u201cdaily\u201d on the chart. Before 1926, and for price only, it uses the monthly values.")
      else
        tags$li("Australian values are at year end, so falls and rebounds within a year (such as October 1987) don't show in full. Monthly history isn't available for Australia; the US version of this chart is monthly."),
      tags$li("The cards give the worst fall in the selected years, the years of the peak and the low, and when the earlier peak was regained."),
      tags$li(if(monthly_falls()) "Bonds earn the 10-year yield each month, so before inflation they never fall. Turn on inflation to see losses of buying power."
              else "Bonds use each December's 10-year yield as that year's return, so before inflation they never fall. Turn on inflation to see losses of buying power.")))
    k <- as.numeric(win()); y1 <- yrs()[2]
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
  # the chart's heading and subheading (also the start of its screen-reader label)
  heading <- reactive({
    real_txt <- if(real()) ", after inflation" else ""
    first_roll <- function() min(sapply(rolling_ctx(), `[[`, "first"))
    switch(view(),
           value = if(measure() == "past")
                     list(paste0("Past returns and what came next", real_txt),
                          sprintf("The average yearly return of US shares over the %d years before each year end, and over the %d years after.", horizon(), horizon()))
                   else list(paste0("Valuation and what came next", real_txt),
                             sprintf("How expensive US shares were at each year end (CAPE), and the average yearly return over the next %d years.", horizon())),
           calc = list(paste0("What ", fmt_amount(amount()), " became, dividends reinvested or taken as cash", real_txt),
                       "The lower shading is the dividends paid out; the upper shading is what reinvesting them added."),
           growth = list(paste0("Growth of $100 invested in ", yrs()[1], real_txt),
                         "Log scale: each gridline is ten times the one below, so equal slopes mean equal growth rates."),
           annual = list(paste0("Calendar-year returns", real_txt),
                         if(stacked()) "Return in each year, one row per series on a shared scale. The line in each row is the steady yearly rate that gives the same result over the period."
                         else "Return in each year."),
           falls = list(paste0("Falls from the previous high", real_txt),
                        if(stacked() && length(shown()) > 1) "How far each series sat below its highest value so far. The line in each row marks its worst fall."
                        else "How far each series sat below its highest value so far."),
           rolling = list(paste0("Rolling ", win(), "-year returns", real_txt),
                          paste0("Average yearly return over the ", win(), " years ending in each year. ",
                                 if(stacked() && length(shown()) > 1) sprintf("Each row shades where 80%% of that series' %s-year periods since %s landed.", win(), first_roll())
                                 else if(length(shown()) > 1) "Show a single series, or switch to stacked rows, to see the typical range."
                                 else sprintf("The shaded band is where 80%% of all %s-year periods since %s landed.", win(), first_roll()))))
  })
  output$chart_title <- renderUI({
    h <- heading()
    div(class = "chart-title", tags$h2(h[[1]]), tags$p(h[[2]]))
  })

  # a text version of the chart for screen readers: the heading and the
  # figures on the cards (the chart div is role="img")
  chart_label <- reactive({
    figs <- switch(view(),
      calc = {
        p <- calc_df(); n <- nrow(p)
        sprintf("Dividends reinvested: %s. Dividends taken as cash: %s, of which %s in dividends.",
                fmt_dollars(p$reinvested[n]), fmt_dollars(p$shares[n] + p$cash[n]), fmt_dollars(p$cash[n]))
      },
      value = if(measure() == "past"){
        q <- value_q(); now <- value_now()
        sprintf("Past %d years now: %s (%s). Median yearly return over the next %d years: %.1f%% after the strongest fifth of past periods, %.1f%% after the weakest.",
                horizon(), now$label, now$when, horizon(), 100 * q$med[5], 100 * q$med[1])
      } else {
        q <- value_q()
        sprintf("CAPE now %.1f (%s). Median yearly return over the next %d years: %.1f%% from the dearest fifth of starting points, %.1f%% from the cheapest.",
                cape_now$cape, cape_now$when, horizon(), 100 * q$med[5], 100 * q$med[1])
      },
      {
        f <- card_figs(); k <- intersect(series$key, shown())
        paste0(paste(sprintf("%s: %s %s", series$label[match(k, series$key)],
                             vapply(f[k], `[[`, "", "value"), vapply(f[k], `[[`, "", "rate")),
                     collapse = ". "), ".")
      })
    h <- heading()
    when <- if(identical(view(), "value")) "" else paste0(", ", yrs()[1], " to ", yrs()[2])   # Valuation has no years
    paste0(h[[1]], when, ". ", figs)
  })
  observe(session$sendCustomMessage("chart-label", list(id = "chart", label = chart_label())))

  # the light figure plus the changes for dark (themed_figure, R/figures.R):
  # the browser applies the theme, so the theme is not part of the cache key
  output$chart <- renderFigure({
    yrs <- yrs()
    keep <- series$market[series$key %in% shown()]
    tall <- (identical(view(), "annual") && stacked()) ||
            (view() %in% c("rolling", "falls") && stacked() && length(keep) > 1)
    themed_figure(tall = tall, function(theme) switch(view(),
           calc = dividend_plot(calc_df(), yrs, narrow = isTRUE(input$narrow), theme = theme),
           value = value_plot(value_pts(), value_q(), value_now(), value_now()$x_all, horizon(),
                              fit = if(isTRUE(input$fit)) value_reg(), narrow = isTRUE(input$narrow), theme = theme),
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
                       theme = theme)
           },
           annual = {
             df <- returns_df() %>% filter(market %in% keep)
             if(stacked()){
               ref <- lapply(setNames(series$label, series$label), function(l){
                 r <- compound_rate(df$percent_yoy[df$market == series$market[series$label == l] &
                                                    df$year > yrs[1]])
                 if(is.na(r)) NULL else list(y = r, text = sprintf("%.1f%% a year", 100 * r))
               })
               stacked_plot(df, "percent_yoy", yrs, type = "bar", ref = ref, theme = theme)
             } else {
               line_plot(df, y_col = "percent_yoy", x_range = yrs,
                         y_format = ".1%", tick_format = ".0%", hline = 0, theme = theme)
             }
           },
           falls = {
             df <- falls_df() %>% filter(market %in% keep)
             fig <- if(stacked() && length(keep) > 1){
               ref <- lapply(setNames(series$key, series$label), function(k){
                 f <- falls_stats()[[k]]
                 if(f$worst == 0) NULL
                 else list(y = f$worst, text = paste0("worst ", fmt_pct(f$worst),
                                                      if(isTRUE(f$exact) && f$low >= daily_start) "<br>at daily closes"))
               })
               stacked_plot(df, "dd", yrs, type = "area", ref = ref, zero_label = "New high",
                            theme = theme)
             } else {
               lo <- min(c(df$dd, -0.01), na.rm = TRUE)
               line_plot(df, y_col = "dd", x_range = yrs, y_format = ".1%", tick_format = ".0%",
                         y_range = c(lo * 1.04, -lo * 0.07),    # headroom above 0 for the note
                         hline = 0, zero_label = "New high", theme = theme)
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
                            type = "line", bands = bands, theme = theme)
             } else {
             band <- NULL
             if(length(keep) == 1){
               i <- match(keep, series$market); c <- rolling_ctx()[[series$key[i]]]
               band <- list(lo = c$lo, hi = c$hi, med = c$med,
                            colour = series_colours(theme)[[series$label[i]]])
             }
             line_plot(rolling_df() %>% filter(market %in% keep),
                       y_col = "percent",
                       x_range = yrs,
                       y_format = ".1%",
                       tick_format = ".0%",
                       hline = 0,
                       band = band,
                       theme = theme)
             }
           }))
  }) %>% bindCache(country(), view(), real(), yrs(), shown(), stacked(),
                   if(identical(view(), "rolling")) win(),
                   if(identical(view(), "value")) list(horizon(), isTRUE(input$fit), measure()),
                   if(identical(view(), "calc")) amount(),
                   if(view() %in% c("growth", "calc", "value")) isTRUE(input$narrow))
  
  #### download: the data behind the current chart ####
  output$download <- downloadHandler(
    filename = function(){
      if(view() == "value") return(paste0(if(measure() == "past") "us-past-vs-next-" else "us-cape-next-",
                                          horizon(), "-years", if(real()) "-real", ".csv"))
      paste0(if(country() == "us") "us" else "australian", "-market-", view(),
             if(view() == "rolling") paste0("-", win(), "yr"),
             if(view() == "calc") paste0("-", format(amount(), scientific = FALSE)),
             "-", yrs()[1], "-", yrs()[2],
             if(real()) "-real", ".csv")
    },
    content = function(file){
      if(identical(view(), "value")){
        d <- value_df()
        out <- data.frame(year = d$year, cape = round(d$cape, 2), past = round(d$past, 4), fwd = round(d$fwd, 4))
        names(out)[3:4] <- sprintf(c("previous_%dyr_return", "next_%dyr_return"), horizon())
        return(write.csv(out, file, row.names = FALSE, na = ""))
      }
      if(identical(view(), "calc")){
        p <- calc_df()
        return(write.csv(data.frame(year = p$year,
                                    dividends_reinvested = round(p$reinvested, 2),
                                    shares_dividends_taken = round(p$shares, 2),
                                    dividends_paid_to_date = round(p$cash, 2)),
                         file, row.names = FALSE))
      }
      df <- switch(view(),
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
        { if(identical(view(), "falls") && monthly_falls()) rename(., month = year) else . } %>%
        write.csv(file, row.names = FALSE, na = "")
    })

  #### bookmarks: the address bar always holds a link to this view ####
  # (store = "url": the state lives in the link, nothing is kept on the
  # server). Left out: the theme (the visitor's own setting), phone width,
  # buttons, the calculator's year boxes (they follow the slider), and each
  # series checkbox on its own (input$show holds them).
  setBookmarkExclude(c("mode", "narrow", "about", "calc_from", "calc_to", paste0("show_", series$key)))
  bookmark_state <- reactive(reactiveValuesToList(input)) %>% debounce(500)
  observe({ bookmark_state(); session$doBookmark() })
  onBookmarked(function(url) updateQueryString(url, mode = "replace"))

  #### about ####
  observeEvent(input$about, {
    showModal(modalDialog(includeHTML("about.Rhtml"),
                          title = "About the data", size = "l",
                          easyClose = TRUE, footer = modalButton("Close")))
  })
}


# Run the application ##########################################################
shinyApp(ui = ui, server = server, enableBookmarking = "url")
