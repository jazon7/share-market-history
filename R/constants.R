# Shared constants: theme, series, chart inks, market events, sources.
# (Files in R/ are sourced by Shiny before app.R, so top-level code here
# uses namespaced calls rather than the packages app.R attaches.)

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#0b0b0b",
  primary = "#1f1f1d",      # controls stay neutral ink; colour is reserved for the data
  base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
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

countries <- c(au = "Australia", us = "United States")
calc_start <- 2000           # the calculator opens on calc_start to the latest year
value_horizon <- 10          # default; the Valuation tab offers 5, 10, 15 or 20 years

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
  list("Kenneth R. French Data Library", "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"),
  list("Yardeni Research (bear markets)", "https://old.yardeni.com/wp-content/uploads/BullBearTables.pdf")))
