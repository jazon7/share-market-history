#######################################################
# Build the US monthly series used by the "Falls & recoveries" tab
# (data/usmonthly.RDS): month-end-of-month indices from December 1882.
#
# Source: Robert J. Shiller, "Online Data" (ie_data.xls), https://shillerdata.com/
#   P    S&P Composite price, the monthly average of daily closes
#   D    dividends, annual rate
#   CPI  consumer prices
#   GS10 10-year government bond yield
#   CAPE cyclically adjusted P/E: price over the 10-year average of
#        inflation-adjusted earnings (used by the "Valuation" tab)
#
# Indices (all 100 in December 1882)
# - price: P
# - tr:    price with dividends reinvested, each month (P + D/12) / previous P
#          (Shiller's own total-return method). Recent months without a
#          dividend figure carry the last one forward.
# - bonds: the previous month's 10-year yield earned for the month (yield/12),
#          the monthly version of the annual series' yield-as-return
# - cpi:   consumer prices, to deflate the others
# - cape:  Shiller's CAPE as published (blank where he has none)
#
# Monthly averages catch most of a fall that recovers within a year (1987,
# 2020), but not the single worst day.
#
# Run from the project root: Rscript code/us_monthly.R
# (code/update.R runs it after rebuilding the annual US series.)
#######################################################
suppressMessages(library(dplyr))

num <- function(v) suppressWarnings(as.numeric(v))
f <- file.path(tempdir(), "ie_data_monthly.xls")
home <- paste(readLines("https://shillerdata.com/", warn = FALSE), collapse = "")
link <- regmatches(home, regexpr("//img1\\.wsimg\\.com/[^\"' ]*ie_data\\.xls[^\"' ]*", home))
# (no fallback: the old Yale copy is served over plain HTTP only, so it
# could be altered in transit, and it is no longer kept up to date)
if(length(link) != 1) stop("Could not find the ie_data.xls link on shillerdata.com; the page may have changed.")
url <- paste0("https:", link)
download.file(url, f, mode = "wb", quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0"))

x <- readxl::read_excel(f, sheet = "Data", skip = 7, col_types = "text", .name_repair = "unique_quiet")
m <- tibble(date = num(x$Date), P = num(x$P), D = num(x$D), CPI = num(x$CPI), GS10 = num(x$`Rate GS10`),
            CAPE = num(x$CAPE)) %>%
  filter(!is.na(date), !is.na(P), !is.na(CPI)) %>%
  mutate(year = as.integer(floor(date)), month = as.integer(round((date - year) * 100))) %>%
  arrange(year, month) %>%
  tidyr::fill(D, GS10) %>%
  filter(year * 100 + month >= 188212)

stopifnot(all(m$month %in% 1:12), !anyNA(m[c("P", "D", "GS10")]),
          all(diff(m$year * 12 + m$month) == 1), m$year[1] == 1882, m$month[1] == 12)

growth <- function(f){ f[1] <- 1; 100 * cumprod(f) }
monthly <- m %>%
  mutate(price = 100 * P / P[1],
         tr    = growth((P + D / 12) / lag(P)),
         bonds = growth(1 + lag(GS10) / 1200),
         cpi   = 100 * CPI / CPI[1],
         cape  = CAPE) %>%
  select(year, month, price, tr, bonds, cpi, cape) %>%
  as.data.frame()

#### checks before anything is written ####
# The monthly GitHub Action commits this file and the app redeploys from it,
# so a broken or altered download must stop here. Limits are well outside
# anything in the history since 1882 (monthly price moves -26% to +50%,
# CPI -7% to +7%, CAPE 4.8 to 44, GS10 0.6% to 15%).
chg <- function(v) v[-1] / v[-length(v)] - 1
cape_ok <- m$CAPE[!is.na(m$CAPE)]
last <- as.Date(sprintf("%d-%02d-01", tail(m$year, 1), tail(m$month, 1)))
problems <- c(
  "monthly price change outside -40% to +60%" = any(chg(m$P) < -0.40 | chg(m$P) > 0.60),
  "monthly CPI change outside -10% to +10%"   = any(abs(chg(m$CPI)) > 0.10),
  "CAPE outside 3 to 80"                      = !length(cape_ok) || any(cape_ok < 3 | cape_ok > 80),
  "10-year yield outside 0% to 20%"           = any(m$GS10 < 0 | m$GS10 > 20),
  "dividend not positive"                     = any(m$D <= 0),
  "latest month more than 8 months old"       = last < Sys.Date() - 245)
if(any(problems)) stop("Shiller data failed checks, data/usmonthly.RDS not written: ",
                       paste(names(problems)[problems], collapse = "; "))

# History should not move: Shiller revises only recent months. Anything more
# than two years back that changes by over 1% (2% for the total-return
# index, which compounds every dividend) stops the update. To accept a
# genuine revision, run with ALLOW_HISTORY_CHANGE=1.
if(file.exists("data/usmonthly.RDS") && Sys.getenv("ALLOW_HISTORY_CHANGE") != "1"){
  old <- readRDS("data/usmonthly.RDS")
  cut <- (tail(old$year, 1) - 2) * 100 + tail(old$month, 1)
  both <- merge(old[old$year * 100 + old$month <= cut, ], monthly, by = c("year", "month"))
  moved <- c(price = max(abs(both$price.y / both$price.x - 1)),
             cpi   = max(abs(both$cpi.y / both$cpi.x - 1)),
             bonds = max(abs(both$bonds.y / both$bonds.x - 1)),
             tr    = max(abs(both$tr.y / both$tr.x - 1)) / 2)
  if(nrow(both) < nrow(old[old$year * 100 + old$month <= cut, ]) || any(moved > 0.01))
    stop(sprintf("Shiller's history changed (largest moves: %s), data/usmonthly.RDS not written. ",
                 paste(sprintf("%s %.2f%%", names(moved), 100 * moved * c(1, 1, 1, 2)), collapse = ", ")),
         "If the revision is genuine, rerun with ALLOW_HISTORY_CHANGE=1.")
}

saveRDS(monthly, "data/usmonthly.RDS")
cat(sprintf("Wrote data/usmonthly.RDS: %d-%02d to %d-%02d\n",
            monthly$year[1], monthly$month[1], tail(monthly$year, 1), tail(monthly$month, 1)))
