#######################################################
# Build the US monthly series used by the "Falls & recoveries" tab
# (data/usmonthly.RDS): month-end-of-month indices from December 1882.
#
# Source: Robert J. Shiller, "Online Data" (ie_data.xls), https://shillerdata.com/
#   P    S&P Composite price, the monthly average of daily closes
#   D    dividends, annual rate
#   CPI  consumer prices
#   GS10 10-year government bond yield
#
# Indices (all 100 in December 1882)
# - price: P
# - tr:    price with dividends reinvested, each month (P + D/12) / previous P
#          (Shiller's own total-return method). Recent months without a
#          dividend figure carry the last one forward.
# - bonds: the previous month's 10-year yield earned for the month (yield/12),
#          the monthly version of the annual series' yield-as-return
# - cpi:   consumer prices, to deflate the others
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
url <- if(length(link)) paste0("https:", link) else "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
download.file(url, f, mode = "wb", quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0"))

x <- readxl::read_excel(f, sheet = "Data", skip = 7, col_types = "text", .name_repair = "unique_quiet")
m <- tibble(date = num(x$Date), P = num(x$P), D = num(x$D), CPI = num(x$CPI), GS10 = num(x$`Rate GS10`)) %>%
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
         cpi   = 100 * CPI / CPI[1]) %>%
  select(year, month, price, tr, bonds, cpi) %>%
  as.data.frame()

saveRDS(monthly, "data/usmonthly.RDS")
cat(sprintf("Wrote data/usmonthly.RDS: %d-%02d to %d-%02d\n",
            monthly$year[1], monthly$month[1], tail(monthly$year, 1), tail(monthly$month, 1)))
