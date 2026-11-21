#######################################################
# Update the Australian annual series (data/australianstockmarket.RDS/.csv)
# with every complete year that is missing.
#
# Each year, around February-March:
#   1. In a browser, export the S&P/ASX All Ordinaries from S&P Dow Jones
#      Indices (https://www.spglobal.com/spdji/en/indices/equity/all-ordinaries/):
#      the price return and the total return series, covering at least the
#      whole of the December just ended. S&P blocks automated downloads, so
#      this is the one manual step.
#   2. Run from the project root:
#        Rscript code/update.R <price_return.xls> <total_return.xls>
#      (defaults: data/ord.xls and data/ord_tr.xls)
#
# What it does
# - Shares: December average of the daily index (the method of Brailsford et
#   al. used from 2006). December averages are kept in
#   data/au_december_index.csv, so each update only needs the latest December.
# - Bonds: RBA table F2, the last December observation of the Australian
#   Government 10-year bond yield (downloaded automatically).
# - Inflation: ABS monthly CPI (All groups, Australia), December to December
#   (downloaded automatically). Years before 2025 used the December quarter.
# - Checks that the stored December averages still reproduce the shares
#   returns already in the dataset before adding anything.
# - Then rebuilds the US series, fully automatic, since the app shows only the
#   years both markets cover: annual (code/us_data.R), monthly for the falls
#   chart and CAPE (code/us_monthly.R), daily for the worst falls
#   (code/us_daily.R).
#######################################################
suppressMessages(library(dplyr))

args <- commandArgs(TRUE)
price_file <- if(length(args) >= 1) args[1] else "data/ord.xls"
tr_file    <- if(length(args) >= 2) args[2] else "data/ord_tr.xls"

data_file <- "data/australianstockmarket.RDS"
dec_file  <- "data/au_december_index.csv"

dl <- function(url, name){
  f <- file.path(tempdir(), name)
  download.file(url, f, mode = "wb", quiet = TRUE)
  f
}

#### shares: December averages from S&P exports ####
# S&P exports have a few header rows, then Excel serial dates and levels
read_sp <- function(f){
  x <- suppressMessages(readxl::read_excel(f, col_names = FALSE, col_types = "text"))
  s <- suppressWarnings(as.numeric(x[[1]])); v <- suppressWarnings(as.numeric(x[[2]]))
  ok <- !is.na(s) & !is.na(v) & s > 30000
  data.frame(date = as.Date(s[ok], origin = "1899-12-30"), index = v[ok])
}
dec_avg <- function(d){
  d %>% filter(format(date, "%m") == "12") %>%
    group_by(year = as.integer(format(date, "%Y"))) %>%
    summarise(avg = mean(index), days = n(), .groups = "drop") %>%
    filter(days >= 15)                           # a whole December, not a partial one
}
new_dec <- full_join(dec_avg(read_sp(price_file)) %>% select(year, price = avg),
                     dec_avg(read_sp(tr_file))    %>% select(year, total_return = avg),
                     by = "year")
old_dec <- if(file.exists(dec_file)) read.csv(dec_file) else new_dec[0, ]
december <- bind_rows(new_dec, old_dec %>% filter(!year %in% new_dec$year)) %>%
  arrange(year) %>% filter(!is.na(price), !is.na(total_return))

#### bonds: RBA F2 ####
f <- dl("https://www.rba.gov.au/statistics/tables/xls/f02d.xlsx", "f02d.xlsx")
x <- suppressMessages(readxl::read_excel(f, col_names = FALSE, col_types = "text"))
col <- which(unlist(x[2, ]) == "Australian Government 10 year bond")
stopifnot(length(col) == 1)
s <- suppressWarnings(as.numeric(x[[1]])); v <- suppressWarnings(as.numeric(x[[col]]))
bonds <- data.frame(date = as.Date(s, origin = "1899-12-30"), yield = v) %>%
  filter(!is.na(date), !is.na(yield), format(date, "%m") == "12") %>%
  group_by(year = as.integer(format(date, "%Y"))) %>%
  slice_max(date, n = 1) %>% ungroup() %>%
  transmute(year, bonds = yield / 100)

#### inflation: ABS monthly CPI ####
page <- paste(readLines("https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release",
                        warn = FALSE), collapse = "")
link <- regmatches(page, regexpr("/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/[a-z0-9-]+/640101\\.xlsx", page))
stopifnot(length(link) == 1)
x <- suppressMessages(readxl::read_excel(dl(paste0("https://www.abs.gov.au", link), "640101.xlsx"),
                                         sheet = "Data1", col_names = FALSE, col_types = "text"))
col <- which(grepl("All groups CPI ;  Australia", unlist(x[1, ])) & unlist(x[4, ]) == "INDEX")[1]
s <- suppressWarnings(as.numeric(x[[1]]))
cpi <- data.frame(date = as.Date(s, origin = "1899-12-30"), index = suppressWarnings(as.numeric(x[[col]]))) %>%
  filter(!is.na(date), !is.na(index), format(date, "%m") == "12") %>%
  transmute(year = as.integer(format(date, "%Y")), index) %>%
  arrange(year) %>%
  mutate(inflation = ifelse(year - lag(year) == 1, index / lag(index) - 1, NA)) %>%
  filter(!is.na(inflation))

#### combine ####
au <- readRDS(data_file)
returns <- december %>%
  arrange(year) %>%
  mutate(stock_accumulation = round(total_return / lag(total_return) - 1, 3),
         stock_price        = round(price / lag(price) - 1, 3),
         gap = year - lag(year)) %>%
  filter(gap == 1)

# the stored December averages must reproduce the shares returns already published
check <- inner_join(returns, au, by = "year", suffix = c("", "_published"))
bad <- check %>% filter(abs(stock_accumulation - stock_accumulation_published) > 0.0015 |
                          abs(stock_price - stock_price_published) > 0.0015)
cat("Checked", nrow(check), "published years against the S&P December averages:",
    if(nrow(bad)) "MISMATCH" else "all match", "\n")
if(nrow(bad)){ print(bad); stop("December averages do not reproduce the published returns; not updating.") }

new <- returns %>%
  filter(year > max(au$year)) %>%
  select(year, stock_accumulation, stock_price) %>%
  inner_join(bonds, by = "year") %>%
  inner_join(cpi %>% transmute(year, inflation = round(inflation, 3)), by = "year")

write.csv(december, dec_file, row.names = FALSE)
if(!nrow(new)){
  cat("No new complete year (shares, bonds and CPI all needed). Latest:", max(au$year), "\n")
} else {
  au <- bind_rows(au, as.data.frame(new)) %>% arrange(year)
  stopifnot(all(diff(au$year) == 1), !anyNA(au))
  saveRDS(au, data_file)
  write.csv(au, "data/australianstockmarket.csv", eol = "\r\n")   # file has Windows line endings
  cat("Added", paste(new$year, collapse = ", "), "\n"); print(new)
}

#### United States: rebuilt automatically from Shiller and Damodaran ####
# The app shows the years both markets cover, so update the US series too.
us_ok <- tryCatch({ source("code/us_data.R", local = new.env()); source("code/us_monthly.R", local = new.env()); source("code/us_daily.R", local = new.env()); TRUE },
                  error = function(e){ cat("US update failed, US data left as it was:", conditionMessage(e), "\n"); FALSE })
au_last <- max(readRDS(data_file)$year); us_last <- max(readRDS("data/usstockmarket.RDS")$year)
cat(sprintf("Latest year: Australia %d, United States %d. The app shows up to %d.\n",
            au_last, us_last, min(au_last, us_last)))
if(us_ok && us_last < au_last)
  cat("The US sources do not have", au_last, "yet; run this again in a few weeks.\n")
