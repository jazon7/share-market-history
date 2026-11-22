#######################################################
# Build the US annual series (data/usstockmarket.RDS and .csv) in the same
# format as the Australian data: year, stock_accumulation, stock_price,
# bonds, inflation (calendar-year returns as decimals).
#
# Sources
# - Robert J. Shiller, "Online Data" (ie_data.xls): monthly S&P Composite
#   price, dividends, CPI and 10-year government bond yield, 1871-.
#   https://shillerdata.com/
# - Aswath Damodaran, "Historical Returns on Stocks, Bonds and Bills",
#   NYU Stern (histretSP.xls): year-end S&P 500 level and total return, 1928-.
#   https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histret.html
#
# Method
# - Shares, 1883-1927: Shiller. December price to December price; total
#   return compounds each month's price change plus 1/12 of the annual
#   dividend rate (Shiller's own total-return method). Shiller's prices are
#   monthly averages of daily closes.
# - Shares, 1928 on: Damodaran. Year-end S&P 500 level (price only) and the
#   S&P 500 return including dividends.
# - Bonds: December 10-year government bond yield (Shiller), used as the
#   year's return, the same treatment as the Australian bonds series.
# - Inflation: December-to-December CPI (Shiller; BLS CPI-U from 1913).
#
# Run from the project root: Rscript code/us_data.R
# The raw spreadsheets are downloaded to a temp folder and not kept.
#######################################################
suppressMessages(library(dplyr))

dl <- function(url, name){
  f <- file.path(tempdir(), name)
  download.file(url, f, mode = "wb", quiet = TRUE,
                headers = c("User-Agent" = "Mozilla/5.0"))
  f
}
num <- function(v) suppressWarnings(as.numeric(v))

#### Shiller: monthly -> annual ####
# shillerdata.com links the current file
home <- paste(readLines("https://shillerdata.com/", warn = FALSE), collapse = "")
link <- regmatches(home, regexpr("//img1\\.wsimg\\.com/[^\"' ]*ie_data\\.xls[^\"' ]*", home))
# (no fallback: the old Yale copy is served over plain HTTP only, so it
# could be altered in transit, and it is no longer kept up to date)
if(length(link) != 1) stop("Could not find the ie_data.xls link on shillerdata.com; the page may have changed.")
shiller_url <- paste0("https:", link)
x <- readxl::read_excel(dl(shiller_url, "ie_data.xls"), sheet = "Data", skip = 7,
                        col_types = "text", .name_repair = "unique_quiet")
m <- tibble(date = num(x$Date), P = num(x$P), D = num(x$D), CPI = num(x$CPI),
            GS10 = num(x$`Rate GS10`)) %>%
  filter(!is.na(date)) %>%
  mutate(year = floor(date), month = round((date - year) * 100)) %>%
  arrange(year, month)
stopifnot(all(m$month %in% 1:12))

shiller <- m %>%
  mutate(tr_factor = (P + D / 12) / lag(P)) %>%
  group_by(year) %>%
  summarise(complete = n() == 12 && !anyNA(tr_factor),
            tr = prod(tr_factor) - 1,
            P_dec = P[month == 12][1], CPI_dec = CPI[month == 12][1],
            GS10_dec = GS10[month == 12][1], .groups = "drop") %>%
  filter(complete) %>%
  transmute(year,
            stock_accumulation = tr,
            stock_price = P_dec / lag(P_dec) - 1,
            bonds = GS10_dec / 100,
            inflation = CPI_dec / lag(CPI_dec) - 1)

#### Damodaran: year-end S&P 500, 1928 on ####
f <- dl("https://pages.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls", "histretSP.xls")
raw <- readxl::read_excel(f, sheet = "S&P 500 & Raw Data", col_types = "text")
damodaran <- tibble(year = num(raw$Year), level = num(raw$`S&P 500`), div = num(raw$Dividends)) %>%
  filter(!is.na(year), !is.na(level)) %>%
  arrange(year) %>%
  transmute(year,
            stock_accumulation = (level + div) / lag(level) - 1,
            stock_price = level / lag(level) - 1) %>%
  filter(year >= 1928)

#### splice ####
us <- shiller %>%
  filter(year >= 1883) %>%
  left_join(damodaran, by = "year", suffix = c("", "_dam")) %>%
  mutate(stock_accumulation = ifelse(year >= 1928, stock_accumulation_dam, stock_accumulation),
         stock_price        = ifelse(year >= 1928, stock_price_dam, stock_price)) %>%
  select(year, stock_accumulation, stock_price, bonds, inflation) %>%
  filter(!is.na(stock_accumulation), !is.na(stock_price)) %>%
  as.data.frame()

stopifnot(!anyNA(us), all(diff(us$year) == 1), min(us$year) == 1883)
saveRDS(us, "data/usstockmarket.RDS")
write.csv(us, "data/usstockmarket.csv", row.names = FALSE)
cat("Wrote data/usstockmarket.RDS:", min(us$year), "-", max(us$year), "\n")
