#######################################################
# Build the US daily total-return series used for the worst-fall figures on
# the "Falls" tab (data/usdaily.RDS).
#
# Source: Kenneth R. French Data Library, "Fama/French 3 Factors [Daily]"
#   https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#   Mkt-RF + RF is the daily return of the CRSP value-weighted market: all
#   US-listed shares (NYSE, AMEX, NASDAQ), weighted by size, dividends
#   reinvested. Daily from 1 July 1926; updated about monthly.
#
# Output: date, tr (index, 100 at the close of 30 June 1926).
#
# Run from the project root: Rscript code/us_daily.R
# (code/update.R runs it after rebuilding the annual US series.)
#######################################################
url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
dir <- file.path(tempdir(), "ff_daily"); dir.create(dir, showWarnings = FALSE)
zip <- file.path(dir, "ff_daily.zip")
download.file(url, zip, mode = "wb", quiet = TRUE)
csv <- unzip(zip, exdir = dir)
csv <- csv[grepl("\\.csv$", csv, ignore.case = TRUE)][1]

# a few lines of notes, then the header row (",Mkt-RF,SMB,HML,RF"), then
# yyyymmdd rows of percentages, then a copyright line
lines <- readLines(csv, warn = FALSE)
rows <- grep("^\\s*[0-9]{8}\\s*,", lines, value = TRUE)
x <- read.csv(text = rows, header = FALSE, strip.white = TRUE)[, c(1, 2, 5)]
names(x) <- c("date", "mkt_rf", "rf")
x$date <- as.Date(as.character(x$date), "%Y%m%d")
x <- x[order(x$date), ]

stopifnot(!anyNA(x), x$date[1] == as.Date("1926-07-01"), !any(duplicated(x$date)),
          all(abs(x$mkt_rf) < 25))           # percentages, not decimals

daily <- data.frame(date = x$date, tr = 100 * cumprod(1 + (x$mkt_rf + x$rf) / 100))
saveRDS(daily, "data/usdaily.RDS")
cat(sprintf("Wrote data/usdaily.RDS: %s to %s, %d trading days\n",
            format(min(daily$date)), format(max(daily$date)), nrow(daily)))
