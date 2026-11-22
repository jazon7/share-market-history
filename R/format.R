# Number and date formatting for cards, labels and ticks.

# "−45%" with a real minus sign
fmt_pct <- function(x, digits = 0) sub("^-", "−", sprintf(paste0("%.", digits, "f%%"), 100 * x))

# translucent version of a hex colour, for area fills
fade <- function(hex, a){ v <- col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }

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
# dollar axis ticks: $100, $1k, $2.5M, $12B (plotly's own formats give "$1000"
# next to "$10k", or "G" for billions), with enough of them on short ranges
fmt_tick <- function(v) vapply(v, function(x){
  k <- findInterval(abs(x), c(1e3, 1e6, 1e9))             # 0: units, 1: k, 2: M, 3: B
  paste0("$", trimws(formatC(x / 1000^k, format = "fg", digits = 3)), c("", "k", "M", "B")[k + 1])
}, "")

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

# 1997, 1998, 1999, 2000, 2020 -> "1997\u20132000, 2020"
year_runs <- function(y){
  y <- sort(y); grp <- cumsum(c(1, diff(y) != 1))
  paste(tapply(y, grp, function(r) if(length(r) > 1) paste0(r[1], "\u2013", r[length(r)]) else as.character(r)), collapse = ", ")
}
