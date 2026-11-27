# Market calculations: indices, returns, falls, the dividend calculator and
# the CAPE valuation fit. Pure functions of the data passed in.

######################################################################
#create final data frames
######################################################################
# one country's annual returns (year, stock_accumulation, stock_price, bonds,
# inflation) -> nominal and real returns and $100 indices in long format.
# Index: $100 the year before the first return, compounded by each year's return.
# Real return: (1 + r) / (1 + inflation) - 1.
prep_market <- function(raw, max_year = Inf){
  cols <- c("bonds", "inflation", "stock_accumulation", "stock_price")
  raw <- raw[raw$year <= max_year, c("year", cols)]
  raw <- raw[order(raw$year), ]
  start <- min(raw$year) - 1                 # the index starts at $100 the year before the first return
  years <- c(start, raw$year)
  nominal <- lapply(setNames(cols, cols), function(k) c(NA, raw[[k]]))
  real <- lapply(nominal[cols != "inflation"], function(r) (1 + r) / (1 + nominal$inflation) - 1)
  index <- function(r) c(100, 100 * cumprod(1 + r[-1]))
  # long format, rows by year then series (the column order of the source data)
  long <- function(vals, src_order, names, value){
    k <- length(src_order)
    out <- tibble::tibble(year = rep(years, each = k),
                          market = factor(rep(unname(names[src_order]), times = length(years)), levels = unname(names[sort(src_order)])),
                          v = as.vector(t(sapply(vals[src_order], identity))))
    names(out)[3] <- value; out
  }
  nm  <- c(bonds = "Bonds 10yr", inflation = "inflation", stock_accumulation = "Stock Price + Div", stock_price = "Stock Price")
  nmi <- c(bonds = "Bonds 10yr", inflation = "inflation_indx", stock_accumulation = "Stock Price + Div", stock_price = "Stock Price")
  src  <- c("stock_accumulation", "stock_price", "bonds", "inflation")   # column order in the source data
  srcr <- c("stock_accumulation", "stock_price", "bonds")
  list(returns_nom  = long(nominal, src, nm, "percent_yoy"),
       index_nom    = long(lapply(nominal, index), src, nmi, "dollars"),
       returns_real = long(real, srcr, nm, "percent_yoy"),
       index_real   = long(lapply(real, index), srcr, nm, "dollars"))
}

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

# rolling returns over the full history for one window length: the average
# yearly return over the no_years years ending in each year
rolling_full <- function(mk, real, no_years){
  idx <- if(real) mk$index_real else mk$index_nom
  idx %>%
    filter(market != "inflation_indx") %>%
    group_by(market) %>%
    mutate(percent = round((dollars / lag(dollars, no_years))^(1 / no_years) - 1, 4)) %>%
    ungroup() %>%
    transmute(year, market, roll_years = factor(as.character(no_years)), percent)
}

# compound yearly rate over a range (years after the first), as on the cards
compound_rate <- function(r) if(length(r)) exp(mean(log1p(r))) - 1 else NA

# worst fall from a previous high: when it peaked, bottomed and recovered
fall_stats <- function(year, dollars){
  dd <- dollars / cummax(dollars) - 1
  # a dip that rounds to 0% counts as no fall
  if(!length(dd) || min(dd) > -0.005) return(list(worst = 0, now = dd[length(dd)]))
  i <- which.min(dd)
  peak_i <- which.max(dollars[seq_len(i)])
  back <- which(dollars >= dollars[peak_i] & seq_along(dollars) > i)[1]
  list(worst = dd[i], peak = year[peak_i], low = year[i],
       back = if(is.na(back)) NA else year[back], now = dd[length(dd)])
}

#######################################################
# dividend calculator
#######################################################
# `amount` put into shares at the end of y0, followed to the end of each later
# year two ways:
#   reinvested: every dividend buys more shares (the total-return series)
#   shares:     the same shares with dividends paid out (the price series)
#   cash:       the dividends paid out, added up (not invested, no interest)
# A year's dividend is the gap between the two returns applied to the value
# of the shares at the start of that year. With `real`, every amount is in
# y0 dollars: each dividend is deflated by prices at the end of its year.
dividend_path <- function(raw, y0, y1, amount, real = FALSE){
  d <- raw[raw$year > y0 & raw$year <= y1, ]; d <- d[order(d$year), ]
  defl <- if(real) cumprod(1 + d$inflation) else rep(1, nrow(d))
  price <- amount * cumprod(1 + d$stock_price)                 # before inflation
  paid  <- c(amount, head(price, -1)) * (d$stock_accumulation - d$stock_price)
  data.frame(year       = c(y0, d$year),
             reinvested = c(amount, amount * cumprod(1 + d$stock_accumulation) / defl),
             shares     = c(amount, price / defl),
             cash       = c(0, cumsum(paid / defl)))
}

# idx: a market's $100 index (long format); cape: year, cape (Decembers).
# One row per December: CAPE then, `past` the average yearly return over
# the h years to that December, and `fwd` the one over the h years after it.
value_data <- function(idx, cape, h = value_horizon){
  idx <- idx[idx$market == "Stock Price + Div", ]; idx <- idx[order(idx$year), ]
  stopifnot(length(h) == 1, h >= 1, h < nrow(idx))
  ratio <- (idx$dollars[-(1:h)] / idx$dollars[1:(nrow(idx) - h)])^(1 / h) - 1
  merge(cape, data.frame(year = idx$year, past = c(rep(NA, h), ratio), fwd = c(ratio, rep(NA, h))), by = "year")
}

# The two ways the Valuation tab can sort starting points: what each needs
# for the fit, axis, cards and notes. `ln`: fit on the log of x (CAPE).
value_measures <- list(
  cape = list(col = "cape", ln = TRUE),
  past = list(col = "past", ln = FALSE))

# the dots for one measure: x = that measure, starting years with both x and
# a full h years after them
value_points <- function(d, measure){
  d$x <- d[[value_measures[[measure]]$col]]
  d[!is.na(d$x) & !is.na(d$fwd), ]
}

# least-squares fit of the next-h-year return on x (ln x for CAPE).
# Neighbouring starting years share h - 1 years of returns, so their errors
# are correlated and ordinary standard errors are too small: Newey-West
# (Bartlett weights, lag h - 1) standard errors allow for that.
# d: value_points(); x_now: today's value of the measure.
value_fit <- function(d, h, x_now, ln = TRUE){
  tx <- if(ln) log else identity
  d$tx <- tx(d$x)
  fit <- lm(fwd ~ tx, d)
  X <- model.matrix(fit); e <- resid(fit); n <- nrow(X); B <- solve(crossprod(X))
  S <- crossprod(X * e)
  for(l in seq_len(h - 1)){
    G <- crossprod(X[(l + 1):n, , drop = FALSE] * e[(l + 1):n], X[1:(n - l), , drop = FALSE] * e[1:(n - l)])
    S <- S + (1 - l / h) * (G + t(G))
  }
  V <- B %*% S %*% B
  b <- coef(fit); se_nw <- sqrt(diag(V)); sig <- summary(fit)$sigma
  lim <- range(d$x, x_now)
  grid <- if(ln) exp(seq(log(lim[1]), log(lim[2]), length.out = 80)) else seq(lim[1], lim[2], length.out = 80)
  Gm <- cbind(1, tx(grid)); se_g <- sqrt(rowSums((Gm %*% V) * Gm))
  x0 <- c(1, tx(x_now)); f0 <- sum(x0 * b); se0 <- sqrt(drop(t(x0) %*% V %*% x0))
  list(n = n, h = h, ln = ln, years = range(d$year), b = b, se_ols = sqrt(diag(vcov(fit))), se_nw = se_nw,
       r2 = summary(fit)$r.squared, r2_inv = if(ln) summary(lm(fwd ~ I(1 / x), d))$r.squared, sigma = sig,
       predict = function(x) sum(b * c(1, tx(x))),       # at one x
       grid = grid, yhat = drop(Gm %*% b), lo = drop(Gm %*% b) - 1.96 * se_g, hi = drop(Gm %*% b) + 1.96 * se_g,
       x_now = x_now, f0 = f0, ci0 = f0 + c(-1, 1) * 1.96 * se0, pi0 = f0 + c(-1, 1) * 1.96 * sqrt(se0^2 + sig^2))
}

# fifths of the starting points (value_points()), lowest x to highest
value_fifths <- function(d){
  br <- quantile(d$x, 0:5 / 5)
  d$q <- cut(d$x, br, include.lowest = TRUE, labels = FALSE)
  do.call(rbind, lapply(1:5, function(i){
    s <- d[d$q == i, ]
    data.frame(q = i, lo = br[i], hi = br[i + 1], n = nrow(s), med = median(s$fwd),
               worst = min(s$fwd), best = max(s$fwd))
  }))
}

#### CAPE over time ####
# m: the US monthly data (year, month, cape). Labelled turning points: the
# highest or lowest CAPE inside each window of years (searched, not typed in,
# so they follow revisions to Shiller's data).
cape_episodes <- function(m){
  w <- data.frame(from = c(1915, 1925, 1930, 1960, 1975, 1990, 2008),
                  to   = c(1925, 1931, 1935, 1970, 1985, 2005, 2010),
                  kind = c("low", "high", "low", "high", "low", "high", "low"))
  t <- m$year + (m$month - 1) / 12
  do.call(rbind, lapply(seq_len(nrow(w)), function(i){
    k <- which(t >= w$from[i] & t < w$to[i])
    j <- k[if(w$kind[i] == "high") which.max(m$cape[k]) else which.min(m$cape[k])]
    data.frame(year = m$year[j], month = m$month[j], cape = m$cape[j], kind = w$kind[i])
  }))
}
# runs of consecutive months with CAPE at or above `level`: from, to (rows of m)
cape_runs <- function(m, level){
  k <- which(m$cape >= level)
  if(!length(k)) return(data.frame(from = integer(), to = integer()))
  g <- cumsum(c(1, diff(k) != 1))
  data.frame(from = tapply(k, g, min), to = tapply(k, g, max))
}
