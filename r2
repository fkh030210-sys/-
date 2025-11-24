backtest_strategy <- function(returns, signals) {
  # Both 'returns' and 'signals' should be aligned and numeric
  if (length(returns) != length(signals)) {
    stop("returns and signals must have the same length")
  }
  
  strat_ret <- returns * signals
  strat_ret[is.na(strat_ret)] <- 0
  
  # Convert to xts for PerformanceAnalytics
  library(xts)
  dates <- attr(returns, "names")  # or pass dates separately
  if (is.null(dates)) {
    stop("Please provide named returns or adapt this function to take dates separately.")
  }
  strat_xts <- xts::xts(strat_ret, order.by = as.Date(dates))
  
  return(strat_xts)
}
