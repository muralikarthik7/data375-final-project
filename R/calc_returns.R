#' Calculate investment growth with periodic contributions
#'
#' Computes future value and a growth table for an investment with
#' regular contributions (SIP model).
#' 
#' @importFrom utils tail
#'
#' @param principal Initial investment amount.
#' @param rate Annual interest rate as a decimal.
#' @param time Number of years.
#' @param compound_freq Number of compounding periods per year (default = 1).
#' @param contribution Amount added each period.
#'
#' @return A list containing the final investment value (future_value) and a data frame (growth_table) showing growth over time.
#' @export
calc_returns <- function(principal, rate, time, compound_freq = 1, contribution = 0) {
  
  total_periods <- time * compound_freq
  period_rate <- rate / compound_freq
  
  values <- numeric(total_periods + 1)
  values[1] <- principal
  
  for (t in 2:(total_periods + 1)) {
    values[t] <- values[t - 1] * (1 + period_rate) + contribution
  }
  
  growth_table <- data.frame(
    period = 0:total_periods,
    year = (0:total_periods) / compound_freq,
    value = values
  )
  
  final_value <- tail(values, 1)
  
  list(
    future_value = final_value,
    growth_table = growth_table
  )
}
