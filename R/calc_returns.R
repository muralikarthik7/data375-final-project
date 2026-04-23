#' Calculate compound investment growth
#'
#' Computes future value and a growth table for an investment.
#'
#' @param principal Initial investment amount.
#' @param rate Annual interest rate as a decimal.
#' @param time Number of years.
#' @param compound_freq Number of compounding periods per year. Default is 1.
#'
#' @return A list containing the future value and a growth table.
#' @export
calc_returns <- function(principal, rate, time, compound_freq = 1) {

  periods <- 0:(time * compound_freq)

  values <- principal * (1 + rate / compound_freq)^periods

  growth_table <- data.frame(
    period = periods,
    year = periods / compound_freq,
    value = values
  )

  future_value <- tail(values, 1)

  list(
    future_value = future_value,
    growth_table = growth_table
  )
}
