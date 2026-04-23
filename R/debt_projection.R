#' Create a debt repayment projection
#'
#' Generates an amortization-style table for debt repayment using
#' a fixed monthly payment.
#'
#' @param balance Starting debt balance.
#' @param annual_rate Annual interest rate as a decimal.
#' @param monthly_payment Fixed monthly payment amount.
#'
#' @return A data frame showing payment progress over time.
#' @export
debt_projection <- function(balance, annual_rate, monthly_payment) {

  if (monthly_payment <= balance * (annual_rate / 12)) {
    stop("Monthly payment is too low to reduce the debt.")
  }

  month <- 0

  schedule <- data.frame(
    month = integer(),
    payment = numeric(),
    interest_paid = numeric(),
    principal_paid = numeric(),
    remaining_balance = numeric()
  )

  while (balance > 0) {
    month <- month + 1

    interest_paid <- balance * (annual_rate / 12)
    principal_paid <- monthly_payment - interest_paid

    if (principal_paid > balance) {
      principal_paid <- balance
      monthly_payment_actual <- principal_paid + interest_paid
    } else {
      monthly_payment_actual <- monthly_payment
    }

    balance <- balance - principal_paid

    schedule <- rbind(schedule, data.frame(
      month = month,
      payment = monthly_payment_actual,
      interest_paid = interest_paid,
      principal_paid = principal_paid,
      remaining_balance = balance
    ))
  }

  schedule
}
