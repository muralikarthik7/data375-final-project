## ----setup, include = FALSE---------------------------------------------------
library(fintrackr)
library(ggplot2)

## -----------------------------------------------------------------------------
finance_data <- data.frame(
  date = as.Date(c("2026-01-01", "2026-01-15", "2026-02-01", "2026-02-10")),
  category = c("Salary", "Groceries", "Salary", "Rent"),
  income = c(3000, 0, 3000, 0),
  expense = c(0, 200, 0, 1200)
)
finance_data

## -----------------------------------------------------------------------------
budget_out <- budget_summary(finance_data, income, expense, category, date)
budget_out$category_summary
budget_out$totals
budget_out$monthly_summary

## -----------------------------------------------------------------------------
invest_out <- calc_returns(principal = 1000, rate = 0.05, time = 5, compound_freq = 12)
invest_out$future_value
head(invest_out$growth_table)

## -----------------------------------------------------------------------------
debt_out <- debt_projection(balance = 5000, annual_rate = 0.18, monthly_payment = 200)
head(debt_out)
tail(debt_out)

## -----------------------------------------------------------------------------
plot_financial_trends(budget_out$monthly_summary, month, net)

