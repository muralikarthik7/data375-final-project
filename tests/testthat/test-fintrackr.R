library(testthat)

test_that("budget_summary computes totals correctly", {
  df <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02")),
    category = c("Salary", "Food"),
    income = c(1000, 0),
    expense = c(0, 200)
  )

  out <- budget_summary(df, income, expense, category, date)

  expect_equal(out$totals$total_income, 1000)
  expect_equal(out$totals$total_expense, 200)
  expect_equal(out$totals$net_balance, 800)
})

test_that("calc_returns returns correct future value", {
  out <- calc_returns(1000, 0.05, 1, 1)
  expect_equal(round(out$future_value, 2), 1050.00)
})

test_that("debt_projection reduces balance to zero", {
  out <- debt_projection(1000, 0.12, 100)
  expect_true(tail(out$remaining_balance, 1) <= 1e-8)
})
