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

test_that("compare_strategies works", {
  df <- data.frame(
    strategy_name = c("A"),
    initial = c(1000),
    rate = c(0.05),
    compound_freq = c(1),
    time = c(1)
  )

  out <- compare_strategies(df)

  expect_true("final_value" %in% names(out))
})

test_that("plot_financial_trends returns ggplot", {
  df <- data.frame(
    month = c("2026-01", "2026-02"),
    net = c(100, 200)
  )

  p <- plot_financial_trends(df, month, net)

  expect_s3_class(p, "ggplot")
})
