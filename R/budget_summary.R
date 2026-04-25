#' Summarize financial data using net amounts
#'
#' This function summarizes financial records using a single amount column,
#' where positive values represent income and negative values represent
#' expenses. It computes category-wise totals, overall totals, and monthly
#' trends, and also tracks a running balance over time starting from a
#' specified opening balance.
#'
#' @param data A data frame containing financial records.
#' @param amount_col Name of the column containing net amounts
#'        (positive = income, negative = expense).
#' @param category_col Name of the category column.
#' @param date_col Name of the date column.
#' @param opening_balance Numeric value representing the starting balance
#'        (default is 0).
#'
#' @return A list with ledger, category summary, totals, and monthly summary.
#' @export
budget_summary <- function(data, amount_col, category_col, date_col, opening_balance = 0) {
  
  amount_col <- rlang::ensym(amount_col)
  category_col <- rlang::ensym(category_col)
  date_col <- rlang::ensym(date_col)
  
  data <- data |>
    dplyr::arrange(!!date_col) |>
    dplyr::mutate(
      month = format(as.Date(!!date_col), "%Y-%m"),
      balance = opening_balance + cumsum(!!amount_col)
    )
  
  category_summary <- data |>
    dplyr::group_by(!!category_col) |>
    dplyr::summarise(
      total_income = sum((!!amount_col)[!!amount_col > 0], na.rm = TRUE),
      total_expense = -sum((!!amount_col)[!!amount_col < 0], na.rm = TRUE),
      net = sum(!!amount_col, na.rm = TRUE),
      .groups = "drop"
    )
  
  totals <- data.frame(
    total_income = sum(dplyr::pull(data, !!amount_col)[dplyr::pull(data, !!amount_col) > 0], na.rm = TRUE),
    total_expense = -sum(dplyr::pull(data, !!amount_col)[dplyr::pull(data, !!amount_col) < 0], na.rm = TRUE)
  )
  totals$net_balance <- opening_balance + sum(dplyr::pull(data, !!amount_col), na.rm = TRUE)
  
  monthly_summary <- data |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      total_income = sum((!!amount_col)[!!amount_col > 0], na.rm = TRUE),
      total_expense = -sum((!!amount_col)[!!amount_col < 0], na.rm = TRUE),
      net = sum(!!amount_col, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    ledger = data,                 # 🔥 NEW (running balance)
    category_summary = category_summary,
    totals = totals,
    monthly_summary = monthly_summary
  )
}