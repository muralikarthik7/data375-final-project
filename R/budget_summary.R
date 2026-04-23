#' Summarize income and expenses by category
#'
#' This function summarizes financial records and computes totals
#' and trends over time.
#'
#' @param data A data frame containing financial records.
#' @param income_col Name of the income column.
#' @param expense_col Name of the expense column.
#' @param category_col Name of the category column.
#' @param date_col Name of the date column.
#'
#' @return A list with category summary, totals, and monthly summary.
#' @export
#'
budget_summary <- function(data, income_col, expense_col, category_col, date_col) {

  income_col <- rlang::ensym(income_col)
  expense_col <- rlang::ensym(expense_col)
  category_col <- rlang::ensym(category_col)
  date_col <- rlang::ensym(date_col)

  data <- data |>
    dplyr::mutate(month = format(as.Date(!!date_col), "%Y-%m"))

  category_summary <- data |>
    dplyr::group_by(!!category_col) |>
    dplyr::summarise(
      total_income = sum(!!income_col, na.rm = TRUE),
      total_expense = sum(!!expense_col, na.rm = TRUE),
      net = total_income - total_expense,
      .groups = "drop"
    )

  totals <- data.frame(
    total_income = sum(dplyr::pull(data, !!income_col), na.rm = TRUE),
    total_expense = sum(dplyr::pull(data, !!expense_col), na.rm = TRUE)
  )
  totals$net_balance <- totals$total_income - totals$total_expense

  monthly_summary <- data |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      total_income = sum(!!income_col, na.rm = TRUE),
      total_expense = sum(!!expense_col, na.rm = TRUE),
      net = total_income - total_expense,
      .groups = "drop"
    )

  list(
    category_summary = category_summary,
    totals = totals,
    monthly_summary = monthly_summary
  )
}
