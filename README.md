# fintrackr

**fintrackr** is an R package for personal finance analysis. It provides tools for budgeting, investment growth, debt repayment, strategy comparison, and financial visualization.

## Features

* **Budget Summary**: Summarize income, expenses, and net balance by category and month
* **Investment Growth**: Calculate compound returns and track growth over time
* **Debt Projection**: Generate repayment schedules and track remaining balance
* **Strategy Comparison**: Compare multiple financial strategies
* **Visualization**: Plot financial trends using `ggplot2`

## Installation

```r
# Install from local source
devtools::install("fintrackr")
```

## Example Usage

```r
library(fintrackr)

finance_data <- data.frame(
  date = as.Date(c("2026-01-01", "2026-01-15")),
  category = c("Salary", "Food"),
  income = c(1000, 0),
  expense = c(0, 200)
)

budget_summary(finance_data, income, expense, category, date)
```

## Authors

Murali Karthik Ganji
Deeksha Reddy
Wesley Bowers

## License

MIT License
