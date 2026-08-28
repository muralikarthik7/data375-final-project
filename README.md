# fintrackr

`fintrackr` is an R package developed for the DATA 375 final project. The package provides tools for personal finance analysis, including budgeting, transaction analysis, investment growth, debt repayment projections, and financial data visualization.

## Authors

* Murali Karthik Ganji
* Deeksha Reddy
* Wesley Bowers

## Overview

Managing personal finances often involves working with different types of financial information, such as expenses, budgets, investments, and debt. The goal of `fintrackr` is to provide a structured and user-friendly set of R tools that can help users analyze this information in one workflow.

The package allows users to summarize financial transactions, examine financial trends over time, model investment growth with regular contributions, simulate debt repayment schedules, and create visualizations that make financial information easier to interpret.

## Features

`fintrackr` supports several areas of personal finance analysis:

* **Transaction Analysis:** Summarize financial transactions and better understand spending patterns.
* **Budgeting:** Analyze expenses and compare financial activity across categories.
* **Financial Trends:** Examine how financial activity changes over time.
* **Investment Growth:** Model how investments may grow over time with regular contributions.
* **Debt Repayment:** Simulate debt repayment schedules and examine how balances change over time.
* **Data Visualization:** Create visual representations of financial data and trends using `ggplot2`.

## Installation

The package requires R version 4.1.0 or later.

The primary package dependencies are:

* `dplyr`
* `ggplot2`
* `rlang`

Development and documentation also use:

* `testthat`
* `knitr`
* `rmarkdown`

After downloading or cloning the project, the package can be installed from the project directory in R using:

```r
devtools::install()
```

The package can then be loaded with:

```r
library(fintrackr)
```

## Example Workflow

A typical `fintrackr` workflow may involve importing personal finance data, summarizing transactions, analyzing spending or financial trends, creating visualizations, and using the package's financial modeling tools to explore investments or debt repayment.

```r
library(fintrackr)

# Load or create financial data
# Analyze transactions and spending
# Visualize financial trends
# Model investment growth or debt repayment
```

See the package documentation and vignette for detailed examples of the available functions and their usage.

## Package Structure

The project follows the standard structure of an R package.

```text
fintrackr/
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── R/
├── man/
├── tests/
├── vignettes/
└── README.md
```

* `R/` contains the package's R functions.
* `man/` contains generated function documentation.
* `tests/` contains package tests using `testthat`.
* `vignettes/` contains longer examples and demonstrations of package functionality.
* `DESCRIPTION` contains package metadata and dependency information.
* `NAMESPACE` defines exported functions and imported functionality.

## Documentation and Testing

Functions in `fintrackr` are documented using `roxygen2`. Package tests are written using the `testthat` framework to verify that the implemented financial analysis functions behave as expected.

Documentation can be generated with:

```r
devtools::document()
```

Tests can be run with:

```r
devtools::test()
```

The complete package can also be checked using:

```r
devtools::check()
```

## License

`fintrackr` is released under the MIT License.

## Course Project

This package was developed as a final project for **DATA 375** at the University of Arizona.

The project demonstrates the development of a structured R package that combines data manipulation, financial calculations, visualization, documentation, and testing into a reusable personal finance analysis toolkit.
