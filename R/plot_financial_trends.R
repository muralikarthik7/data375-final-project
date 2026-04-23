#' Plot financial trends
#'
#' Creates a line plot for financial data and adds points so trends
#' are easier to see.
#'
#' @param data A data frame.
#' @param x Variable for x-axis.
#' @param y Variable for y-axis.
#' @param group Optional grouping variable.
#'
#' @return A ggplot object.
#' @export
plot_financial_trends <- function(data, x, y, group = NULL) {

  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

  if (!is.null(group)) {
    group <- rlang::ensym(group)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = !!group)) +
      ggplot2::geom_point(ggplot2::aes(color = !!group))
  } else {
    p <- p +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  }

  p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = rlang::as_string(x),
      y = rlang::as_string(y)
    )
}
