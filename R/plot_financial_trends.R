plot_financial_trends <- function(data, date_col, value_col, group_col = NULL) {
  library(ggplot2)
  
  p <- ggplot(data, aes_string(x = date_col, y = value_col))
  
  if (!is.null(group_col)) {
    p <- p + geom_line(aes_string(color = group_col))
  } else {
    p <- p + geom_line(color = "blue")
  }
  
  p <- p +
    labs(
      title = "Financial Trends Over Time",
      x = "Date",
      y = "Value"
    ) +
    theme_minimal()
  
  return(p)
}