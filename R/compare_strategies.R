#' Compare multiple financial strategies
#'
#' Takes a data frame of financial strategies and computes final values
#' for each scenario based on compound growth.
#'
#' @param strategies_df Data frame containing strategy details:
#' strategy_name, initial, rate, compound_freq, time
#'
#' @return A data frame comparing final values for each strategy.
#' @export
compare_strategies <- function(strategies_df) {

  results <- data.frame()

  for (i in 1:nrow(strategies_df)) {
    row <- strategies_df[i, ]

    final_value <- row$initial * (1 + row$rate / row$compound_freq)^(row$compound_freq * row$time)

    results <- rbind(results, data.frame(
      strategy = row$strategy_name,
      final_value = final_value
    ))
  }

  return(results)
}
