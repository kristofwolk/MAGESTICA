#' Add monotonicity filter
#'
#' @description Adds statistic which we refer to as LFC direction (dir) to the data frame obtained with df_consecutive_LFCs. We can filter on this statistic for monotonicity.
#'
#' @param df Data frame obtained with function df_consecutive_LFCs.
#'
#' @return Data frame with a statistic addded (dir) on which we can filter for monotonicity.
#' @export
#'
#' @examples
add_monotonicity <- function(df) {
  df$dir <- base::rowSums(sign(df))
  df
}
