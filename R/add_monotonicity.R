add_monotonicity <- function(df) {
  df$dir <- base::owSums(sign(df))
  df
}
