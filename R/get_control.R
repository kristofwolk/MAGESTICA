get_control <- function(df, control) {
  indices_control <- base::which(rownames(df) %in% control)
  df[indices_control,]
}
