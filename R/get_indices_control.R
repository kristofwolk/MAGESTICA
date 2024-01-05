get_indices_control <- function(df, control) {
  indices_control <- base::which(base::rownames(df) %in% control)
  indices_control
}
