#' Get Subset corresponding to Control Barcodes
#'
#' @description Given a list of the control barcodes, returns the control barcodes in the data frame.
#'
#' @param df A data frame with barcodes as the rownames e.g. count matrix or results table.
#' @param control A vector containing the names of the control barcodes.
#'
#' @return A subset of the data frame containing only the control features.
#' @export
#'
#' @examples data(counts_MAGESTICA, control_bcs)
#' counts_MAGESTICA_control <- get_control(counts_MAGESTICA, control_bcs)
get_control <- function(df, control) {
  indices_control <- base::which(rownames(df) %in% control)
  df[indices_control,]
}
