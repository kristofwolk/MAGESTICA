#' Get Indices corresponding to the Control Barcodes (Helper Function)
#'
#' @description Given a list of the control barcodes, returns the indices of the control barcodes in the data frame.
#'
#' @param df A data frame with the barcodes as rownames e.g. count matrix or results table.
#' @param control A vector containing the control barcodes.
#'
#' @return A vector containing the indices corresponding to the control barcodes in given data frame.
#' @export
#'
#' @examples
get_indices_control <- function(df, control) {
  indices_control <- base::which(base::rownames(df) %in% control)
  indices_control
}
