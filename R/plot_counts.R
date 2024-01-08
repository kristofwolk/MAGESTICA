#' Plot Count Trajectories
#'
#' @description Given a count matrix (raw or normalized), metadata and barcode names, returns a plot showing trajectories of the different replicates over time for the given barcode.
#'
#' @param counts A count matrix (raw or normalized) with the rows corresponding to the barcodes and the columns to the samples.
#' @param meta A table with the rows corresponding to the samples and the columns corresponding to the metadata. Columns must contain at least timepoint and replicate.
#' @param bc A barcode name
#'
#' @return A plot showing the trajectories of the different replicates over time for the given barcode.
#' @export
#'
#' @examples data(MAGESTICA_output, metadata_MAGESTIC, control_bcs)
#' bc <- rownames(MAGESTICA_output$results)[1] # First barcode of dataframe (random example)
#' plot_counts(MAGESTICA_output$norm.counts, metadata_MAGESTIC, bc = bc)
plot_counts <- function(counts, meta, bc) {
  counts <- base::data.frame(counts)
  # Get index of barcode in the count matrix
  i <- base::which(base::rownames(counts) %in% bc)
  # Create df of counts of bc together with meta information
  bc_counts <- base::t(counts[i,])
  df <- base::data.frame(bc_counts, meta)
  base::colnames(df)[1] <- "count"

  # Plot
  ggplot2::ggplot(df, aes(x = timepoint, y = count)) +
    ggplot2::geom_line(aes(group = replicate, color = replicate), size = 1.2) +
    ggplot2::theme_bw()
}
