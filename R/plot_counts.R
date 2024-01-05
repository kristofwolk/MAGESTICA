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
