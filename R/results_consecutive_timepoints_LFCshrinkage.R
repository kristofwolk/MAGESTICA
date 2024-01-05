results_consecutive_timepoints_LFCshrinkage <- function(dds) {
  output <- base::list()
  for (t in 1:(base::length(base::levels(dds$timepoint))-1)) {
    res <- DESeq2::lfcShrink(dds, contrast=c('timepoint', base::levels(dds$timepoint)[t+1], base::levels(dds$timepoint)[t]), type = "ashr")
    output <- rlist::list.append(output, res)
  }
  output
}
