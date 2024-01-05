results_first_vs_other_timepoints_LFCshrinkage <- function(dds) {
  output <- base::list()
  for (t in 2:base::length(base::levels(dds$timepoint))) {
    res <- DESeq2::lfcShrink(dds, coef=paste0("timepoint_", base::levels(dds$timepoint)[t], "_vs_", base::levels(dds$timepoint)[1]))
    output <- rlist::list.append(output, res)
  }
  output
}
