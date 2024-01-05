results_furthest_timepoints_LFCshrinkage <- function(dds) {
  first_timepoint <- base::levels(dds$timepoint)[1]
  last_timepoint <- base::levels(dds$timepoint)[base::length(base::levels(dds$timepoint))]
  contrast <- base::paste0("timepoint_", last_timepoint, "_vs_", first_timepoint)
  DESeq2::lfcShrink(dds, coef = contrast)
}
