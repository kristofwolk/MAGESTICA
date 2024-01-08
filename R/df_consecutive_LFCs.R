#' Create Table of Trajectory of LFCs
#'
#' @description Given a DESeqDataSet from the output of MAGESTICA, return a table containing the LFCs over consecutive timepoints (T1vsT2, T2vsT3, ...) for every barcode.
#'
#' @param dds DESeqDataSet corresponding to MAGESTICA pipeline (MAGESTICA_output$dds.MAGESTICA).
#'
#' @return Table containing the LFCs over consecutive timepoints (T1vsT2, T2vsT3, ...) for every barcode.
#' @export
#'
#' @examples data(MAGESTICA_output)
#' df <- df_consecutive_LFCs(MAGESTICA_output$dds.MAGESTICA)
df_consecutive_LFCs <- function(dds) {
  list_results <- base::list()
  for (t in 1:(base::length(base::levels(dds$timepoint))-1)) {
    res <- DESeq2::lfcShrink(dds, contrast=c('timepoint', base::levels(dds$timepoint)[t+1], base::levels(dds$timepoint)[t]), type = "ashr")
    list_results <- rlist::list.append(list_results, res)
  }

  df <- base::data.frame(row.names = base::rownames(list_results[[1]]))
  for (i in base::seq_along(list_results)) {
    df <- base::cbind(df, list_results[[i]]$log2FoldChange)
    base::colnames(df)[i] <- base::paste0("T",i,"vsT",i+1)
  }

  df
}
