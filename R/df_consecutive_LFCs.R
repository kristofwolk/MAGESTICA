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
