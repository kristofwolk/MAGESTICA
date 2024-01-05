dds_MAGESTICA <- function(counts, coldata) {
  DESeq2::DESeqDataSetFromMatrix(countData = counts,
                         colData = coldata,
                         design = ~ replicate + timepoint)
}
