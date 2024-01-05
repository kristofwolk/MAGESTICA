add_variant <- function(res, annotation_file) {
  # Order res by padj
  res2 <- res[base::order(res$padj),]
  # Obtain variants corresponding to the results
  variants <- annotation_file[base::match(base::rownames(res2), annotation_file$bc1),]$name
  # Add to results
  res2$variant <- variants
  return(res2)
}
