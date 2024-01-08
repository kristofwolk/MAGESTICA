#' Add Annotation
#'
#' @description Given the results table from MAGESTICA and an annotation file with rownames corresponding to the barcodes, returns the results table with annotation information.
#'
#' @param res Results table obtained from MAGESTICA (MAGESTICA_output$results).
#' @param annotation_file Annotation file with rownames corresponding to the barcodes.
#'
#' @return Results table with annotation information.
#' @export
#'
#' @examples
add_annotation <- function(res, annotation_file) {
  # Order res by padj
  res2 <- res[base::order(res$padj),]
  # Obtain variants corresponding to the results
  variants <- annotation_file[base::match(base::rownames(res2), rownames(annotation_file)),]
  # Add to results
  res2$variant <- variants
  return(res2)
}
