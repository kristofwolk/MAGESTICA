#' MA-Plot
#'
#' @description Generates an MA-plot from the results of MAGESTICA. Thresholds indicate the minimum fitness effect against which was tested.
#'
#' @param res Results table obtained from MAGESTICA (MAGESTICA_output$results).
#' @param alpha Significance cutoff (by default 0.1). If the adjusted p-value cutoff (FDR) used was a value other than 0.1, alpha should be set to that value.
#' @param thresholds Thresholds against which testing was done, calculated in MAGESTICA (MAGESTICA_output$min.fitness.thresholds).
#'
#' @return An MA-plot indicating thresholds against which was tested.
#' @export
#'
#' @examples data(MAGESTIC_output, control_bcs)
#' MA_plot(MAGESTICA_output$results, thresholds = MAGESTICA_output$min.fitness.thresholds)
MA_plot <- function(res, alpha = 0.1, thresholds = c(0,0)) {

  df_res <- base::data.frame(res)

  df_res$fitness <- "Baseline"
  # if log2Foldchange > biggest LFC.threshold and padj < alfa, set as "UP"
  df_res$fitness[df_res$log2FoldChange > thresholds[2] & df_res$padj < alpha] <- "High"
  # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
  df_res$fitness[df_res$log2FoldChange < thresholds[1] & df_res$padj < alpha] <- "Low"

  ggplot2::ggplot(data=df_res, ggplot2::aes(x=log(baseMean), y=log2FoldChange, col=fitness)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept=thresholds[1], col="black") +
    ggplot2::geom_hline(yintercept=thresholds[2], col="black") +
    #ggplot2::geom_hline(yintercept=0, col="black") +
    ggplot2::scale_color_manual(values=c("Low" = "#0073C2FF", "Baseline" = "grey", "High" = "#CD534CFF"))
}
