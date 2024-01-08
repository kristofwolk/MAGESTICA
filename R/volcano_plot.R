#' Volcano Plot
#'
#' @description Generates a volcano plot from the results of MAGESTICA. Thresholds indicate the minimum fitness effect against which was tested.
#'
#' @param res Results table obtained from MAGESTICA (MAGESTICA_output$results).
#' @param alpha Significance cutoff (by default 0.1). If the adjusted p-value cutoff (FDR) used was a value other than 0.1, alpha should be set to that value.
#' @param thresholds Thresholds against which testing was done, calculated in MAGESTICA (MAGESTICA_output$min.fitness.thresholds).
#'
#' @return A volcano plot indicating thresholds against which were tested.
#' @export
#'
#' @examples data(MAGESTIC_output, control_bcs)
#' volcano_plot(MAGESTICA_output$results, thresholds = MAGESTICA_output$min.fitness.thresholds)
volcano_plot <- function(res, alpha = 0.1, thresholds = c(0,0)) {
  df_res <- base::data.frame(res)

  # Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)
  # add a column of NAs
  df_res$fitness <- "Baseline"
  # if log2Foldchange > biggest LFC.threshold and padj < alfa, set as "UP"
  df_res$fitness[df_res$log2FoldChange > thresholds[2] & df_res$padj < alpha] <- "High"
  # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
  df_res$fitness[df_res$log2FoldChange < thresholds[1] & df_res$padj < alpha] <- "Low"

  ggplot2::ggplot(data=df_res, ggplot2::aes(x=log2FoldChange, y=-log10(padj), col=fitness)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::theme_bw() +
    #ggplot2::ylim(0,300) +
    ggplot2::geom_vline(xintercept=thresholds[1], col="black") + # vertical lines for log2FoldChange left threshold
    ggplot2::geom_vline(xintercept=thresholds[2], col="black") + # vertical lines for log2FoldChange right threshold
    ggplot2::geom_hline(yintercept=-log10(alpha), col="black") +  # horizontal line for the p-value threshold
    ggplot2::scale_color_manual(values=c("Low" = "#0073C2FF", "Baseline" = "grey", "High" = "#CD534CFF")) # Change point color. Tt is assigned to the categories in an alphabetical order so DOWN, NO, UP
}
