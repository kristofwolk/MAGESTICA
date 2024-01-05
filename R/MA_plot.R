MA_plot <- function(res, alpha = 0.1, thresholds = c(0,0)) {
  df_res <- base::data.frame(res)

  df_res$fitness <- "Baseline"
  # if log2Foldchange > biggest LFC.threshold and padj < alfa, set as "UP"
  df_res$fitness[df_res$log2FoldChange > thresholds[2] & df_res$padj < alpha] <- "High"
  # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
  df_res$fitness[df_res$log2FoldChange < thresholds[1] & df_res$padj < alpha] <- "Low"

  ggplot2::ggplot(data=df_res, aes(x=log(baseMean), y=log2FoldChange, col=fitness)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept=thresholds[1], col="black") +
    ggplot2::geom_hline(yintercept=thresholds[2], col="black") +
    #ggplot2::geom_hline(yintercept=0, col="black") +
    ggplot2::scale_color_manual(values=c("Low" = "#0073C2FF", "Baseline" = "grey", "High" = "#CD534CFF"))
}
