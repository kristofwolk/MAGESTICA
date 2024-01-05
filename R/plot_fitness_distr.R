plot_fitness_distr <- function(res, control, thresholds = c(0,0)) {

  res <- base::data.frame(res)
  res$strain <- "edited"

  indices_control <- get_indices_control(res, control)
  res[indices_control,]$strain <- "WT"

  res$strain <- base::as.factor(res$strain) %>% stats::relevel("edited")

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = dplyr::filter(res, strain == "WT"),
                            ggplot2::aes(x=log2FoldChange, fill = "WT"), binwidth = 0.2, alpha = 0.7) +
    ggplot2::geom_histogram(data = dplyr::filter(res, strain == "edited"),
                            ggplot2::aes(x=log2FoldChange, fill = "edited"), binwidth= 0.2, alpha = 0.3) +
    ggplot2::scale_colour_manual(name = "strain", values = c("WT" = "blue", "edited" = "red")) +
    ggplot2::scale_fill_manual(name = "strain", values = c("WT" = "blue", "edited" = "red")) +
    ggplot2::ylab("# barcodes") +
    ggplot2::scale_y_continuous(trans = "sqrt") +
    ggplot2::theme_bw() +
    ggplot2::xlim(c(-2,2))


  plot_with_lines <- plot +
    ggplot2::geom_vline(xintercept = thresholds, linetype = "dashed", color = c("black", "black"), size = 1.2)

  plot_with_lines
}
