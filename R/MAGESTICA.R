MAGESTICA <- function(counts, coldata, alpha = 0.1, quantile_range = c(0.1,0.9), control = NULL, norm_method = "GMPR", pseudo_count = 1) {
  # Check for missing values
  # If missing values are found in the count matrix, they are set to zero
  # If missing values are found in the coldata, We give an error
  if(base::any(base::is.na(counts))) {
    base::message('NAs detected in count matrix, setting them to 0')
    counts[base::is.na(counts)] <- 0
  }

  if(base::any(base::is.na(coldata))) {
    stop('NAs detected in coldata, please check out coldata before proceeding with analysis!')
  }

  # Check if coldata has timepoint and replicate column
  if(!("timepoint" %in% base::colnames(coldata))) {
    stop('Coldata does not contain a column \'timepoint\', please provide valid coldata!')
  }

  if(!("replicate" %in% base::colnames(coldata))) {
    stop('Coldata does not contain a column \'replicate\', please provide valid coldata!')
  }

  # Check if thresholds add up to 1 if not c(0,0)
  if(!base::setequal(quantile_range, c(0,0))) {
    if (quantile_range[1] + quantile_range[2] != 1) {
      stop("Quantile values do not add up to 1. Please provide an appropriate quantile range (e.g. c(0.1, 0.9)).")
    }
  }

  # Check if alpha has a valid value
  if (!(alpha > 0 && alpha < 1)) {
    stop("Invalid alpha. Please provide a value between 0 and 1.")
  }

  # Check if control barcodes are provided
  if (base::is.null(control)) {
    stop("No control barcodes provided, please provide a list of control barcodes\n")
  }

  if (!base::is.numeric(pseudo_count)) {
    stop("Pseudo-count is no numeric value, please provide a numeric value.")
  }

  # Convert to data frame (if that was not already the case) and add pseudo-counts
  counts = base::data.frame(counts + pseudo_count)

  # Factorize coldata
  for (i in 1:base::dim(coldata)[2]) {
    coldata[,i] <- base::factor(coldata[,i])
  }


  # POTENTIALLY ADD: INSPECTION OF MISSING VALUES AND REMOVAL OF SAMPLES SHOULD THIS BE THE CASE

  # Add type of barcode (variant or WT) to counts and split by type
  counts2 <- counts
  counts2$type <- "variant"
  indices_control <- base::which(base::rownames(counts2) %in% control)
  counts2[indices_control,]$type <- "WT"

  counts_split <- base::split(counts, f = counts2$type)
  counts_WT <- counts_split$WT
  counts_var <- counts_split$variant


  # Create DESeqDataSets
  dds_WT <- DESeq2::DESeqDataSetFromMatrix(countData = counts_WT,
                                           colData = coldata,
                                           design = ~ replicate + timepoint)

  dds_var <- DESeq2::DESeqDataSetFromMatrix(countData = counts_var,
                                            colData = coldata,
                                            design = ~ replicate + timepoint)

  # GMPR or MR normalization leveraging WT subset
  if(norm_method == "GMPR") {
    # Function GMPR (From https://github.com/jchen1981/GMPR/blob/master/GMPR.R)
    GMPR <- function (comm, intersect.no = 10, ct.min = 1, trace = TRUE) {
      # Computes the GMPR size factor
      #
      # Args:
      #   comm: a matrix of counts, row - features (OTUs, genes, etc) , column - sample
      #   intersect.no: the minimum number of shared features between sample pair, where the ratio is calculated
      #   ct.min: the minimum number of counts required to calculate ratios

      #
      # Returns:
      #   a vector of the size factors with attribute 'NSS'. Samples with distinct sets of features will be output as NA.
      #         NSS:   number of samples with significant sharing (> intersect.no) including itself

      # mask counts < ct.min
      comm[comm < ct.min] <- 0

      if (is.null(colnames(comm))) {
        colnames(comm) <- base::paste0('S', 1:base::ncol(comm))
      }

      if (trace) base::cat('Begin GMPR size factor calculation ...\n')

      comm.no <- base::numeric(ncol(comm))
      gmpr <- base::sapply(1:base::ncol(comm),  function(i) {
        if (i %% 50 == 0) {
          base::cat(i, '\n')
        }
        x <- comm[, i]
        # Compute the pairwise ratio
        pr <- x / comm
        # Handling of the NA, NaN, Inf
        pr[base::is.nan(pr) | !base::is.finite(pr) | pr == 0] <- NA
        # Counting the number of non-NA, NaN, Inf
        incl.no <- base::colSums(!is.na(pr))
        # Calculate the median of PR
        pr.median <- matrixStats::colMedians(pr, na.rm=TRUE)
        # Record the number of samples used for calculating the GMPR
        comm.no[i] <<- base::sum(incl.no >= intersect.no)
        # Geometric mean of PR median
        if (comm.no[i] > 1) {
          return(base::exp(base::mean(base::log(pr.median[incl.no >= intersect.no]))))
        } else {
          return(NA)
        }
      }
      )

      if (base::sum(base::is.na(gmpr))) {
        warning(base::paste0('The following samples\n ', base::paste(base::colnames(comm)[base::is.na(gmpr)], collapse='\n'),
                             '\ndo not share at least ', intersect.no, ' common taxa with the rest samples! ',
                             'For these samples, their size factors are set to be NA! \n',
                             'You may consider removing these samples since they are potentially outliers or negative controls!\n',
                             'You may also consider decreasing the minimum number of intersecting taxa and rerun the procedure!\n'))
      }

      if (trace) base::cat('Completed!\n')
      if (trace) base::cat('Please watch for the samples with limited sharing with other samples based on NSS! They may be outliers! \n')
      base::names(gmpr) <- base::names(comm.no) <- base::colnames(comm)

      #attr(gmpr, 'NSS') <- comm.no

      return(gmpr)
    }

    DESeq2::sizeFactors(dds_var) <- GMPR(base::as.matrix(counts_WT))

  } else if (norm_method == "RLE") {

    DESeq2::sizeFactors(dds_var) <- DESeq2::sizeFactors(DESeq2::estimateSizeFactors(dds_WT))

  } else {

    stop(base::paste0(norm_method, " is not a implemented normalization method. Please set parameter norm_method to either \'GMPR\' or \'RLE\'."))

  }

  # DESeq2 analysis
  dds_var <- DESeq2::DESeq(dds_var)

  q <- c(0, 0)

  res <- if (base::setequal(quantile_range, c(0,0))) {

    results_nothreshold(dds_var)

  } else {

    # Calculate thresholds based on quantile range obtained from LFC of WT subset
    dds_WT <- DESeq2::DESeq(dds_WT)
    res_WT <- results_nothreshold(dds_WT)

    q <- stats::quantile(res_WT$log2FoldChange, quantile_range)

    first_timepoint <- base::levels(dds_var$timepoint)[1]
    last_timepoint <- base::levels(dds_var$timepoint)[base::length(base::levels(dds_var$timepoint))]
    contrast <- base::paste0("timepoint_", last_timepoint, "_vs_", first_timepoint)

    res_after_cutoff_right <- DESeq2::results(dds_var, altHypothesis = "greater", lfcThreshold = q[2], alpha = alpha)
    res_before_cutoff_left <- DESeq2::results(dds_var, altHypothesis = "less", lfcThreshold = -q[1], alpha = alpha)

    res_after_cutoff_right_shrunk <- base::data.frame(DESeq2::lfcShrink(dds_var, coef = contrast, res = res_after_cutoff_right))
    res_before_cutoff_left_shrunk <- base::data.frame(DESeq2::lfcShrink(dds_var, coef = contrast, res = res_before_cutoff_left))

    res <- base::data.frame()

    for (i in 1:base::nrow(res_after_cutoff_right_shrunk)) {
      if (res_after_cutoff_right_shrunk[i,]$log2FoldChange < 0) {
        res <- base::rbind(res, res_before_cutoff_left_shrunk[i,])
      } else {
        res <- base::rbind(res, res_after_cutoff_right_shrunk[i,])
      }
    }

    res$pvalue <- res$pvalue*2; res$padj <- res$padj*2

    if(base::sum(base::is.na(res$padj))!=0) {
      res[base::is.na(res$padj),]$padj <- 1
    }

    res[res$pvalue > 1,]$pvalue <- 1; res[res$padj > 1,]$padj <- 1

    res

  }


  l <- base::list(norm.counts = counts(dds_var, normalized = T),
                  DESeqDataSet_variants = dds_var,
                  results = res,
                  size.factors = sizeFactors(dds_var),
                  min.fitness.thresholds = q)

  return(l)

}
