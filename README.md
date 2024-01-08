# Analysis of MAGESTIC data using the MAGESTICA Package 

This R Markdown document contains a quick walkthrough of the functionalities of the R package MAGESTICA.

### To install and load the MAGESTICA package, we can use following code:

```{r}
devtools::install_github('kristofwolk/MAGESTICA')
library(MAGESTICA)
```

### Next, we need to load in the data. This should be: 

  - A count matrix corresponding to the experimental design of MAGESTIC. The rownames should correspond to the barcodes, columns to the sample names, and a subset of the barcodes should correspond to an internal control (WT strains). Ideally, counts are already filtered for low quality barcodes/ low abundance at timepoint 1. COUNTS SHOULD NOT BE NORMALIZED YET.
  
  - A dataframe containing the meta-information of the experiment. This dataframe should have in the rows, same sample names as the columns of the count matrix and contain at least a column timepoint denoting from which timepoint the sample comes and a column replicate, denoting the replicate. 
  
  - A vector containing the names of the control barcodes.
  
  - (Optional) Annotation files with as rownames the barcodes. We can add this information to the results tables.


#### The MAGESTICA package also provides some example data we can use for now:

```{r}
cts <- counts_MAGESTIC
meta <- metadata_MAGESTIC
ctrl_bcs <- control_bcs
```

### The optimized workflow is summarized into one function called 'MAGESTICA'. 

#### For more information about this function, run:

```{r,message=FALSE}
?MAGESTICA
```

### Default MAGESTICA analysis:

```{r}
MAGESTICA_output <- MAGESTICA(cts, meta, ctrl_bcs)
```

### Additional to the analysis workflow, MAGESTICA provides for some diagnostic plots. These are:

#### A volcano plot where statistical significance (represented as -log10 of the FDR-adjusted p-value) is plotted against the LFC for every barcode. Plot also visualizes the thresholds that were calculated from the control group.


```{r}
volcano_plot(MAGESTICA_output$results, thresholds = MAGESTICA_output$min.fitness.thresholds)
```


#### An MA plot where LFC is plotted against the mean abundance for every barcode. Again, plot visualizes the thresholds that were calculated from the control group.


```{r}
MA_plot(MAGESTICA_output$results, thresholds = MAGESTICA_output$min.fitness.thresholds)
```

#### A plot visualizing the trajectories of the replicates over time for a given barcode.

```{r}
bc <- rownames(MAGESTICA_output$results[which.min(MAGESTICA_output$results$padj),]) # barcode with lowest adjusted p-value
plot_counts(MAGESTICA_output$norm.counts, metadata_MAGESTIC, bc = bc) + ggplot2::ggtitle(bc)
```

### Leveraging all timepoints, we can construct a table for which we can prioritize for monotonicity (or other fitness trends).

```{r}
df <- df_consecutive_LFCs(MAGESTICA_output$dds.MAGESTICA)
df <- add_monotonicity(df)
```

#### Filter for monotonicity

```{r}
df_filtered1 <- df[abs(df$dir) > 1,]
df_filtered2 <- df[abs(df$dir) > 3,]
 
dim(df)[1]; dim(df_filtered1)[1]; dim(df_filtered2)[1]
```

