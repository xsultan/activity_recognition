#' Plot the hierarchical Clustering
#'
#' Plot the hierarchical Clustering using sperman correlation of the highly correlated columns.
#'
#' @param dataSource the data frame to process.
#' @param threshold the threshold of the line of the highly correlated columns - default = 0.5
#' @return a plot containg a hierarchical presentation of the highly correlated values
#' @importFrom Hmisc varclus
#' @importFrom reshape2 melt
#' @export
hierarchicalClustering_Spearman <- function(dataSource, threshold=0.5){
  vclus = varclus(step~., data=dataSource)
  plot(vclus)
  abline(h = 1-threshold, col="red") # draw a line to visualize the threshold

  spearman_values = vclus$sim
  spearman_values[lower.tri(spearman_values, diag=TRUE)] = NA
  subset(melt(spearman_values, na.rm = TRUE), value > threshold)
}
