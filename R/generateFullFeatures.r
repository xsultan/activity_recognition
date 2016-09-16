#' Generate a dataset with all the features
#'
#' Merge session files into a data frame and calculate each feature by the step segment of that session.
#'
#' @param steps The data frame of the steps.
#' @param stepsCycle The data frame of the steps' cycle.
#' @param step indicates the step, we used {up, down and flat} as {0,1,2} respectively.
#' @return data frame containing the step segments and with a prefix of the feature in the column names.
#'
#' @export
generateFullFeatures <- function(steps, stepsCycle, feature, step){
  y4 <- NULL
  features <- feature
  for (i in features){
    y3 <- mergeAndApplyFeatureDataFrame(steps, stepsCycle, i)
    y3$key <- rownames(y3)
    y4$key <- rownames(y3)
    y4 <- merge(y3, y4, by = "key")
  }

  y4$key <- NULL
  y4$step <- as.integer(step)

  return(y4)
}
