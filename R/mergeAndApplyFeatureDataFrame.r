#' Merge session files into one data frame - using data frame
#'
#' Merge session files into a data frame and calculate each feature by the step segment of that session.
#'
#' @param dataSource the data frame to process.
#' @param stepsCycle the step cycle data frame
#' @param feature Which feature to calculate, should be passed as a string
#' @return data frame containing the step segments and with a prefix of the feature in the column names.
#'
#' @export
mergeAndApplyFeatureDataFrame <- function(dataSource, stepsCycle, feature){
  rms <- function(num) sqrt(sum(num^2)/length(num));
  `%nin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
  datalist <- dataSource
  results2 <- data.frame("ax"= numeric(0), "ay"= numeric(0), "az"= numeric(0), "roll1"= numeric(0), "roll2"= numeric(0), "pitch"= numeric(0), "yaw1"= numeric(0), "yaw2"= numeric(0), fx= numeric(0), fy= numeric(0), fz= numeric(0), gx= numeric(0), gy= numeric(0), gz= numeric(0), mx= numeric(0), my= numeric(0), mz= numeric(0), stringsAsFactors = FALSE)
  results <- data.frame() # empty data frame to dump the segmants on
  for (i in 1:(nrow(stepsCycle)-1)){
    data <- dataSource[which(dataSource$timestamp >= (stepsCycle[i,]) & dataSource$timestamp <= (stepsCycle[i+1,])),]
    data[is.na(data)] <- 0
    data = data[-1,] #remove the first row
    data = data[-nrow(data),] #remove the last row
    data$timestamp <- NULL # remove timestamps
    data$step <- NULL # remove steps
    #aggregated_data <- sapply(data, feature, na.rm = TRUE), 2) # apply the feature to the dataframe
    #aggregated_data <- round(apply(data, 2, feature), 2)
    aggregated_data <- apply(data, 2, feature)
    results <- rbind(results, setNames(aggregated_data, names(data))) # TODO: Verify the output of this
  }

  #print(nrow(results))

  for (j in 1:nrow(results)){
    results2[nrow(results2)+1,] <- results[j,]
  }

  colnames(results2) <- paste(feature, colnames(results2), sep = "_") # add prefix to the column names
  return(results2)
}
