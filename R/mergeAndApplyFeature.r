#' Merge session files into one data frame
#'
#' Merge session files into a data frame and calculate each feature by the step segment of that session.
#'
#' @param pathOfTheFolder The full path of the session.
#' @param feature Which feature to calculate, should be passed as a string
#' @return data frame containing the step segments and with a prefix of the feature in the column names.
#'
#'
mergeAndApplyFeature <- function(pathOfTheFolder, feature){
  `%nin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
  filenames <- list.files(path=pathOfTheFolder, full.names=TRUE)
  stepsCycle <- filenames[basename(filenames) %in% c("StepCycles.csv")]
  filenames <- filenames[basename(filenames) %nin% c("StepCycles.csv")]
  datalist <- lapply(filenames, function(x){read.csv(file=x,header=T)})
  datalist <- Reduce(function(x,y) {merge(x,y)}, datalist)
  results2 <- data.frame("ax"= numeric(0), "ay"= numeric(0), "az"= numeric(0), "roll1"= numeric(0), "roll2"= numeric(0), "pitch"= numeric(0), "yaw1"= numeric(0), "yaw2"= numeric(0), fx= numeric(0), fy= numeric(0), fz= numeric(0), gx= numeric(0), gy= numeric(0), gz= numeric(0), mx= numeric(0), my= numeric(0), mz= numeric(0), stringsAsFactors = FALSE)
  stepsCycleData <- read.table(file=stepsCycle, header = T)
  for (i in 1:(length(stepsCycleData$timestamp)-1)){
    start <- which(datalist$timestamp %in% (stepsCycleData[i,]))[1]
    end <- which(datalist$timestamp %in% (stepsCycleData[i+1,]))[1]

    data = datalist[start:end,]
    data = data[-1,] #remove the first row
    data = data[-nrow(data),] #remove the last row
    data$timestamp <- NULL # remove timestamps
    aggregated_data <- apply(data, 2, feature) # apply the feature to the dataframe
    results <- rbind(results, setNames(aggregated_data, names(data)))
  }

  for (j in 1:nrow(results)){
    results2[nrow(results2)+1,] <- results[j,]
  }

    colnames(results2) <- paste(feature, colnames(results2), sep = "_") # add prefix to the column names
    return(results2)
}

