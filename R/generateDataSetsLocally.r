#' Generate dataset containing the steps data - from the local files of the package
#'
#' Generate dataset containing the steps data
#'
#' @param folder path to the source of the raw data.
#' @param feature Which feature to calculate, should be passed as a string or a vector of strings.
#' @param step indicates the step, we used {up, down and flat} as {0,1,2} respectively.
#' @param sep the field separator character - default ";".
#' @return a data frame
#' @importFrom logging basicConfig
#' @export
generateDataSetsLocally <- function(folder, step, sep = ","){
  pathForFolder <- paste(system.file(package="motsai"), "/extdata/",folder, sep="")
  sessions <- list.files(pathForFolder, full.names = T)
  fullDataSet <- NULL
  all <- NULL
  for (i in sessions){
    loginfo(paste("Loading data from", i, sep=" : "), logger="motsai.test")
    filenames <- list.files(path=i, full.names=TRUE)
    pedometer <- filenames[basename(filenames) %in% c("StepCycles.csv")]
    filenames <- sort(filenames[basename(filenames) %nin% c(".DS_Store", "StepCycles.csv")])
    mergedData <- read.csv(file=filenames[1], sep = sep, header=T)
    mergedData[is.na(mergedData)] <- 0
    steps <- read.csv(file=pedometer, sep = sep, header=T)

    features <- c("var", "sd", "rms", "median", "mean", "mad","aad")
    var <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "var")
    sd <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "sd")
    median <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "median")
    mean <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "mean")
    mad <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "mad")
    aad <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "aad")
    rms <- mergeAndApplyFeatureDataFrameLocally(mergedData, steps, "rms")

    fullDataSet <- merge(aad, mad, by = "key")
    fullDataSet <- merge(fullDataSet, mean, by = "key")
    fullDataSet <- merge(fullDataSet, median, by = "key")
    fullDataSet <- merge(fullDataSet, rms, by = "key")
    fullDataSet <- merge(fullDataSet, sd, by = "key")
    fullDataSet <- merge(fullDataSet, var, by = "key")


    fullDataSet$key <- NULL
    fullDataSet$step <- as.integer(step)
    all <- rbind(all, fullDataSet)
  }

  return(all)

}
