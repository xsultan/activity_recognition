#' Generate dataset containing the metrics
#'
#' Detecting steps using a training and testing datasets
#'
#' @param pathOfTheFolder path to the source of the raw data.
#' @param feature Which feature to calculate, should be passed as a string or a vector of strings.
#' @param step indicates the step, we used {up, down and flat} as {0,1,2} respectively.
#' @param sep the field separator character - default ";".
#' @return a data frame
#' @export
generateDataSet <- function(pathOfTheFolder, features, step, sep = ";"){
  sessions <- list.files(pathOfTheFolder, full.names = T)
  all <- NULL
  for (i in sessions){
    extractStepsCommand <-  paste("extractStepCycle('",i,"/0'",")", sep="")
    steps <- eval(parse(text=extractStepsCommand))
    mergeAllRawDataCommand <- paste("mergeAllRawData('",i,"/0'",",", step, ")", sep="")
    mergedData <- eval(parse(text=mergeAllRawDataCommand))
    z<- generateFullFeatures(mergedData, steps, features, step)
    all <-rbind(all, z)
  }

  return(all)

}
