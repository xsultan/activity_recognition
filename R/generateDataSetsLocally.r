#' Generate dataset containing the steps data - from the local files of the package
#'
#' Generate dataset containing the steps data
#'
#' @param folder path to the source of the raw data.
#' @param feature Which feature to calculate, should be passed as a string or a vector of strings.
#' @param step indicates the step, we used {up, down and flat} as {0,1,2} respectively.
#' @param sep the field separator character - default ";".
#' @return a data frame
#' @export
generateDataSetsLocally <- function(folder, step, features, sep = ","){
  pathForFolder <- paste(system.file(package="motsai"), "/extdata/",folder, sep="")
  sessions <- list.files(pathForFolder, full.names = T)
  all <- NULL
  for (i in sessions){
    filenames <- list.files(path=i, full.names=TRUE)
    pedometer <- filenames[basename(filenames) %in% c("StepCycles.csv")]
    filenames <- sort(filenames[basename(filenames) %nin% c(".DS_Store", "StepCycles.csv")])
    mergedData <- read.csv(file=filenames, sep = sep, header=T)
    steps <- read.csv(file=pedometer, sep = sep, header=T)

    z<- generateFullFeatures(mergedData, steps, features, step)
    all <-rbind(all, z)
  }

  return(all)

}
