#' Extract the step cycle
#'
#' Extract the step cycle of a session.
#'
#' @param pathOfTheFolder path to the source of the raw data.
#' @param sep the field separator character.
#' @return a data frame of the steps cycle.
#'
#' @export
extractStepCycle <- function(pathOfTheFolder, sep = ";"){
  filenames <- list.files(path=pathOfTheFolder, full.names=TRUE)
  filenames <- filenames[basename(filenames) %in% c("pedometer.csv")]

  steps <- data.frame("timestamp"= numeric(0), "empty"= numeric(0), stringsAsFactors = FALSE)
  stepsCSV <- read.csv(file=filenames[1], sep = sep, header=F)[, -c(2,3,4)]
  for (j in 1:nrow(stepsCSV)){
    steps[nrow(steps)+1,] <- stepsCSV[j,]
  }

  steps$empty <- NULL
  return(steps)
}
