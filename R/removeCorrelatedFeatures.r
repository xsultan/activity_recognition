#' Remove the highly correlated features
#'
#' Dropping the most redundant variables in a step wise fashion, the remaining variables along with the predicator are returned in a forumla.
#'
#' @param dataSource the data frame to process.
#' @param predicator the independent variable - default = step.
#' @param threshold the adjusted R^2 cutoff for redundancy - default = 0.9
#' @param numberOfKnots Number of knots - Specifying numberOfKnots=0 will force all effects to be linear which will speed up the process.
#' @return text
#' @importFrom Hmisc redun
#' @importFrom logging basicConfig
#' @export
removeCorrelatedFeatures <- function(dataSource, predicator="step", threshold=0.9, numberOfKnots=NULL){

  # Logging header
  basicConfig()
  addHandler(writeToFile, logger="motsai", file="motsai.log")

  spliting.rows <- sample(1:nrow(dataSource), 0.5*nrow(dataSource));
  firstPart <- dataSource[spliting.rows, ];
  secondPart <- dataSource[-spliting.rows, ];

  firstPart$step <- 0
  secondPart$step <- 1

  data <- rbind(firstPart, secondPart)

  loginfo("Applying the Redundancy Analysis to remove the highly correlated metrics", logger="motsai.redundancy")
  message("Note: This might take up to 3 minutes to be completed!")
  packageStartupMessage("Initializing...", appendLF = FALSE)
  if (is.null(numberOfKnots)){
    variablesRedun <- redun(~., data=data, r2 = threshold)
  }else{
    variablesRedun <- redun(~., data=data, r2 = threshold, nk = numberOfKnots)
  }
  packageStartupMessage(" done")


  variablesRedun <- variablesRedun$In
  variablesRedun <- variablesRedun[variablesRedun != predicator]
  forumla <- paste(variablesRedun, collapse="+")

  totalNumberOfMetrics <- length(dataSource)-1
  totalNumberOfMetricsRemoved <- (length(dataSource)-1) - length(variablesRedun)

  loginfo(paste("Number of metrics removed", totalNumberOfMetricsRemoved, sep=" : "), logger="motsai.redundancy")
  loginfo(paste("The remaining metrics are", paste(variablesRedun, collapse = ", "), sep=" : "), logger="motsai.redundancy")

  return(forumla)

}
