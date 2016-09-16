#' Display the number of metrics
#'
#' Display the number of metrics used in the probability equation
#'
#' @param model The model equation
#' @return text
#' @export
totalNumberOfMetricsUsed <- function(model){
  return (message(paste("NUMBER OF METRICS USED TO GENERATE THE ESTAIMATE PROBABILITY EQUATION: ", nchar(gsub("[^*]","",model)))))
}
