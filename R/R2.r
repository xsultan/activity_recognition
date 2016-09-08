#' Calculate the explained deviance of a model
#'
#' Calculate the fitness of a model using the following forumla: R-squared = 1 - (estimated variance)/(observed variance)
#'
#' @param model the model to extract the explained deviance from.
#' @return text
#'
R2 <- function(model){
  ED = 1 - (model$deviance / model$null.deviance)
  return (cat("Explained Deviance [R^2]: ", ED, sep=""))
}
