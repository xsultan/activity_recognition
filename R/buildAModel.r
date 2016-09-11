#' Builds a GLM model
#'
#' Dropping the most redundant variables in a step wise fashion, the remaining variables are used to build a GLM model, then extract only the statistically significant variables and rebuild the model. Finally, generate the probability formula.
#'
#' @param dataSource the data frame to process.
#' @param predicator the independent variable.
#' @return text
#' @importFrom arm bayesglm
#' @importFrom Hmisc redun
#' @export
buildAModel <- function(dataSource, predicator){
  sig_threshold = 0.05 # threshold of the p-value
  datasetName = deparse(substitute(dataSource))

  redunancyForumla <- paste(predicator,'~.')
  redunancyOfModel <- redun(~., dataSource)
  redunancyOfModel <- redunancyOfModel$In
  redunancyOfModel <- redunancyOfModel[redunancyOfModel != predicator]
  forumla <- paste(redunancyOfModel, collapse="+")
  forumlaOfModel <- paste(predicator,'~', forumla)
  modelFitness <- bayesglm(forumlaOfModel, data=dataSource, family = "binomial", control = list(maxit = 120))
  # extract the metrics that are statistically significant and rebuild the model.
  significantMetrics <- names(coefficients(summary(modelFitness))[coefficients(summary(modelFitness))[,4] < sig_threshold,][,4])
  significantMetrics <- significantMetrics[ significantMetrics != "(Intercept)"]
  significantMetrics <- paste(significantMetrics, collapse="+")
  forumla2 <- paste(predicator,'~',significantMetrics)
  #rebuilding the model
  modelFitness2 <- bayesglm(forumla2, data=dataSource, family = "binomial", control = list(maxit = 120))
  summary(modelFitness2)
  # build the equation of the model with the metrics that are statistically significant.

  ### FIX
  # issue with passing the data frame name, can be fixed using the outputed text function 'capture.output'
  equationText <- paste("generateEquation(", deparse(substitute(dataSource)), "modelFitness2)")
  equation <- generateEquation(dataSource, modelFitness2)

  return(equation)
}
