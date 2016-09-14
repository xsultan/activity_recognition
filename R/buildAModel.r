#' Builds a GLM model
#'
#' Dropping the most redundant variables in a step wise fashion, the remaining variables are used to build a GLM model, then extract only the statistically significant variables and rebuild the model. Finally, generate the probability formula.
#'
#' @param dataSource the data frame to process.
#' @param predicator the independent variable.
#' @param numberOfKnots Number of knots - Specifying numberOfKnots=0 will force all effects to be linear which will speed up the process.
#' @return text
#' @importFrom arm bayesglm
#' @importFrom Hmisc redun
#' @export
buildAModel <- function(dataSource, predicator, numberOfKnots = NULL){
  sig_threshold = 0.05 # threshold of the p-value
  datasetName = deparse(substitute(dataSource))

  redunancyForumla <- paste(predicator,'~.')
  message("Applying the Redundancy Analysis to remove the highly correlated metrics")
  message("Note: This might take up to 3 minutes to be completed!")
  packageStartupMessage("Initializing...", appendLF = FALSE)
  if (is.null(numberOfKnots)){
    redunancyOfModel <- redun(~., dataSource)
  }else{
    redunancyOfModel <- redun(~., dataSource, nk = numberOfKnots)
  }
  packageStartupMessage(" done")
  redunancyOfModel <- redunancyOfModel$In
  redunancyOfModel <- redunancyOfModel[redunancyOfModel != predicator]
  forumla <- paste(redunancyOfModel, collapse="+")
  forumlaOfModel <- paste(predicator,'~', forumla)

  message("Building the model using the metrics that are not correlated!")
  modelFitness <- bayesglm(forumlaOfModel, data=dataSource, family = "binomial", control = list(maxit = 120))
  # extract the metrics that are statistically significant and rebuild the model.

  significantMetrics <- names(coefficients(summary(modelFitness))[coefficients(summary(modelFitness))[,4] < sig_threshold,][,4])
  significantMetrics <- significantMetrics[ significantMetrics != "(Intercept)"]
  significantMetrics <- paste(significantMetrics, collapse="+")

  message("Extract only the statistically significant metrics from the first model and re-build it using them.")
  if (significantMetrics == ""){
    message("NOTE: All the metrics seems to be statistically insignificant, we will build the model again with them all")
    significantMetrics <-forumla
  }
  forumla2 <- paste(predicator,'~',significantMetrics,sep="")
  #rebuilding the model
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/complete_separation_logit_models.htm
  modelFitness2 <- bayesglm(forumla2, data=dataSource, family = "binomial", control = list(maxit = 120))
  summary(modelFitness2)
  # build the equation of the model with the metrics that are statistically significant.

  message("Building the probability formula.")
  equationText <- paste("generateEquation(", deparse(substitute(dataSource))[1], ",modelFitness2)")
  equation <- eval(parse(text=equationText))

  return(equation)
}
