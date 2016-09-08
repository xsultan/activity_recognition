


#' Generates an equation
#'
#' Generates an equation of the form Y = a + bX, where X is the explanatory variable and Y is the dependent variable. Using this equation we can find the probability of being an the dependent variable.
#'
#' @param dataSource the data frame to process.
#' @param model the GLM model where to extract the coefficients from.
#' @return text
generateEquation <- function(dataSource, model){
  equation <- (gsub(paste("\\*\\s", deparse(substitute(dataSource)), "\\$\\(Intercept\\)\\s\\+", sep=""), "- (", (paste(coef(model), names(coef(model)), sep = paste(' * ',gsub(" ", "", deparse(substitute(dataSource))),'$', sep=""), collapse = ' + '))))
  evEquation <- paste("1/(1+exp((-", equation, "))))", sep = "")
  return(evEquation)
}

#' Builds a GLM model
#'
#' Dropping the most redundant variables in a step wise fashion, the remaining variables are used to build a GLM model, then extract only the statistically significant variables and rebuild the model. Finally, generate the probability formula.
#'
#' @param dataSource the data frame to process.
#' @param predicator the independent variable.
#' @return text
#' @importFrom Hmisc, arm
#'
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
  equation <- generateEquation(dataSource, modelFitness2)

  return(equation)
}
