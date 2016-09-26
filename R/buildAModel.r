#' Builds a GLM model
#'
#' Dropping the most redundant variables in a step wise fashion, the remaining variables are used to build a GLM model, then extract only the statistically significant variables and rebuild the model. Finally, generate the probability formula.
#'
#' @param dataSource the data frame to process.
#' @param predicator the independent variable.
#' @param forumla list of metrics concatenated with a plus.
#' @param threshold the threshold of the significant variables
#' @return text
#' @importFrom arm bayesglm
#' @importFrom logging basicConfig
#' @export
buildAModel <- function(dataSource, predicator, forumla, threshold=0.05){

  # Logging header
  basicConfig()
  addHandler(writeToFile, logger="motsai", file="motsai.log")

  sig_threshold = threshold # threshold of the p-value
  forumlaOfModel <- paste(predicator,'~', forumla)

  loginfo("Building the model using the metrics that are not correlated!", logger="motsai.model")
  modelFitness <- bayesglm(forumlaOfModel, data=dataSource, family = "binomial", control = list(maxit = 120))
  # extract the metrics that are statistically significant and rebuild the model.

  significantMetrics <- names(coefficients(summary(modelFitness))[coefficients(summary(modelFitness))[,4] < sig_threshold,][,4])
  significantMetrics <- significantMetrics[ significantMetrics != "(Intercept)"]
  #calculate the number of metrics removed from the model.
  significantMetricsPrint <- significantMetrics
  totalNumberOfMetrics <- abs(length(significantMetrics) - length(coefficients(summary(modelFitness))[,4]))
  totalNumberOfMetricsRemoved <- totalNumberOfMetrics
  significantMetrics <- paste(significantMetrics, collapse="+")

  loginfo("Extract only the statistically significant metrics from the first model and re-build it using them.", logger="motsai.model")
  if (significantMetrics == ""){
    logwarn("NOTE: All the metrics seems to be statistically insignificant, we will build the model again with them all", logger="motsai.model")
    significantMetrics <- forumla
  }


  loginfo(paste("Number of metrics removed", totalNumberOfMetricsRemoved, sep=" : "), logger="motsai.model")
  loginfo(paste("The statistically significant metrics are", paste(significantMetricsPrint, collapse = ", "), sep=" : "), logger="motsai.model")


  forumla2 <- paste(predicator,'~',significantMetrics,sep="")
  #rebuilding the model
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/complete_separation_logit_models.htm
  modelFitness2 <- bayesglm(forumla2, data=dataSource, family = "binomial", control = list(maxit = 120))
  summary(modelFitness2)
  # build the equation of the model with the metrics that are statistically significant.

  loginfo("Building the probability formula.", logger="motsai.model")
  equationText <- paste("generateEquation(", deparse(substitute(dataSource))[1], ",modelFitness2)")
  equation <- eval(parse(text=equationText))

  loginfo(paste("The probability formula", equation, sep=" : "), logger="motsai.model.equation")
  return(equation)
}
