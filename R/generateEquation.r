#' Generates an equation
#'
#' Generates an equation of the form Y = a + bX, where X is the explanatory variable and Y is the dependent variable. Using this equation we can find the probability of being an the dependent variable.
#'
#' @param dataSource the data frame to process.
#' @param model the GLM model where to extract the coefficients from.
#' @return text
#' @export
generateEquation <- function(dataSource, model){
  equation <- (gsub(paste("\\*\\s", deparse(substitute(dataSource)), "\\$\\(Intercept\\)\\s\\+", sep=""), "- (", (paste(coef(model), names(coef(model)), sep = paste(' * ',gsub(" ", "", deparse(substitute(dataSource))),'$', sep=""), collapse = ' + '))))
  evEquation <- paste("1/(1+exp((-", equation, "))))", sep = "")
  return(evEquation)
}
