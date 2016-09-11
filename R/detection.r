#' Detection of the steps - helper function
#'
#' Detecting steps using a training and testing datasets
#'
#' @param dataSource the data frame to process.
#' @param train the training dataset.
#' @param test the testing dataset.
#' @param cutoff the cutoff of the detection
#' @param predicator the predicator, it can be passed as a vector of predicators
#' @return vector containing false positive, accuracy, recall, precision and the f-measure of the detection
#' @export
detection <- function (dataSource, train, test, cutoff, predictor)
{
  formula <- paste('(step>0)~',predictor) #determine F or T
  model.glm <- glm(formula=formula, data=train, family = "binomial")
  test.prob <- predict (model.glm, test, type="response")
  test.pred <- test.prob >= cutoff

  outcome <- table(factor(test$step>0, levels=c(F,T)), factor(test.pred, levels=c(F,T)))
  TN <- outcome[1,1]
  FN <- outcome[2,1]
  FP <- outcome[1,2]
  TP <- outcome[2,2]
  precision <- if (TP + FP ==0) { 1 } else { TP / (TP + FP) }
  recall <- TP / (TP + FN)
  accuracy <- (TP + TN) / (TN + FN + FP + TP)
  fmeasure <- 2*((precision*recall)/(precision+recall))
  falsep <- FP/(FP+TN)
  return (c(falsep, accuracy, recall, precision, fmeasure))
}
