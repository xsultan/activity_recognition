
#' Builds a predictive model and returns the RMSE - helper
#'
#' Builds a predictive model and return the Root-Mean-Square Error (the difference between the actual and the predicted values).
#'
#' @param dataSource the data frame to process.
#' @param train the training data frame.
#' @param test the testing data frame.
#' @param predictor the predictor to build the model with.
#' @return matrix
#' @export
findRMSEFoundation <- function (dataSource, train, test, predictor)
{
  train <- na.omit(train)
  test <- na.omit(test)
  formula <- paste('step>0~',predictor)
  model.glm <- glm(formula, data=train, family = "binomial")
  test.prob <- predict (model.glm, test, type="response")

  return (rmserr(test.prob,test$step)$rmse[1])
}


#' Builds a predictive model and returns the RMSE
#'
#' Builds a predictive model and return the Root-Mean-Square Error (the difference between the actual and the predicted values).
#'
#' @param dataSource the data frame to process.
#' @param predictor the predictor to build the model with, a factor of predictors can be passed.
#' @param number_of_runs Number of of folds of the cross validation.
#' @return factor
#' @export
findRMSE <- function (dataSource, predictor, number_of_runs){
  class.res <- 0
  rmse.res <- 0

  #[1] 0.3562574
  #[1] 0.3513133
  for (i in 1:number_of_runs){
    set.seed(i);
    test.rows <- sample(1:nrow(dataSource), 0.33*nrow(dataSource));
    test <- dataSource[test.rows, ];
    train <- dataSource[-test.rows, ];

    class.res <- findRMSEFoundation(dataSource, train, test, predictor)
    rmse.res[i] <- class.res[1]
  }
  values = c(mean(rmse.res))
  return(values)
}
