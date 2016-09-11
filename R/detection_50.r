#' Detection of the steps - main function
#'
#' This can be used to detect steps using a training and testing datasets - using detection()
#'
#' @param dataSource the data frame to process.
#' @param predicator the predicator, it can be passed as a vector of predicators
#' @param cutoff the cutoff of the detection
#' @param num_runs the number of runs - default = 10
#' @return vector containing the mean offalse positive, accuracy, recall, precision and the f-measure of the detection based on the number of runs.
#' @export
detection_50 <- function (dataSource, predictor, cutoff, num_runs = 10){
  class.res <- 0
  prec.res =0
  rec.res =0
  fmeas.res =0
  acc.res =0
  falp.res =0


  for (i in 1:num_runs){
    set.seed(i);
    test.rows <- sample(1:nrow(dataSource), 0.33*nrow(dataSource));
    test <- dataSource[test.rows, ];
    train <- dataSource[-test.rows, ];

    cutoff = 0.5
    class.res <- detection(dataSource, train, test,cutoff,predictor)
    falp.res[i] <- class.res[1]
    acc.res[i] <- class.res[2]
    rec.res[i] <- class.res[3]
    prec.res[i] <- class.res[4]
    fmeas.res[i] <- class.res[5]
  }
  values = c(mean(acc.res),mean(falp.res), mean(rec.res), mean(prec.res), mean(fmeas.res))
  return (values)
}
