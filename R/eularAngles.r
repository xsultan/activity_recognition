#' Calculate the Eular angles from quaternion
#'
#' Calculate the eular angles from quaternion and return (timestamp, roll1, roll2, pitch, yaw1, yaw2)
#'
#' @param timestamp the timestamp of the step
#' @param a represents the reading from the quaternion value of quat0
#' @param b represents the reading from the quaternion value of quat1
#' @param c represents the reading from the quaternion value of quat2
#' @param d represents the reading from the quaternion value of quat3
#' @param radian by default is TRUE, if FALSE the returned calculations would be in degrees instead.
#' @return a list contains the following data (timestamp, roll1, roll2, pitch, yaw1, yaw2)
#'
#' @export
eularAngles <- function(timestamp, a,b,c,d, radian = TRUE){
  aq <- a/32768.0
  bq <- b/32768.0
  cq <- c/32768.0
  dq <- d/32768.0

  roll1  <- 2*(aq*bq+cq*dq)
  roll2  <- 1-2*(bq*bq+cq*cq)
  pitch <- asin(2*(aq*cq-bq*dq))
  yaw1   <- 2*(aq*dq+bq*cq)
  yaw2   <- 1-2*(dq*dq+cq*cq)


  if (radian == FALSE){
    roll1  <- roll1 * 180/pi
    roll2  <- roll2 * 180/pi
    pitch <- pitch * 180/pi
    yaw1   <- yaw1 * 180/pi
    yaw2   <- yaw2 * 180/pi
  }

  return (c(timestamp=timestamp, roll1=roll1, roll2=roll2, pitch=pitch, yaw1=yaw1, yaw2=yaw2))
}
