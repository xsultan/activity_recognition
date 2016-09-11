#' Merge all the raw data into one dataframe
#'
#' Merge all the raw data into one dataframe and return a data frame of all the data merged.
#'
#' @param pathOfTheFolder path to the source of the raw data.
#' @param step indicates the step, we used {up, down and flat} as {0,1,2} respectively.
#' @param radian by default is TRUE, if FALSE the returned calculations would be in degrees instead.
#' @param sep the field separator character.
#' @return a data frame of all the data merged.
#'
#' @export
mergeAllRawData <- function(pathOfTheFolder, step, radian = TRUE, sep = ";"){
  `%nin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
  filenames <- list.files(path=pathOfTheFolder, full.names=TRUE)
  pedometer <- filenames[basename(filenames) %in% c("pedometer.csv")]
  filenames <- sort(filenames[basename(filenames) %nin% c("pedometer.csv", "rotation.csv", "dump.txt")])

  force <- data.frame("timestamp"= numeric(0), "fx"= numeric(0), "fy"= numeric(0), "fz"= numeric(0),stringsAsFactors = FALSE)
  forceCSV <- read.csv(file=filenames[1], sep = sep, header=F)[, -c(5)]

  for (j in 1:nrow(forceCSV)){
    force[nrow(force)+1,] <- forceCSV[j,]
  }


  accelerometer <- data.frame("timestamp"= numeric(0), "ax"= numeric(0), "ay"= numeric(0), "az"= numeric(0),stringsAsFactors = FALSE)
  accelerometerCSV <- read.csv(file=filenames[2], sep = sep, header=F)[, -c(5,6,7)]

  for (j in 1:nrow(accelerometerCSV)){
    accelerometer[nrow(accelerometer)+1,] <- accelerometerCSV[j,]
  }


  gyro <- data.frame("timestamp"= numeric(0), "gx"= numeric(0), "gy"= numeric(0), "gz"= numeric(0),stringsAsFactors = FALSE)
  gyroCSV <- read.csv(file=filenames[2], sep = sep, header=F)[, -c(2,3,4)]

  for (j in 1:nrow(gyroCSV)){
    gyro[nrow(gyro)+1,] <- gyroCSV[j,]
  }

  mag <- data.frame("timestamp"= numeric(0), "mx"= numeric(0), "my"= numeric(0), "mz"= numeric(0),stringsAsFactors = FALSE)
  magCSV <- read.csv(file=filenames[3], sep = sep, header=F)[, -c(2,3,4)]

  for (j in 1:nrow(magCSV)){
    mag[nrow(mag)+1,] <- magCSV[j,]
  }


  quat <- data.frame("timestamp"= numeric(0), "roll1"= numeric(0), "roll2"= numeric(0), "pitch"= numeric(0), "yaw1"= numeric(0), "yaw2"= numeric(0), stringsAsFactors = FALSE)
  quatCSV <- read.csv(file=filenames[4], sep = sep, header=F)[, -c(6)]

  for (j in 1:nrow(quatCSV)){
    quat[nrow(quat)+1,] <- eularAngles(quatCSV[j,][1], quatCSV[j,][2], quatCSV[j,][3], quatCSV[j,][4], quatCSV[j,][5], radian)
  }

  #merge all datasets into one data frame
  MergedData <- accelerometer
  MergedData <- merge(MergedData, quat, by="timestamp")
  MergedData <- merge(MergedData, force, by="timestamp")
  MergedData <- merge(MergedData, gyro, by="timestamp")
  MergedData <- merge(MergedData, mag, by="timestamp")
  MergedData$step <-step

  return(MergedData)
}

#d<- MergeAllRawData("/Users/sultan/Downloads/neblina-python/record/2016-07-27-early-morning/Session-0/0", 0, F)
