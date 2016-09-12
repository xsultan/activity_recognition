features <- c("mean", "median", "sd", "var", "mad", "aad","rms")


generateDataSet <- function(pathOfTheFolder, features, step, sep = ";"){
  sessions <- list.files(pathOfTheFolder, full.names = T)
  all <- NULL
  for (i in sessions){
    extractStepsCommand <-  paste("extractStepCycle('",i,"/0'",")", sep="")
    steps <- eval(parse(text=extractStepsCommand))
    mergeAllRawDataCommand <- paste("mergeAllRawData('",i,"/0'",",", step, ")", sep="")
    mergedData <- eval(parse(text=mergeAllRawDataCommand))
    z<- generateFullFeatures(mergedData, steps, features, step)
    all <-rbind(all, z)
  }

  return(all)

}



generateFullFeatures <- function(steps, stepsCycle, f, step){
  y4 <- NULL
  features <- f
  for (i in features){
    y3 <- mergeAndApplyFeatureDataFrame(steps, stepsCycle, i)
    y3$key <- rownames(y3)
    y4$key <- rownames(y3)
    y4 <- merge(y3, y4, by = "key")
  }

  y4$key <- NULL
  y4$step <- step

  return(y4)
}


features <- c("mean", "median", "sd", "var", "mad", "aad","rms")

up <- generateDataSet("/Users/x21/Desktop/motsai_r/record/2016-08-01/raw/walking/Session-17/", features, "0")

# save(all_data, file="data/finalDataSet.rda")




