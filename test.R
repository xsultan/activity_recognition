library("activity_recognition")
library("logging")

#This is a binary version of the data set, can be loaded without the need to preform the setps below to generate the data frames.
data("finalDataSet")
motsaiFinalDataSet <- all_data

#features to be extracted
features <- c("var", "sd", "rms", "median", "mean", "mad","aad")

sample_size <- 300
predicator <- "step"

# Logging header
basicConfig()
addHandler(writeToFile, logger="motsai", file="motsai.log")


# loginfo("Generating the data from the raw files in the package. These raw files reside under extdata.", logger="motsai.test")
# up <- generateDataSetsLocally("up", 0)
# down <- generateDataSetsLocally("down", 1)
# flat <- generateDataSetsLocally("flat", 2)
#
# loginfo("Merge all the data frames into one data frame.", logger="motsai.test")
# dataSetOfDifferentScenarios<- rbind(up,down,flat)
#
# motsaiFinalDataSet <- dataSetOfDifferentScenarios





#-------------------- Testing the data set -------------------#


up <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 0),]
down <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 1),]
flat <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 2),]

loginfo("Remove the highly correlated metrics.", logger="motsai.test")
removedMetrics <- removeCorrelatedFeatures(motsaiFinalDataSet, predicator = "step", threshold = 0.9)

############### EXTRACTING SAMPLES OF THE DATA TO GENERATE THREE DATASETS INORDER FOR US TO BUILD 3 MODELS ###############
# 1. UP AGAINST DOWN AND FLAT - 300 ROWS OF UP, 150 ROWS OF DOWN AND 150 ROWS OF FLAT
# 2. DOWN AGAINST UP AND FLAT - 300 ROWS OF DOWN, 150 ROWS OF UP AND 150 ROWS OF FLAT
# 3. FLAT AGAINST UP AND DOWN - 300 ROWS OF FLAT, 150 ROWS OF UP AND 150 ROWS OF DOWN

#   :::    ::: :::::::::
#   :+:    :+: :+:    :+:
#   +:+    +:+ +:+    +:+
#   +#+    +:+ +#++:++#+
#   +#+    +#+ +#+
#   #+#    #+# #+#
#   ########  ###

up_processed <- up
up_processed$step <- 1

down_processed <- down
down_processed$step <- 0

flat_processed <- flat
flat_processed$step <- 0


up_againstall <- rbind(up_processed[sample(nrow(up_processed),sample_size),], down_processed[sample(nrow(down_processed),(sample_size/2)),], flat_processed[sample(nrow(flat_processed),(sample_size/2)),])


loginfo("Building the up model.", logger="motsai.test")
up_model <- buildAModel(up_againstall, predicator, removedMetrics)
############# END ############



#   :::::::::   ::::::::  :::       ::: ::::    :::
#   :+:    :+: :+:    :+: :+:       :+: :+:+:   :+:
#   +:+    +:+ +:+    +:+ +:+       +:+ :+:+:+  +:+
#   +#+    +:+ +#+    +:+ +#+  +:+  +#+ +#+ +:+ +#+
#   +#+    +#+ +#+    +#+ +#+ +#+#+ +#+ +#+  +#+#+#
#   #+#    #+# #+#    #+#  #+#+# #+#+#  #+#   #+#+#
#   #########   ########    ###   ###   ###    ####

up_processed <- up
up_processed$step <- 0

down_processed <- down
down_processed$step <- 1

flat_processed <- flat
flat_processed$step <- 0


down_againstall <- rbind(up_processed[sample(nrow(up_processed),(sample_size/2)),], down_processed[sample(nrow(down_processed),sample_size),], flat_processed[sample(nrow(flat_processed),(sample_size/2)),])

loginfo("Building the down model", logger="motsai.test")
down_model <- buildAModel(down_againstall, predicator, removedMetrics)

############# END ############


#   :::::::::: :::            ::: :::::::::::
#   :+:        :+:          :+: :+:   :+:
#   +:+        +:+         +:+   +:+  +:+
#   :#::+::#   +#+        +#++:++#++: +#+
#   +#+        +#+        +#+     +#+ +#+
#   #+#        #+#        #+#     #+# #+#
#   ###        ########## ###     ### ###


up_processed <- up

down_processed <- down
down_processed$step <- 0

flat_processed <- flat
flat_processed$step <- 1

flat_againstall <- rbind(up_processed[sample(nrow(up_processed),(sample_size/2)),], down_processed[sample(nrow(down_processed),(sample_size/2)),], flat_processed[sample(nrow(flat_processed),sample_size),])

loginfo("Building the flat model", logger="motsai.test")
flat_model <- buildAModel(flat_againstall, predicator, removedMetrics)

############# END ############





# NOW EXTRACTING THE UNTOUCHED ROWS TO TEST THE EFFECTIVENESS OF OUR ESTAIMATE PROBABILITY EQUATIONS.
motsaiFinalDataSet_clean <- motsaiFinalDataSet
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(up_againstall)), ]
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(down_againstall)), ]
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(flat_againstall)), ]
motsaiFinalDataSet_clean <- na.omit(motsaiFinalDataSet_clean)


#Naming the steps from 0,1,2 to up, down, flat
loginfo("Naming the steps from 0,1,2 to up, down, flat", logger="motsai.test")
motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 0),]$step = "UP"
motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 1),]$step = "DOWN"
motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 2),]$step = "FLAT"




# CALCULATE THE ESTAIMATE PROBABILITY OF EACH MODEL
loginfo("Calculate the Estaimate Probability of Each Model", logger="motsai.test")

motsaiFinalDataSet_clean$UP <- round(eval(parse(text=gsub("up_againstall", "motsaiFinalDataSet_clean", up_model))),2)
motsaiFinalDataSet_clean$DOWN <- round(eval(parse(text=gsub("down_againstall", "motsaiFinalDataSet_clean", down_model))),2)
motsaiFinalDataSet_clean$FLAT <- round(eval(parse(text=gsub("flat_againstall", "motsaiFinalDataSet_clean", flat_model))),2)

motsaiFinalDataSet_clean$PROBABILITY <- colnames(motsaiFinalDataSet_clean[120:123])[apply(motsaiFinalDataSet_clean[120:123],1,which.max)]

# SPLIT THE DATA BASED ON STEP (UP, DOWN OR FLAT)
loginfo("Split the Data Based on Step (Up, Down or Flat)", logger="motsai.test")
step <- split( motsaiFinalDataSet_clean , f = motsaiFinalDataSet_clean$step)



# ██╗   ██╗██████╗
# ██║   ██║██╔══██╗
# ██║   ██║██████╔╝
# ██║   ██║██╔═══╝
# ╚██████╔╝██║
# ╚═════╝ ╚═╝


# SHOW THE STEP AND THE PROBABILITY OF IT BEING UP
loginfo("Show the Step and the Probability of It Being Up", logger="motsai.test")
#step$UP[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
loginfo("Show the Percentage of the Correct Detection", logger="motsai.test")
loginfo(paste("Detection of steps being up : ", format((100-colSums(step$UP[120]!=step$UP[124])/nrow(step$UP)*100)[1], digits = 2,  nsmall=2),"%", sep=""))




# ██████╗  ██████╗ ██╗    ██╗███╗   ██╗
# ██╔══██╗██╔═══██╗██║    ██║████╗  ██║
# ██║  ██║██║   ██║██║ █╗ ██║██╔██╗ ██║
# ██║  ██║██║   ██║██║███╗██║██║╚██╗██║
# ██████╔╝╚██████╔╝╚███╔███╔╝██║ ╚████║
# ╚═════╝  ╚═════╝  ╚══╝╚══╝ ╚═╝  ╚═══╝

# SHOW THE STEP AND THE PROBABILITY OF IT BEING DOWN
loginfo("Show the Step and the Probability of It Being Down", logger="motsai.test")
#step$DOWN[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
loginfo("Show the Percentage of the Correct Detection", logger="motsai.test")
loginfo(paste("Detection of steps being down : ", format((100-colSums(step$DOWN[120]!=step$DOWN[124])/nrow(step$DOWN)*100)[1], digits = 2,  nsmall=2),"%", sep=""))




# ███████╗██╗      █████╗ ████████╗
# ██╔════╝██║     ██╔══██╗╚══██╔══╝
# █████╗  ██║     ███████║   ██║
# ██╔══╝  ██║     ██╔══██║   ██║
# ██║     ███████╗██║  ██║   ██║
# ╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝


# SHOW THE STEP AND THE PROBABILITY OF IT BEING FLAT
loginfo("Show the Step and the Probability of It Being Flat", logger="motsai.test")
#step$FLAT[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
loginfo("Show the Percentage of the Correct Detection", logger="motsai.test")
loginfo(paste("Detection of steps being flat : ", format((100-colSums(step$FLAT[120]!=step$FLAT[124])/nrow(step$FLAT)*100)[1], digits = 2,  nsmall=2),"%", sep=""))
