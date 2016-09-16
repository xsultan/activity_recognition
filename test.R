library("motsai")
data("finalDataSet")
motsaiFinalDataSet <- all_data



#features to be extracted
features <- c("var", "sd", "rms", "median", "mean", "mad","aad")


#generating the data from the raw files in the package. These raw files reside under extdata
up <- generateDataSetsLocally("up", 0)
down <- generateDataSetsLocally("down", 1)
flat <- generateDataSetsLocally("flat", 2)

#merge all the data frames into one data frame
dataSetOfDifferentScenarios<- rbind(up,down,flat)

motsaiFinalDataSet <- dataSetOfDifferentScenarios





#-------------------- Testing the data set -------------------#


up <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 0),]
down <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 1),]
flat <- motsaiFinalDataSet[which(motsaiFinalDataSet$step == 2),]


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


up_againstall <- rbind(up_processed[sample(nrow(up_processed),300),], down_processed[sample(nrow(down_processed),150),], flat_processed[sample(nrow(flat_processed),150),])


up_model <- buildAModel(up_againstall, "step")
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


down_againstall <- rbind(up_processed[sample(nrow(up_processed),150),], down_processed[sample(nrow(down_processed),300),], flat_processed[sample(nrow(flat_processed),150),])

down_model <- buildAModel(down_againstall, "step")

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

flat_againstall <- rbind(up_processed[sample(nrow(up_processed),150),], down_processed[sample(nrow(down_processed),150),], flat_processed[sample(nrow(flat_processed),300),])

flat_model <- buildAModel(flat_againstall, "step")





############# END ############





# NOW EXTRACTING THE UNTOUCHED ROWS TO TEST THE EFFECTIVENESS OF OUR ESTAIMATE PROBABILITY EQUATIONS.
motsaiFinalDataSet_clean <- motsaiFinalDataSet
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(up_againstall)), ]
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(down_againstall)), ]
motsaiFinalDataSet_clean <- motsaiFinalDataSet_clean[which(rownames(motsaiFinalDataSet_clean) %nin% rownames(flat_againstall)), ]
motsaiFinalDataSet_clean <- na.omit(motsaiFinalDataSet_clean)


#Naming the steps from 0,1,2 to up, down, flat

motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 0),]$step = "UP"
motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 1),]$step = "DOWN"
motsaiFinalDataSet_clean[which(motsaiFinalDataSet_clean$step == 2),]$step = "FLAT"




# CALCULATE THE ESTAIMATE PROBABILITY OF EACH MODEL
motsaiFinalDataSet_clean$UP <- round(eval(parse(text=gsub("up_againstall", "motsaiFinalDataSet_clean", up_model))),2)
motsaiFinalDataSet_clean$DOWN <- round(eval(parse(text=gsub("down_againstall", "motsaiFinalDataSet_clean", down_model))),2)
motsaiFinalDataSet_clean$FLAT <- round(eval(parse(text=gsub("flat_againstall", "motsaiFinalDataSet_clean", flat_model))),2)

motsaiFinalDataSet_clean$PROBABILITY <- colnames(motsaiFinalDataSet_clean[120:123])[apply(motsaiFinalDataSet_clean[120:123],1,which.max)]

# SPLIT THE DATA BASED ON STEP (UP, DOWN OR FLAT)
step <- split( motsaiFinalDataSet_clean , f = motsaiFinalDataSet_clean$step)



# ██╗   ██╗██████╗
# ██║   ██║██╔══██╗
# ██║   ██║██████╔╝
# ██║   ██║██╔═══╝
# ╚██████╔╝██║
# ╚═════╝ ╚═╝


# SHOW THE STEP AND THE PROBABILITY OF IT BEING UP
step$UP[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
cat("Detection", sprintf("%.2f%%", 100-colSums(step$UP[120]!=step$UP[124])/nrow(step$UP)*100)[1], sep=" : ")

# SHOW THE NUMBER OF METRICS USED IN THE PROBABILITY EQUATION
totalNumberOfMetricsUsed(up_model)



# ██████╗  ██████╗ ██╗    ██╗███╗   ██╗
# ██╔══██╗██╔═══██╗██║    ██║████╗  ██║
# ██║  ██║██║   ██║██║ █╗ ██║██╔██╗ ██║
# ██║  ██║██║   ██║██║███╗██║██║╚██╗██║
# ██████╔╝╚██████╔╝╚███╔███╔╝██║ ╚████║
# ╚═════╝  ╚═════╝  ╚══╝╚══╝ ╚═╝  ╚═══╝

# SHOW THE STEP AND THE PROBABILITY OF IT BEING DOWN
step$DOWN[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
cat("Detection", sprintf("%.2f%%", 100-colSums(step$DOWN[120]!=step$DOWN[124])/nrow(step$DOWN)*100)[1], sep=" : ")

# SHOW THE NUMBER OF METRICS USED IN THE PROBABILITY EQUATION
totalNumberOfMetricsUsed(down_model)




# ███████╗██╗      █████╗ ████████╗
# ██╔════╝██║     ██╔══██╗╚══██╔══╝
# █████╗  ██║     ███████║   ██║
# ██╔══╝  ██║     ██╔══██║   ██║
# ██║     ███████╗██║  ██║   ██║
# ╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝


# SHOW THE STEP AND THE PROBABILITY OF IT BEING FLAT
step$FLAT[120:124]

# SHOW THE PERCENTAGE OF THE CORRECT DETECTION
cat("Detection", sprintf("%.2f%%", 100-colSums(step$FLAT[120]!=step$FLAT[124])/nrow(step$FLAT)*100)[1], sep=" : ")

# SHOW THE NUMBER OF METRICS USED IN THE PROBABILITY EQUATION
totalNumberOfMetricsUsed(flat_model)
