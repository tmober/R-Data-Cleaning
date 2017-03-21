#03/18/2017 - TO
#DATA CLEANING FOR LETTERS AND NUMBERS TASK (AKA CREATE - R Challenge 3 - ATTEMPT#3)

# R Version Information:
# _                           
# platform       x86_64-apple-darwin13.4.0   
# arch           x86_64                      
# os             darwin13.4.0                
# system         x86_64, darwin13.4.0        
# status                                     
# major          3                           
# minor          3.2                         
# year           2016                        
# month          10                          
# day            31                          
# svn rev        71607                       
# language       R                           
# version.string R version 3.3.2 (2016-10-31)
# nickname       Sincere Pumpkin Patch 

# Find the following using the ag_output_raw.csv file:
## (1) RTs on single-task trials (Block 1 and Block 2)
## (2) RTs on repetition trials (in Block 3)
## (3) RTs on switching trials (in Block 3)
## (4) Switching cost in RTs (switching RTs – repetition RTs, with smaller values indicating better performance [Block 3 RTs repetition - Block 3 switching])
## (5) Mixing cost in RTs (repetition RTs - single-task RTs, with smaller values indicating better performance [Block 1&2 RTs single-task - Block 3 repetition])


### Some notes:
## For each of these variables, the outliers (more than 2SD from mean for each user) have been removed.
## RTs used only for userScore=='correct' trials. Incorrect trials not counted.
## Both pre- and posttest scores are calculated for each of the 5 variables above.
## This R script to be used for the file '20170318_output_ln_ALL.csv' only
library(data.table)
library(datasets)
library(lubridate)  #work with dates

# Get and set your working directory
getwd()
#setwd('C:/Users/...')

# Import data from the file in your working directory
rawlnOutput <- read.csv("20170318_output_ln_ALL.csv", header=TRUE, stringsAsFactor = FALSE)
#Check its structure
str(rawlnOutput)

# Remove certain subject numbers that are not part of the actual sample (e.g., researcher associated studyIDs)
rawlnOutput <- subset(rawlnOutput, rawlnOutput$participantID != '0')

# Create new variable "test" and use it to relabel the itemIDs as the pre-/posttest
rawlnOutput['test'] <- NA
rawlnOutput$test[rawlnOutput$itemID == "894" | rawlnOutput$itemID == "907" | rawlnOutput$itemID == "922" | rawlnOutput$itemID == "935" ] <- 'pre'
rawlnOutput$test[rawlnOutput$itemID == "903" | rawlnOutput$itemID == "916" | rawlnOutput$itemID == "931"] <- 'post'


# Generate max trials for blocks in each session per user. Blocks 1&2 have 24 trials total. Block 3 has 96 trials total.
block12 = subset(rawlnOutput, (rawlnOutput$blockID=='BLOCK1' | rawlnOutput$blockID=='BLOCK2'))
block12Max = setnames(aggregate(block12$trialID ~ block12$participantID + block12$sesID + block12$blockID, block12, max), c('participantID', 'sesID', 'blockID', 'maxTrial12'))
rawlnOutput1 <- merge(rawlnOutput, block12Max, all.x = TRUE, sort=F)

block3 = subset(rawlnOutput,rawlnOutput$blockID=='BLOCK3')
block3Max = setnames(aggregate(block3$trialID ~ block3$participantID + block3$sesID + block3$blockID, block3, max), c('participantID', 'sesID', 'blockID', 'maxTrial3'))
rawlnOutput2 <- merge(rawlnOutput1, block3Max, all.x = TRUE, sort=F)

# Filter for complete sessions only by comparing the total number of trials with the maximum expected
lnOutput = subset(rawlnOutput2, ((rawlnOutput2$blockID=='BLOCK1' | rawlnOutput2$blockID=='BLOCK2') & rawlnOutput2$maxTrial12=='24') | (rawlnOutput2$blockID=='BLOCK3' & rawlnOutput2$maxTrial3=='96'))
str(rawlnOutput)



# Generate new variable "test" and use it to relable the pre-/posttests sessions that we want to use
## This includes (1) Most recent complete pretest session, and (2) Earliest complete posttest session
lnOutput['time'] <- NA
lnOutput['time'] <- as.POSIXct(as.POSIXlt(lnOutput$trialStart, format = "%m/%d/%Y %H:%M"))


# Creates a new dataframe for timeMax to be used to select pretest sessions (i.e., most recent pretests)
pretestSes = subset(lnOutput, lnOutput$test=='pre')
pretestSesTimeMax = setnames(aggregate(pretestSes$time ~ pretestSes$participantID + pretestSes$test + pretestSes$sesID, pretestSes, max), c('participantID', 'test', 'sesID', 'mostRecent'))
pretestSesTimeMax$mostRecent = NULL

# Creates a new dataframe for timeMin to be used to select posttest sessions (i.e., earliest posttest)
posttestSes = subset(lnOutput, lnOutput$test=='post')
pretestSes = subset(lnOutput, lnOutput$test=='pre')
posttestSesTimeMin = setnames(aggregate(posttestSes$time ~ posttestSes$participantID + posttestSes$test + posttestSes$sesID, posttestSes, min), c('participantID', 'test', 'sesID', 'Earliest'))
posttestSesTimeMin$Earliest = NULL


# Merges only columns that were associated with sessions (i.e., "sesIDs") that were either the most recent pretest or the earliest posttest of those that were complex
lnOutput <- merge(lnOutput, pretestSesTimeMax, all.x = TRUE, sort=F)
lnOutput <- merge(lnOutput, posttestSesTimeMin, all.x = TRUE, sort=F)



#######################
#### PRETEST DATA ####
#######################

### Part 1a Pre: RTs on single-task trials [*Assumes that single-task traisl occur in all trials in BLOCK1 and BLOCK2]
preRTsingleTaskData = subset(lnOutput, lnOutput$test=='pre' & lnOutput$userScore=='correct' & (lnOutput$blockID=='BLOCK1' | lnOutput$blockID=='BLOCK2'))
preavgRTsingleTask = setNames(aggregate(preRTsingleTaskData$userRT~preRTsingleTaskData$participantID, lnOutput, mean),c('participantID', 'preavgRTsingleTaskUser'))
presdRTsingleTask = setNames(aggregate(preRTsingleTaskData$userRT~preRTsingleTaskData$participantID, lnOutput, sd),c('participantID', 'presdRTsingleTaskUser'))

### Part 1b Pre: RTs on single-task trials with outliers removed
pren1 = boxplot(preRTsingleTaskData$userRT~preRTsingleTaskData$participantID, data = preRTsingleTaskData, boxwex=0.25)
preRTsingleTaskNoOutliers = preRTsingleTaskData[-which(preRTsingleTaskData$userRT %in% pren1$out), ]
preavgRTsingleTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTsingleTaskNoOutliers, mean),c('participantID', 'preavgRTsingleTaskNoOutliers'))
presdRTsingleTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTsingleTaskNoOutliers, sd),c('participantID', 'presdRTsingleTaskNoOutliers'))

preRTsingleTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(preavgRTsingleTask, presdRTsingleTask, preavgRTsingleTaskNoOutliers, presdRTsingleTaskNoOutliers))


### Part 2a Pre: RTs on repetition task trials (Block 3) [*Assumes that switching only occurs between quadrants 2->3 and 4->1 in BLOCK3]
preRTrepTaskData = subset(lnOutput, lnOutput$test=='pre' & lnOutput$blockID=='BLOCK3' & lnOutput$userScore=='correct' & (lnOutput$QuadrID==2 | lnOutput$QuadrID==4))
preavgRTrepTask = setNames(aggregate(preRTrepTaskData$userRT~preRTrepTaskData$participantID, preRTrepTaskData, mean),c('participantID', 'preavgRTrepTaskUser'))
presdRTrepTask = setNames(aggregate(preRTrepTaskData$userRT~preRTrepTaskData$participantID, preRTrepTaskData, sd),c('participantID', 'presdRTrepTaskUser'))

### Part 2b Pre: RTs on repetition task trials with outliers removed
pren2 = boxplot(preRTrepTaskData$userRT~preRTrepTaskData$participantID, data = preRTrepTaskData, boxwex=0.25)
preRTrepTaskNoOutliers = preRTrepTaskData[-which(preRTrepTaskData$userRT %in% pren2$out), ]
preavgRTrepTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTrepTaskNoOutliers, mean),c('participantID', 'preavgRTrepTaskNoOutliers'))
presdRTrepTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTrepTaskNoOutliers, sd),c('participantID', 'presdRTrepTaskNoOutliers'))

preRTrepTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(preavgRTrepTask, presdRTrepTask, preavgRTrepTaskNoOutliers, presdRTrepTaskNoOutliers))
#write.csv(RTrepTask,file='RTrepTask')


### Part 3a Pre: RTs on switch task trials (Block 3) [*Assumes that switching only occurs between quadrants 2->3 and 4->1 in BLOCK3]
preRTswitchTaskData = subset(lnOutput, lnOutput$test=='pre' & lnOutput$blockID=='BLOCK3' & lnOutput$userScore=='correct' & (lnOutput$QuadrID==1 | lnOutput$QuadrID==3))
preavgRTswitchTask = setNames(aggregate(preRTswitchTaskData$userRT~preRTswitchTaskData$participantID, preRTswitchTaskData, mean),c('participantID', 'preavgRTswitchTaskUser'))
presdRTswitchTask = setNames(aggregate(preRTswitchTaskData$userRT~preRTswitchTaskData$participantID, preRTswitchTaskData, sd),c('participantID', 'presdRTswitchTaskUser'))

### Part 3b Pre: RTs on switch task trials with outliers removed
pren3 = boxplot(preRTswitchTaskData$userRT~preRTswitchTaskData$participantID, data = preRTswitchTaskData, boxwex=0.25)
preRTswitchTaskNoOutliers = preRTswitchTaskData[-which(preRTswitchTaskData$userRT %in% pren3$out), ]
preavgRTswitchTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTswitchTaskNoOutliers, mean),c('participantID', 'preavgRTswitchTaskNoOutliers'))
presdRTswitchTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = preRTswitchTaskNoOutliers, sd),c('participantID', 'presdRTswitchTaskNoOutliers'))

preRTswitchTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(preavgRTswitchTask, presdRTswitchTask, preavgRTswitchTaskNoOutliers, presdRTswitchTaskNoOutliers))


# Use the line below if the ParticipantRTStats csv for the pretest only
#ParticipantRTStats <- Reduce(function(x, y) merge(x, y, all=TRUE), list(preRTsingleTask, preRTrepTask, preRTswitchTask))



#######################
#### POSTTEST DATA ####
#######################

### Part 1a Post: RTs on single-task trials [*Assumes that single-task traisl occur in all trials in BLOCK1 and BLOCK2]
postRTsingleTaskData = subset(lnOutput, lnOutput$test=='post' & lnOutput$userScore=='correct' & (lnOutput$blockID=='BLOCK1' | lnOutput$blockID=='BLOCK2'))
postavgRTsingleTask = setNames(aggregate(postRTsingleTaskData$userRT~postRTsingleTaskData$participantID, lnOutput, mean),c('participantID', 'postavgRTsingleTaskUser'))
postsdRTsingleTask = setNames(aggregate(postRTsingleTaskData$userRT~postRTsingleTaskData$participantID, lnOutput, sd),c('participantID', 'postsdRTsingleTaskUser'))

### Part 1b Post: RTs on single-task trials with outliers removed
postn1 = boxplot(postRTsingleTaskData$userRT~postRTsingleTaskData$participantID, data = postRTsingleTaskData, boxwex=0.25)
postRTsingleTaskNoOutliers = postRTsingleTaskData[-which(postRTsingleTaskData$userRT %in% postn1$out), ]
postavgRTsingleTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTsingleTaskNoOutliers, mean),c('participantID', 'postavgRTsingleTaskNoOutliers'))
postsdRTsingleTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTsingleTaskNoOutliers, sd),c('participantID', 'postsdRTsingleTaskNoOutliers'))

postRTsingleTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(postavgRTsingleTask, postsdRTsingleTask, postavgRTsingleTaskNoOutliers, postsdRTsingleTaskNoOutliers))

### Part 2a Post: RTs on repetition task trials (Block 3) [*Assumes that switching only occurs between quadrants 2->3 and 4->1 in BLOCK3]
postRTrepTaskData = subset(lnOutput, lnOutput$test=='post' & lnOutput$blockID=='BLOCK3' & lnOutput$userScore=='correct' & (lnOutput$QuadrID==2 | lnOutput$QuadrID==4))
postavgRTrepTask = setNames(aggregate(postRTrepTaskData$userRT~postRTrepTaskData$participantID, postRTrepTaskData, mean),c('participantID', 'postavgRTrepTaskUser'))
postsdRTrepTask = setNames(aggregate(postRTrepTaskData$userRT~postRTrepTaskData$participantID, postRTrepTaskData, sd),c('participantID', 'postsdRTrepTaskUser'))

### Part 2b Post: RTs on repetition task trials with outliers removed
postn2 = boxplot(postRTrepTaskData$userRT~postRTrepTaskData$participantID, data = postRTrepTaskData, boxwex=0.25)
postRTrepTaskNoOutliers = postRTrepTaskData[-which(postRTrepTaskData$userRT %in% postn2$out), ]
postavgRTrepTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTrepTaskNoOutliers, mean),c('participantID', 'postavgRTrepTaskNoOutliers'))
postsdRTrepTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTrepTaskNoOutliers, sd),c('participantID', 'postsdRTrepTaskNoOutliers'))

postRTrepTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(postavgRTrepTask, postsdRTrepTask, postavgRTrepTaskNoOutliers, postsdRTrepTaskNoOutliers))


### Part 3a Post: RTs on switch task trials (Block 3) [*Assumes that switching only occurs between quadrants 2->3 and 4->1 in BLOCK3]
postRTswitchTaskData = subset(lnOutput, lnOutput$test=='post' & lnOutput$blockID=='BLOCK3' & lnOutput$userScore=='correct' & (lnOutput$QuadrID==1 | lnOutput$QuadrID==3))
postavgRTswitchTask = setNames(aggregate(postRTswitchTaskData$userRT~postRTswitchTaskData$participantID, postRTswitchTaskData, mean),c('participantID', 'postavgRTswitchTaskUser'))
postsdRTswitchTask = setNames(aggregate(postRTswitchTaskData$userRT~postRTswitchTaskData$participantID, postRTswitchTaskData, sd),c('participantID', 'postsdRTswitchTaskUser'))

### Part 3b Post: RTs on switch task trials with outliers removed
postn3 = boxplot(postRTswitchTaskData$userRT~postRTswitchTaskData$participantID, data = postRTswitchTaskData, boxwex=0.25)
postRTswitchTaskNoOutliers = postRTswitchTaskData[-which(postRTswitchTaskData$userRT %in% postn3$out), ]
postavgRTswitchTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTswitchTaskNoOutliers, mean),c('participantID', 'postavgRTswitchTaskNoOutliers'))
postsdRTswitchTaskNoOutliers = setNames(aggregate(userRT ~ participantID, data = postRTswitchTaskNoOutliers, sd),c('participantID', 'postsdRTswitchTaskNoOutliers'))

postRTswitchTask <- Reduce(function(x, y) merge(x, y, all=TRUE), list(postavgRTswitchTask, postsdRTswitchTask, postavgRTswitchTaskNoOutliers, postsdRTswitchTaskNoOutliers))


# Use the line below if the ParticipantRTStats csv for the posttest only
#ParticipantRTStats <- Reduce(function(x, y) merge(x, y, all=TRUE), list(postRTsingleTask, postRTrepTask, postRTswitchTask))


# Use the line below to merge all of the data into a single csv file
ParticipantRTStats <- Reduce(function(x, y) merge(x, y, all=TRUE), list(preRTsingleTask, preRTrepTask, preRTswitchTask, postRTsingleTask, postRTrepTask, postRTswitchTask))
# ParticipantRTStats['dreamID'] <- NA
# ParticipantRTStats['age'] <- lnOutput$dreamID
# These two lines commented out because I am not sure the datafile will have the dreamID. If it does, you can uncomment and add that to the .csv

####################################
#### SWITCHING AND MIXING COSTS ####
####################################

#### PRETEST #### 

### Part 4 Pre: Switching cost in RTs (switching RTs – repetition RTs, with smaller values indicating better performance [Block 3 RTs repetition - Block 3 switching])
ParticipantRTStats['preavgRTswitchCostUser'] <- NA
ParticipantRTStats['preavgRTswitchCostUser'] <- ParticipantRTStats$preavgRTswitchTaskUser - ParticipantRTStats$preavgRTrepTaskUser


### Part 5 Pre: Mixing cost in RTs (repetition RTs - single-task RTs, with smaller values indicating better performance [Block 1&2 RTs single-task - Block 3 repetition])
ParticipantRTStats['preavgRTmixCostUser'] <- NA
ParticipantRTStats['preavgRTmixCostUser'] <- ParticipantRTStats$preavgRTrepTaskUser - ParticipantRTStats$preavgRTsingleTaskUser


#### POSTTEST #### 

### Part 4 Post: Switching cost in RTs (switching RTs – repetition RTs, with smaller values indicating better performance [Block 3 RTs repetition - Block 3 switching])
ParticipantRTStats['postavgRTswitchCostUser'] <- NA
ParticipantRTStats['postavgRTswitchCostUser'] <- ParticipantRTStats$postavgRTswitchTaskUser - ParticipantRTStats$postavgRTrepTaskUser


### Part 5 Post: Mixing cost in RTs (repetition RTs - single-task RTs, with smaller values indicating better performance [Block 1&2 RTs single-task - Block 3 repetition])
ParticipantRTStats['postavgRTmixCostUser'] <- NA
ParticipantRTStats['postavgRTmixCostUser'] <- ParticipantRTStats$postavgRTrepTaskUser - ParticipantRTStats$postavgRTsingleTaskUser


#### WRITE TO CSV #### 

# Run the following code to produce a csv in the working directory
#write.csv(ParticipantRTStats,file='LN_Participant_Data')

# Run the following code to produce a csv in the working directory that only has the variables you indicated:
#preLN_switchCost, preLN_switchRT, preLN_nonswitchRT, postLN_switchCost, postLN_switchRT, postLN_nonswitchRT  
ln_participant_data_clean <- data.frame(ParticipantRTStats$participantID)
ln_participant_data_clean['preLN_switchCost'] <- NA
ln_participant_data_clean['preLN_switchCost'] <- ParticipantRTStats$preavgRTswitchCostUser
  
ln_participant_data_clean['preLN_switchRT'] <- NA
ln_participant_data_clean['preLN_switchRT'] <- ParticipantRTStats$preavgRTswitchTaskUser

ln_participant_data_clean['preLN_nonswitchRT'] <- NA
ln_participant_data_clean['preLN_nonswitchRT'] <- ParticipantRTStats$preavgRTrepTaskUser

ln_participant_data_clean['postLN_switchCost'] <- NA
ln_participant_data_clean['postLN_switchCost'] <- ParticipantRTStats$postavgRTswitchCostUser

ln_participant_data_clean['postLN_switchRT'] <- NA
ln_participant_data_clean['postLN_switchRT'] <- ParticipantRTStats$postavgRTswitchTaskUser

ln_participant_data_clean['postLN_nonswitchRT'] <- NA
ln_participant_data_clean['postLN_nonswitchRT'] <- ParticipantRTStats$postavgRTrepTaskUser

write.csv(ln_participant_data_clean,file='ln_participant_data-edited_clean4')
