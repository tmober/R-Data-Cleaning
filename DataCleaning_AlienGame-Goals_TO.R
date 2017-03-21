#03/12/2017 - TO
#DATA CLEANING FOR ALIEN GAME GOAL ATTAINMENT (AKA CREATE - R Challenge 2 - ATTEMPT#2)

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
## (1) highest level (string, numeric value), loop, look for log, count number of variables for goals
## (2) total aliens saved (overall, and perhaps also per session)
## (3) whether they met their stated goal for each level (1, 0)


# Get and set your working directory
getwd()
#setwd('C:/Users/...')

# Import data from the file in your working directory
agOutput <- read.csv('ag_output_raw.csv', header=TRUE, stringsAsFactor = FALSE, fill=TRUE)


### Part 1: Highest level achieved per user
# Create new dataframe with only the max levels shown per participant
ag_highestLevelCompleted = setNames(aggregate(agOutput$level ~ agOutput$studyID, agOutput, max),c('studyID','highestLevel'))
 

if (agOutput$level == ag_highestLevelCompleted) {
  ag_lastLevelRT = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='reactionti']~agOutput$studyID[agOutput$goalType=='reactionti'],agOutput,mean),c('studyID','ag_lastLevelRT'))
}

if (agOutput$level == ag_highestLevelCompleted) {
  ag_lastLevelAccuracy = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='percent']~agOutput$studyID[agOutput$goalType=='percent'],agOutput,mean),c('studyID','ag_lastLevelAccuracy'))
}

### Part 2: Total number of saves achieved per user

## 2.a: Total number saves per session overall (across all users)
totSavedSes = setNames(aggregate(agOutput$hitResult=='SAVED'~agOutput$session,agOutput,sum),c('session','totAlienSaved'))


## 2.b: Total number saves per user overall
totSavedUser = setNames(aggregate(agOutput$hitResult=='SAVED'~agOutput$studyID,agOutput,sum),c('studyID','totAlienSaved'))



# This line below shows the number of alien saves per user per session (Note that session could be easily replaced with 'leve' if that were more of interest.)
totSavedUserSes = setNames(aggregate(agOutput$hitResult=='SAVED'~agOutput$session+agOutput$studyID,agOutput,sum),c('session','studyID','totAlienSaved'))



## Part 3: Goals achieved per user 

## 3.a Create a sum of the number of goals for accuracy (aka percent) achieved per user
totGoalAchAcc = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='percent'] >= agOutput$goalSetAt[agOutput$goalType=='percent']~agOutput$studyID[agOutput$goalType=='percent'],agOutput,sum),c('studyID','totGoalAchAcc'))


# Create a sum of the number of goals for accuracy (aka percent) failed per user
totGoalFailAcc = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='percent'] < agOutput$goalSetAt[agOutput$goalType=='percent']~agOutput$studyID[agOutput$goalType=='percent'],agOutput,sum),c('studyID','totGoalFailAcc'))


# Create a sum of the number of goals for speed (aka reaction time) achieved per user
totGoalAchRT = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='reactionti'] <= agOutput$goalSetAt[agOutput$goalType=='reactionti']~agOutput$studyID[agOutput$goalType=='reactionti'],agOutput,sum),c('studyID','totGoalAchRT'))


# Create a sum of the number of goals for speed (aka reaction time) failed per user
totGoalFailRT = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='reactionti'] > agOutput$goalSetAt[agOutput$goalType=='reactionti']~agOutput$studyID[agOutput$goalType=='reactionti'],agOutput,sum),c('studyID','totGoalFailRT'))

# Create a ratio of the accuracy and RT goals achieved for the total number of goals attempted for each user overall
totGoalUser <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ag_highestLevelCompleted, ag_lastLevelRT, ag_lastLevelAccuracy, totSavedUser, totGoalAchAcc, totGoalFailAcc, totGoalAchRT, totGoalFailRT))

totGoalUser['ratiolevGoalAchAccUser'] <- NA
totGoalUser['ratiolevGoalAchAccUser'] <- totGoalUser$totGoalAchAcc/(totGoalUser$totGoalAchAcc+totGoalUser$totGoalFailAcc)

totGoalUser['ratiolevGoalAchRTUser'] <- NA
totGoalUser['ratiolevGoalAchRTUser'] <- totGoalUser$totGoalAchRT/(totGoalUser$totGoalAchRT+totGoalUser$totGoalFailRT)

totGoalUser['ratiolevGoalAchALLUser'] <- NA
totGoalUser['ratiolevGoalAchALLUser'] <- (totGoalUser$totGoalAchAcc+totGoalUser$totGoalAchRT)/(totGoalUser$totGoalAchAcc+totGoalUser$totGoalAchRT+totGoalUser$totGoalFailAcc+totGoalUser$totGoalFailRT)



## 3.b Goals achieved per user per level
# Create a sum of the accuracy goals for each level per user
levGoalAchAccUser = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='percent'] >= agOutput$goalSetAt[agOutput$goalType=='percent']~agOutput$level[agOutput$goalType=='percent']+agOutput$studyID[agOutput$goalType=='percent'],agOutput,sum),c('level','studyID','goalAchievementAcc'))
levGoalFailAccUser = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='percent'] < agOutput$goalSetAt[agOutput$goalType=='percent']~agOutput$level[agOutput$goalType=='percent']+agOutput$studyID[agOutput$goalType=='percent'],agOutput,sum),c('level','studyID','goalFailedAcc'))

# Create a sum of the reaction time goals for each level per user
levGoalAchRTUser = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='reactionti'] <= agOutput$goalSetAt[agOutput$goalType=='reactionti']~agOutput$level[agOutput$goalType=='reactionti']+agOutput$studyID[agOutput$goalType=='reactionti'],agOutput,sum),c('level','studyID','goalAchievementRT'))
levGoalFailRTUser = setNames(aggregate(agOutput$playerAchievement[agOutput$goalType=='reactionti'] > agOutput$goalSetAt[agOutput$goalType=='reactionti']~agOutput$level[agOutput$goalType=='reactionti']+agOutput$studyID[agOutput$goalType=='reactionti'],agOutput,sum),c('level','studyID','goalFailedRT'))

# Create a ratio of the accuracy and RT goals achieved for the total number of goals attempted for each user per level
levGoalUser <- Reduce(function(x, y) merge(x, y, all=TRUE), list(levGoalAchAccUser, levGoalFailAccUser, levGoalAchRTUser, levGoalFailRTUser))

levGoalUser['ratiolevGoalAchAccUser'] <- NA
levGoalUser['ratiolevGoalAchAccUser'] <- levGoalUser$goalAchievementAcc/(levGoalUser$goalAchievementAcc+levGoalUser$goalFailedAcc)

levGoalUser['ratiolevGoalAchRTUser'] <- NA
levGoalUser['ratiolevGoalAchRTUser'] <- levGoalUser$goalAchievementRT/(levGoalUser$goalAchievementRT+levGoalUser$goalFailedRT)

levGoalUser['ratiolevGoalAchALLUser'] <- NA
levGoalUser['ratiolevGoalAchALLUser'] <- (levGoalUser$goalAchievementAcc+levGoalUser$goalAchievementRT)/(levGoalUser$goalAchievementAcc+levGoalUser$goalAchievementRT+levGoalUser$goalFailedAcc+levGoalUser$goalFailedRT)


## Write to a csv
### Data per user
write.csv(totGoalUser,file='AGgoalsPerUser')


### Data per session user
write.csv(levGoalUser,file='AGgoalsPerSesUser')
