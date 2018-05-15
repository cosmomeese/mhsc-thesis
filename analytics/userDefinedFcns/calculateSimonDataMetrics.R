### Calculate Further Metrics for Simon's Data
#Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

#local functions

# intervals for MET_Groups
MET_INTERVALS <- setNames(c(0, # 0 <= METs < 2 (for NYHA IV), N.B. below 0 is grouped seperately
                            2, # 2 <= MET < 5 (for NYHA III)
                            5, # 5 <= MET < 7 (for NYHA II)
                            7), # 7 < MET (for NYHA I)
                          c("IV",
                            "III",
                            "II",
                            "I"))

# converts Steps to METs (metabolic equivalent)
# valuesed based on: https://www.ajpmonline.org/article/S0749-3797(09)00087-7/fulltext
# 
# if invert = FALSE: convert steps to METs
# x: steps/min
# output: Y estimate of METS
#         for men: Y=0.061*x-2.597 
#         for women: Y=0.042*x-0.82
#         for unknown: average of men + women
# 
# if invert = TRUE: convert METs to steps
# x: METS
# output: Y estimate of equivalent steps/min
#         for men: Y=(x+2.597)/0.061 
#         for women: Y=(x+0.82)/0.042
#         for unknown: average of men + women
# v1.1
convertStepsMETs <- function(x, isMale=TRUE, invert=FALSE) {

  mMale <- 0.061
  bMale <- -2.597
  mFemale <- 0.042
  bFemale <- -0.82
  
  if(!is.na(isMale))
  {
    if(isMale)
    {
      m <- mMale
      b <- bMale
    }
    else if(!isMale) #i.e. is female
    {
      m <- mFemale
      b <- bFemale
    }
  }
  else #e.g. is N.A.
  {
    m <- (mMale + mFemale)/2
    b <- (bMale + bFemale)/2
  }
  
  if(!invert)
  {
    yhat = m*x + b
  }
  else
  {
    yhat = (x-b)/m
  }
  
  return(yhat)
}

# calculate longest positive (negative if negate = TRUE) streak in x
# modified from: https://stackoverflow.com/a/4656018
#
# e.g. use for data frame
# for dataframe with columns: Instrument + TradeResult.Currency
# interested in longest streak of TradeResult.Currency for each instrument
# wins <- lapply(split(subRes[,2],subRes[,1]), FUN)
# loses <- lapply(split(subRes[,2],subRes[,1]), FUN, negate = TRUE)
longestStreak <- function(x, negate = FALSE, na.rm = FALSE) {
  rles <- rle(x > 0)
  result <- 0
  if(length(rles) > 0)
  {
    if(negate) {
      result <- max(rles$lengths[!rles$values], na.rm = na.rm)
    } else {
      result <- max(rles$lengths[rles$values], na.rm = na.rm)
    }
  }
  return(result)
}


#N.B. returns the most frequent element or the first one for multi-modals unique
Statistical.Mode <- function(x) { #http://stackoverflow.com/a/8189441
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculateSimonDataMetrics <- function(processedData)
{
  STEPDATA.METCLASSPREFIX <- "StepData.METClass"
  ### for each Participant/Sheet
  oldNumVars <- length(processedData)
  
  for(participant_Index in 1:nrow(processedData)) 
  { #for each participant
    oldParticipantStepData <- processedData$StepData[[participant_Index]]
    isMale <- "M" == processedData$Sex[[participant_Index]]
    participantStepSeries <- data.frame()
    if(!(is.null(oldParticipantStepData) || 
         is.na(oldParticipantStepData)))
    {
      for(day_Index in 1:nrow(oldParticipantStepData))
      {
        IntraDay.InstanceOf <- oldParticipantStepData$Intraday[[day_Index]]
        participantStepSeries <- rbind(participantStepSeries, IntraDay.InstanceOf)
        
        #Some statistics that we need
        
        #For the day
        processedData$StepData[[participant_Index]][day_Index,"Total"] <- sum(IntraDay.InstanceOf$Steps)
        processedData$StepData[[participant_Index]][day_Index,"Mean"]  <- mean(IntraDay.InstanceOf$Steps)
        processedData$StepData[[participant_Index]][day_Index,"StdDev"] <- sd(IntraDay.InstanceOf$Steps)
        
        FiveNumSummary <- fivenum(IntraDay.InstanceOf$Steps)
        processedData$StepData[[participant_Index]][day_Index,"Max"]  <- FiveNumSummary[[5]] #Max
        processedData$StepData[[participant_Index]][day_Index,"Q3"]  <- FiveNumSummary[[4]] #3rd Quartile
        processedData$StepData[[participant_Index]][day_Index,"Median"]  <- FiveNumSummary[[3]] #Median
        processedData$StepData[[participant_Index]][day_Index,"Q1"]  <-  FiveNumSummary[[2]] #1st Quartile
        processedData$StepData[[participant_Index]][day_Index,"Min"]  <- FiveNumSummary [[1]] #Min
        
        processedData$StepData[[participant_Index]][day_Index,"Mode"]  <- Statistical.Mode(IntraDay.InstanceOf$Steps)
        
        #per day v1.1 additions #####
        processedData$StepData[[participant_Index]][day_Index,"StepData.ActiveMinutes_Pure"] <- sum(IntraDay.InstanceOf$Steps > 0)
        
         #add count of MET from steps. Levels from Australian CHF Guidelines
         #https://www.heartfoundation.org.au/images/uploads/publications/Chronic_Heart_Failure_Guidelines_2011.pdf
        
        METGroups <-  findInterval(IntraDay.InstanceOf$Steps,
                                   convertStepsMETs(MET_INTERVALS,isMale,TRUE))
        
        processedData$StepData[[participant_Index]][day_Index,"StepData.ActiveMinutes_PosMET"] <- sum(METGroups > 0)

        belowMinSuffix <- ".BelowMin"
        for(indexMET in 0:length(MET_INTERVALS))
        {
          name <- STEPDATA.METCLASSPREFIX
          isMETInterval <- indexMET > 0
          if(isMETInterval)
          {
            name <- paste(name,
                          names(MET_INTERVALS)[[indexMET]],
                          sep="")
          }
          else
          {
            name <- paste(name,
                          belowMinSuffix,
                          sep="")
          }
          percentageName <- paste(name,".Percentage",sep="")
          percentageAllName <- paste(name,".PercentageAll",sep="")
          # calculate metric
          sumForGroup <- sum(indexMET == METGroups)
          sumForAllGroups <- sum(METGroups > 0)
          percentageForGroup <- sumForGroup / sumForAllGroups
          if(is.na(sumForAllGroups) || 0 == sumForAllGroups) # i.e. percentage yields NaN (no steps)
          {
            #if(isMETInterval) #i.e. is named class
            #{
            #  percentageForGroup <- 0 # i.e. 0% of steps fall in this group
            #}
            #else
            #{
            #  percentageForGroup <- 1 # i.e. 0% of steps fall into the 'BelowMin' group
            #}
            percentageForGroup <- NA
          }
          
          percentageForAll <- sumForGroup / length(METGroups)
          
          processedData$StepData[[participant_Index]][day_Index,name] <- sumForGroup
          if(isMETInterval)
          {
            processedData$StepData[[participant_Index]][day_Index,percentageName] <- percentageForGroup
          }
          processedData$StepData[[participant_Index]][day_Index,percentageAllName] <- percentageForAll
        }
      }
    }
    
    #Metrics that use the entire time series
    if(!(is.null(participantStepSeries) ||
         (length(participantStepSeries) < 1) ||
         is.na(participantStepSeries)))
    {
      #order the series by time stamp
      participantStepSeries <- participantStepSeries[order(participantStepSeries$Timestamp),]$Steps
      # get longest uninterupted streak
      processedData[participant_Index,"StepData.LongestActiveStreak"] <- longestStreak(participantStepSeries)
      
      # get highest valued uninterrupted streak
      splitSteps <- split(participantStepSeries[participantStepSeries>0],
                          cumsum(participantStepSeries==0)[participantStepSeries>0])
      processedData[participant_Index,"StepData.HighestValuedStreak"] <- max(unlist(lapply(splitSteps,sum)))
      
      
    }
    
    
    #Averaged over the day
    updatedParticipantStepData <- processedData$StepData[[participant_Index]]
    if(!(is.null(updatedParticipantStepData) || 
         is.na(updatedParticipantStepData)))
    { #i.e. if StepData is available
      processedData[participant_Index,"StepData.MeanDailyTotalSteps"] <- mean(updatedParticipantStepData[['Total']])
      processedData[participant_Index,"StepData.StdDevDailyTotalSteps"] <- sd(updatedParticipantStepData[['Total']])
      processedData[participant_Index,"StepData.MeanDailyMeanSteps"] <- mean(updatedParticipantStepData[['Mean']])
      processedData[participant_Index,"StepData.StdDevDailyMeanSteps"] <- sd(updatedParticipantStepData[['Mean']])
      processedData[participant_Index,"StepData.ModeDailyModeSteps"] <- Statistical.Mode(updatedParticipantStepData[['Mode']])
      processedData[participant_Index,"StepData.MeanDailyMinSteps"] <- mean(updatedParticipantStepData[['Min']])
      processedData[participant_Index,"StepData.MeanDailyMaxSteps"] <- mean(updatedParticipantStepData[['Max']])
      processedData[participant_Index,"StepData.ModeDailyMinSteps"] <- Statistical.Mode(updatedParticipantStepData[['Min']])
      processedData[participant_Index,"StepData.ModeDailyMaxSteps"] <- Statistical.Mode(updatedParticipantStepData[['Max']])
      processedData[participant_Index,"StepData.MaxDailyMaxSteps"] <- max(updatedParticipantStepData[['Max']])


      #averaged over day v1.1 additions #####
       # add the some of the missing, possibly important, raw step value calculations
      processedData[participant_Index,"StepData.MeanDailyMaxSteps"] <- mean(updatedParticipantStepData[['Max']])
      processedData[participant_Index,"StepData.StdDevDailyMaxSteps"] <- sd(updatedParticipantStepData[['Max']])
       
       # add the MET derived features
      matchPrefix <- "StepData."
      pDataCNames <- grep(matchPrefix, colnames(updatedParticipantStepData), value = TRUE)
      for(colName in pDataCNames)
      {
        withoutPrefix <- sub(matchPrefix,"",colName)
        if(grepl("Percentage",withoutPrefix))
        { # if a percentage just calculate mean
          processedData[participant_Index,paste(matchPrefix,"Overall",withoutPrefix,sep="")] <- mean(updatedParticipantStepData[[colName]], na.rm = TRUE)
        }
        else
        {
          processedData[participant_Index,paste(matchPrefix,"Total",withoutPrefix,sep="")] <- sum(updatedParticipantStepData[[colName]])
          processedData[participant_Index,paste(matchPrefix,"Mean",withoutPrefix,sep="")] <- mean(updatedParticipantStepData[[colName]])
          processedData[participant_Index,paste(matchPrefix,"StdDev",withoutPrefix,sep="")] <- sd(updatedParticipantStepData[[colName]])
          processedData[participant_Index,paste(matchPrefix,"Mode",withoutPrefix,sep="")] <- Statistical.Mode(updatedParticipantStepData[[colName]])
          processedData[participant_Index,paste(matchPrefix,"Max",withoutPrefix,sep="")] <- max(updatedParticipantStepData[[colName]])
        }

      }
    }
  }
  
  #shunt the stepData column to the back
  newNumVars <- length(processedData)
  #order as: first #[oldNumVars-1] (to account for Step Data Column) then, skip the StepData Column and grab the rest, and lastly the StepData Column
  processedData <- processedData[c(1:(oldNumVars-1),(oldNumVars+1):newNumVars,oldNumVars)]

  return(processedData)
}

