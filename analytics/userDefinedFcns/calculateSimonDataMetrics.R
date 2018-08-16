### Calculate Further Metrics for Simon's Data
#Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries
require(tidyverse)
require(e1071) 

simonExtraMetrics.CodeVersion <- "1.4"

#local functions

#### define MET$INTERVALS ####
# intervals for MET$Groups
MET <- list()
MET$BASELINE <- 0 # consider 0 (if MET calc is assumed to include base metabolic rate) or 1 (if it doesn't)
MET$INTERVALS <- setNames(-MET$BASELINE + c(0, # 0 <= METs < 1, N.B. below 0 is grouped seperately
                                            1, # 1 <= METs < 1.6, (roughly below NYHA IV)
                                            1.6, # 1.6 <= METS < 2, (for NYHA IV) 
                                            2, # 2 <= MET < 3 (for NYHA III)
                                            3, # 3 <= MET < 5 (for NYHA III)
                                            5, # 5 <= MET < 7 (for NYHA II)
                                            7), # 7 < MET (for NYHA I)
                          c("Negligible", # 0 <= METs < 1, N.B. below 0 is grouped seperately
                            "Sedentary", # 1 <= METs < 1.6, (roughly below NYHA IV)
                            "VeryLight", # 1.6 <= METS < 2, (for NYHA IV) 
                            "Light", # 2 <= MET < 3 (for NYHA III)
                            "LowModerate", # 3 <= MET < 5 (for NYHA III)
                            "Moderate", # 5 <= MET < 7 (for NYHA II)
                            "Vigorous")) # 7+)
#MET$INTERVALS <- setNames(c(0, # 0 <= METs < 2 (for NYHA IV), N.B. below 0 is grouped seperately
#                            2, # 2 <= MET < 5 (for NYHA III)
#                            5, # 5 <= MET < 7 (for NYHA II)
#                            7), # 7 < MET (for NYHA I)
#                          c("IV",
#                            "III",
#                            "II",
#                            "I"))

#### define STEP$INTERVALS ####
STEP <- list()
STEP$BASELINE <- 0
STEP$INTERVAL_MAX <- 10 # in thousands of steps
STEP$INTEVAL_FACTOR <- 1000
STEP$INTERVAL_NAME_SUFFIX <- "Steps"
STEP$INTERVALS <- seq(0,STEP$INTERVAL_MAX,by=0.5)*STEP$INTEVAL_FACTOR # in thousands of steps
names(STEP$INTERVALS) <- paste0(STEP$INTERVALS,STEP$INTERVAL_NAME_SUFFIX)

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
Statistical.Mode <- function(x, na.rm=FALSE) { #http://stackoverflow.com/a/8189441
  
  if(na.rm) # then remove NAs
  {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#N.B. returns standard error
std.err <- function(x, na.rm=FALSE) {
  
  if(na.rm) # then remove NAs
  {
    x <- x[!is.na(x)]
  }
  se <- sd(x,na.rm=na.rm)/sqrt(length(x))
  return(se)
}

calculateSimonDataMetrics <- function(processedData)
{
  STEPDATA.METCLASSPREFIX <- "StepData.METClass"
  ### for each Participant/Sheet
  oldNumVars <- length(processedData)
  hasWarnedTime <- FALSE
  
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
        processedData$StepData[[participant_Index]][day_Index,"StdErr"] <- std.err(IntraDay.InstanceOf$Steps)
        
        processedData$StepData[[participant_Index]][day_Index,"Skewness"] <- e1071::skewness(IntraDay.InstanceOf$Steps)
        processedData$StepData[[participant_Index]][day_Index,"Kurtosis"] <- e1071::kurtosis(IntraDay.InstanceOf$Steps)
        
        
        FiveNumSummary <- fivenum(IntraDay.InstanceOf$Steps)
        processedData$StepData[[participant_Index]][day_Index,"Maximum"]  <- FiveNumSummary[[5]] #Max
        processedData$StepData[[participant_Index]][day_Index,"Q3"]  <- FiveNumSummary[[4]] #3rd Quartile
        processedData$StepData[[participant_Index]][day_Index,"Median"]  <- FiveNumSummary[[3]] #Median
        processedData$StepData[[participant_Index]][day_Index,"Q1"]  <-  FiveNumSummary[[2]] #1st Quartile
        processedData$StepData[[participant_Index]][day_Index,"Minimum"]  <- FiveNumSummary [[1]] #Min
        processedData$StepData[[participant_Index]][day_Index,"IQR"]  <- FiveNumSummary[[4]] - FiveNumSummary[[2]] #Min
        
        
        processedData$StepData[[participant_Index]][day_Index,"Mode"]  <- Statistical.Mode(IntraDay.InstanceOf$Steps)
        
        
        #per day v1.1 additions #####
        processedData$StepData[[participant_Index]][day_Index,"StepData.ActiveMinutes_Pure"] <- sum(IntraDay.InstanceOf$Steps > 0)
        
        #add count of MET from steps. Levels from Australian CHF Guidelines
        #https://www.heartfoundation.org.au/images/uploads/publications/Chronic_Heart_Failure_Guidelines_2011.pdf
        
        METGroups <-  findInterval(IntraDay.InstanceOf$Steps,
                                   convertStepsMETs(MET$INTERVALS,isMale,TRUE))
        
        processedData$StepData[[participant_Index]][day_Index,"StepData.ActiveMinutes_PosMET"] <- sum(METGroups > 0)
        
        belowMinSuffix <- ".BelowMin"
        for(indexMET in 0:length(MET$INTERVALS))
        {
          name <- STEPDATA.METCLASSPREFIX
          isMETInterval <- indexMET > 0
          if(isMETInterval)
          {
            name <- paste(name,
                          names(MET$INTERVALS)[[indexMET]],
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
      #!!!!! N.B. you can't do this 1) TimeStamp doesn't exist for SimonData & 2) only Time exists & sorting by this will mess everything up
      #order the series by time stamp
      #participantStepSeries <- participantStepSeries[order(participantStepSeries$Timestamp),]
      #!!!!! N.B. above
      if(!hasWarnedTime)
      {
        if("Timestamp" %in% names(participantStepSeries))
        {
          msg <- 'Timestamp exists in participant step series but did not sort'
          warning(msg)
          hasWarnedTime <- TRUE
        }
        else
        {
          msg <- "Timestamp does not exist in StepData; could not sort"
          if('Time' %in% names(participantStepSeries))
          {
            msg <- paste0(msg, " (Time exists, but this alone does not help)")
          }
          warning(msg)
          hasWarnedTime <- TRUE
        }
      }
      
      participantSteps <- participantStepSeries$Steps
      # get longest uninterupted streak
      processedData[participant_Index,"StepData.LongestActiveStreak"] <- longestStreak(participantSteps)
      
      # get highest valued uninterrupted streak
      splitSteps <- split(participantSteps[participantSteps>0],
                          cumsum(participantSteps==0)[participantSteps>0])
      processedData[participant_Index,"StepData.HighestValuedStreak"] <- max(unlist(lapply(splitSteps,sum)))
      
      #### v1.2 stats ####
      processedData.BasicStats <- calculateBasicStatisticalMetrics(participantStepSeries,
                                                        COL_NAME="Steps",
                                                        SAVE_COL_PREFIX="Overall",
                                                        removeNAFlag=TRUE,
                                                        SEP="")
      names(processedData.BasicStats) <- paste0("StepData.",names(processedData.BasicStats))
      for(name in names(processedData.BasicStats))
      {
        processedData[participant_Index,name] <- processedData.BasicStats[[name]]
      }
      
      #processedData[participant_Index,"StepData.OverallSkewness"] <- e1071::skewness(participantSteps)
      #processedData[participant_Index,"StepData.OverallKurtosis"] <- e1071::kurtosis(participantSteps)
      #### v1.2 stats
      
      #### v1.3 stats ####
      processedData[participant_Index,"StepData.OverallTotal"] <- sum(participantSteps)
      processedData[participant_Index,"StepData.OverallMean"]  <- mean(participantSteps)
      processedData[participant_Index,"StepData.OverallStdDev"] <- sd(participantSteps)
      processedData[participant_Index,"StepData.OverallStdErr"] <- std.err(participantSteps)
      processedData[participant_Index,"StepData.OverallSkewness"] <- e1071::skewness(participantSteps)
      processedData[participant_Index,"StepData.OverallKurtosis"] <- e1071::kurtosis(participantSteps)
      
      
      FiveNumSummary <- fivenum(participantSteps)
      processedData[participant_Index,"StepData.OverallMaximum"]  <- FiveNumSummary[[5]] #Max
      processedData[participant_Index,"StepData.OverallQ3"]  <- FiveNumSummary[[4]] #3rd Quartile
      processedData[participant_Index,"StepData.OverallMedian"]  <- FiveNumSummary[[3]] #Median
      processedData[participant_Index,"StepData.OverallQ1"]  <-  FiveNumSummary[[2]] #1st Quartile
      processedData[participant_Index,"StepData.OverallMinimum"]  <- FiveNumSummary [[1]] #Min
      processedData[participant_Index,"StepData.OverallIQR"]  <- FiveNumSummary[[4]] - FiveNumSummary [[2]] #Min
      
      
      processedData[participant_Index,"StepData.OverallMode"]  <- Statistical.Mode(participantSteps)
      
      #### v1.3 stats
    }
    
    
    #Averaged over the day
    updatedParticipantStepData <- processedData$StepData[[participant_Index]]
    if(!(is.null(updatedParticipantStepData) || 
         is.na(updatedParticipantStepData)))
    { #i.e. if StepData is available
      
      dailySummaryNames <- c('Total','Maximum','Q3','Median','Mean','StdDev','StdErr','Q1','Minimum','Mode','IQR','Skewness','Kurtosis')
      noStepsSuffix <- c('IQR','Skewness','Kurtosis')
      superSummaryFcnMappings <- list(Maximum=max,Mean=mean,Mode=Statistical.Mode,Minimum=min,StdDev=sd,StdErr=std.err)
      
      na.remove <- FALSE # default for all functions
      for(summaryName in dailySummaryNames)
      {
        for(superSummaryIdx in seq_along(superSummaryFcnMappings))
        {
          superSummaryFcn <- superSummaryFcnMappings[[superSummaryIdx]]
          superSummaryPrefix <- names(superSummaryFcnMappings)[superSummaryIdx]
          finalSuffix <- 'Steps'
          if(summaryName %in% noStepsSuffix) #i.e. if one of these (namely Skewness and Kurtosis) then don't add 'Steps' to the end
          {
            finalSuffix <- ''
          }
          saveName <- paste0('StepData.',superSummaryPrefix,'Daily',summaryName,finalSuffix)
          #e.g. in the form: processedData[participant_Index,"StepData.MeanDailyTotalSteps"] <- mean(updatedParticipantStepData[['Total']])
          processedData[participant_Index,saveName] <- superSummaryFcn(updatedParticipantStepData[[summaryName]],na.rm=na.remove)
        }
      }
      
      #processedData[participant_Index,"StepData.MeanDailyTotalSteps"] <- mean(updatedParticipantStepData[['Total']])
      #processedData[participant_Index,"StepData.StdDevDailyTotalSteps"] <- sd(updatedParticipantStepData[['Total']])
      #processedData[participant_Index,"StepData.MeanDailyMeanSteps"] <- mean(updatedParticipantStepData[['Mean']])
      #processedData[participant_Index,"StepData.StdDevDailyMeanSteps"] <- sd(updatedParticipantStepData[['Mean']])
      #processedData[participant_Index,"StepData.ModeDailyModeSteps"] <- Statistical.Mode(updatedParticipantStepData[['Mode']])
      #processedData[participant_Index,"StepData.MeanDailyMinSteps"] <- mean(updatedParticipantStepData[['Min']])
      #processedData[participant_Index,"StepData.MeanDailyMaxSteps"] <- mean(updatedParticipantStepData[['Max']])
      #processedData[participant_Index,"StepData.ModeDailyMinSteps"] <- Statistical.Mode(updatedParticipantStepData[['Min']])
      #processedData[participant_Index,"StepData.ModeDailyMaxSteps"] <- Statistical.Mode(updatedParticipantStepData[['Max']])
      #processedData[participant_Index,"StepData.MaxDailyMaxSteps"] <- max(updatedParticipantStepData[['Max']])
      
      
      #averaged over day v1.1 additions #####
      # add the some of the missing, possibly important, raw step value calculations
      #processedData[participant_Index,"StepData.MeanDailyMaxSteps"] <- mean(updatedParticipantStepData[['Max']])
      #processedData[participant_Index,"StepData.StdDevDailyMaxSteps"] <- sd(updatedParticipantStepData[['Max']])
      
      #averaged over day v1.2 additions #####
      # add the some of the missing, possibly important, raw step value calculations
      #processedData[participant_Index,"StepData.MeanDailyStdDevSteps"] <- mean(updatedParticipantStepData[['StdDev']], na.rm=TRUE)
      #processedData[participant_Index,"StepData.MeanDailySkewness"] <- mean(updatedParticipantStepData[['Skewness']], na.rm=TRUE)
      #processedData[participant_Index,"StepData.MeanDailyKurtosis"] <- mean(updatedParticipantStepData[['Kurtosis']], na.rm=TRUE)
      
      #continue v1.1 additions
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

#TODO: N.B. problems occur if one of the participants doesn't have a data column. This is unlikely to happen
#      so I didn't protect for it but if it does fundamentally it will cause this line:
#      processedData <- cbind(processedData,series.newColumns) to have mismatched dimension arguments
#      and it won't be able to figure out what to do. Suggest adding a UUID to merge them on later.
calculateFitbitDataMetrics <- function(processedData,
                                       unworn.mode=4)
{
  # TODO: update with v1.2 metrics
  # overall basic stats + overall & day-by-day skewness and kurtosis
  stop('Not updated with v1.2 metrics')
  # 
  SEP_CHAR="."
  DATA_COL_NAME <- "FitbitData"
  INTRADAY_COL_NAME <- "Intraday"
  STEPS_COL_NAME <- "Steps"
  STEPS_DAILY_SAVE_COL_PREFIX <- "Steps"
  STEPS_SAVE_COL_PREFIX <- "StepData"
  HR_COL_NAME <- "HeartRate"
  HR_DAILY_SAVE_COL_PREFIX <- "HeartRate"
  HR_SAVE_COL_PREFIX <- "HeartRateData"
  TIMESTAMP_COL_NAME <- "DateTime"
  MET_KWD <- "MET"
  ACTIVESTEPS_KWD <- "ActiveSteps"
  SEX_COL_NAME <- "Sex"
  oldNumVars <- length(processedData)
  removeNAFlag <- TRUE
  sexColExists <- SEX_COL_NAME %in% names(processedData)
  if(!sexColExists)
  {
    warning("No column for participant sex; using average formula")
    isMale <- NA
  }
  
  # for later
  commonDailyActiveMinutesFcn <- function(cum.sum,
                                          bl,
                                          at.keywd,
                                          interval,
                                          tu,
                                          # and the common ones
                                          iddf=intraDayData,
                                          cname=STEPS_COL_NAME,
                                          savepfx=STEPS_SAVE_COL_PREFIX,
                                          tcname=TIMESTAMP_COL_NAME,
                                          naflag=removeNAFlag,
                                          spchr=SEP_CHAR)
  {
    result <- calculateDailyActiveMinutes(iddf,
                                          cum.sum=cum.sum,
                                          BASELINE=bl,
                                          ACTIVITY_TYPE_KEYWORD=at.keywd,
                                          GROUP_INTERVALS=interval,
                                          COL_NAME=cname,
                                          SAVE_COL_PREFIX=savepfx,
                                          TIMESTAMP_COL_NAME=tcname,
                                          TIME_UNIT=tu,
                                          removeNAFlag=naflag,
                                          SEP=spchr)
    return(result)
  }
  
  ### for each Participant/Sheet
  firstParticipant <- TRUE
  series.newColumns <- data.frame()
  for(participant_Index in seq(nrow(processedData))) 
  { #for each participant
    # get their Fitbit data
    cat("Participant: ",participant_Index,"/",nrow(processedData),"\n")
    oldParticipantFitbitData <- processedData[[DATA_COL_NAME]][[participant_Index]]
    
    # if possible, get their sex (for Step METs)
    if(sexColExists)
    {
      isMale <- "M" == processedData[[SEX_COL_NAME]][[participant_Index]]
    }
    
    # create data frame to hold combined data series
    participantFBDataSeries <- data.frame()
    if(isSeriesNotEmpty(oldParticipantFitbitData,INTRADAY_COL_NAME))
    {
      firstDay <- TRUE
      for(day_Index in seq(nrow(oldParticipantFitbitData)))
      {
        IntraDay.InstanceOf <- oldParticipantFitbitData[[INTRADAY_COL_NAME]][[day_Index]]
        # clear times when Fitbit was not worn
        IntraDay.InstanceOf <- IntraDay.InstanceOf %>% convertUnWornValuesToNA(mode=unworn.mode,
                                                                               stepColName=STEPS_COL_NAME,
                                                                               hrColName=HR_COL_NAME)
        participantFBDataSeries <- rbind(participantFBDataSeries, IntraDay.InstanceOf)
        
        #### Metrics that use the entire daily values ####
        stepDayStatistics <- calculateBasicStatisticalMetrics(intraDayData=IntraDay.InstanceOf,
                                                              COL_NAME=STEPS_COL_NAME,
                                                              SAVE_COL_PREFIX=STEPS_DAILY_SAVE_COL_PREFIX,
                                                              removeNAFlag=removeNAFlag,
                                                              SEP=SEP_CHAR)

        hrDayStatistics <- calculateBasicStatisticalMetrics(intraDayData=IntraDay.InstanceOf,
                                                            COL_NAME=HR_COL_NAME,
                                                            SAVE_COL_PREFIX=HR_DAILY_SAVE_COL_PREFIX,
                                                            removeNAFlag=removeNAFlag,
                                                            SEP=SEP_CHAR)
        
        #per day v1.1 + 1.2 additions #####

        convertedMETStepIntervals <- convertStepsMETs(MET$INTERVALS,isMale,TRUE)
        metStatistics <- commonDailyActiveMinutesFcn(cum.sum=FALSE,
                                                     bl=MET$BASELINE,
                                                     at.keywd=MET_KWD,
                                                     interval=convertedMETStepIntervals,
                                                     tu='min')
        
        combinedStatistics <- cbind(stepDayStatistics,hrDayStatistics,metStatistics)
        if(firstDay)
        {
          IntraDay.newColumns <- combinedStatistics
          firstDay <- FALSE
        } else
        {
          IntraDay.newColumns <- rbind(IntraDay.newColumns,
                                       combinedStatistics)
        }
      } # loop /day
      
      # combine with original sub data frame
      newParticipantFitbitData <- cbind(oldParticipantFitbitData,IntraDay.newColumns)
      # save to original data frame
      processedData[[DATA_COL_NAME]][[participant_Index]] <- newParticipantFitbitData
      
      #### Metrics that use the entire time series ####
      
      intradayOverallMetrics <- calculateOverallSummaries(IntraDay.newColumns,
                                                          removeNAFlag=removeNAFlag,
                                                          SEP=SEP_CHAR) # yup, new columns not the combined yet
      
      stepActiveStatistics <- commonDailyActiveMinutesFcn(iddf=participantFBDataSeries,
                                                          cum.sum=TRUE,
                                                          bl=STEP$BASELINE,
                                                          at.keywd=ACTIVESTEPS_KWD,
                                                          interval=STEP$INTERVALS,
                                                          tu='day')
      
      seriesStepStats <- calculateSeriesStepMetrics(participantFBDataSeries,
                                                    COL_NAME=STEPS_COL_NAME,
                                                    TIMESTAMP_COL_NAME=TIMESTAMP_COL_NAME,
                                                    SAVE_COL_PREFIX=STEPS_SAVE_COL_PREFIX,
                                                    removeNAFlag,
                                                    SEP=SEP_CHAR)
      
      seriesHRStats <- calculateSeriesHRMetrics(participantFBDataSeries,
                                                HR_COL_NAME=HR_COL_NAME,
                                                STEPS_COL_NAME=STEPS_COL_NAME,
                                                TIMESTAMP_COL_NAME=TIMESTAMP_COL_NAME,
                                                SAVE_COL_PREFIX=HR_SAVE_COL_PREFIX,
                                                removeNAFlag,
                                                SEP=SEP_CHAR)
      
      combinedSeriesStatistics <- cbind(intradayOverallMetrics,stepActiveStatistics,seriesStepStats)
      if(0 < nrow(seriesHRStats)) # since this one won't produce metrics if the series contains no Step or HR data
      {
        combinedSeriesStatistics <- cbind(combinedSeriesStatistics,seriesHRStats)
      }
      if(1 > nrow(combinedSeriesStatistics))
      {
        browser()
        error("combined series statistics should only have 1 row; has", nrow(combinedSeriesStatistics), " for participant index ", participant_Index)
      }
      if(firstDay)
      {
        series.newColumns <- combinedSeriesStatistics
        firstDay <- FALSE
      } else
      {
        if(nrow(series.newColumns) == nrow(combinedSeriesStatistics))
        { # then we can do faster rbind operation
          series.newColumns <- rbind(series.newColumns,
                                     combinedSeriesStatistics)
        }
        else
        { # then we have to do by row match, but this is a much slower operation
          series.newColumns <- bind_rows(series.newColumns,
                                         combinedSeriesStatistics)
        }
        cat('    nrows:', nrow(series.newColumns),'\n')
        if(participant_Index < nrow(series.newColumns))
        {
          browser()
          error("row count mismatch")
        }
        
      }
    } # end make sure oldParticipantFitbitData exists
  } # loop /participant
  processedData <- cbind(processedData,series.newColumns)
  
  #shunt the heart data column to the back
  newNumVars <- length(processedData)
  #order as: first #[oldNumVars-1] (to account for HeartRate Column) then, skip the HeartRate Column and grab the rest, and lastly the HeartRate Column
  processedData <- processedData[c(1:(oldNumVars-1),(oldNumVars+1):newNumVars,oldNumVars)]
  
  return(processedData)
}

# N.B. also used for things other than just the intraDayData frame format
calculateBasicStatisticalMetrics <- function(intraDayData,
                                             COL_NAME="Steps",
                                             SAVE_COL_PREFIX="StepData",
                                             removeNAFlag,
                                             SEP=SEP_CHAR)
{
  result <- list()
  
  #For the day

  FiveNumSummary <- fivenum(intraDayData[[COL_NAME]],
                            na.rm= removeNAFlag)
  
  result[[paste0(SAVE_COL_PREFIX,SEP,"Maximum")]]  <- FiveNumSummary[[5]] #Max
  result[[paste0(SAVE_COL_PREFIX,SEP,"Q3")]]  <- FiveNumSummary[[4]] #3rd Quartile
  
  result[[paste0(SAVE_COL_PREFIX,SEP,"Mean")]] <- mean(intraDayData[[COL_NAME]],
                                                       na.rm=removeNAFlag)
  result[[paste0(SAVE_COL_PREFIX,SEP,"StdDev")]] <- sd(intraDayData[[COL_NAME]],
                                                       na.rm=removeNAFlag)
  result[[paste0(SAVE_COL_PREFIX,SEP,"Median")]]  <- FiveNumSummary[[3]] #Median
  result[[paste0(SAVE_COL_PREFIX,SEP,"Mode")]]  <- Statistical.Mode(intraDayData[[COL_NAME]],
                                                                    na.rm=removeNAFlag)
  result[[paste0(SAVE_COL_PREFIX,SEP,"Q1")]]  <-  FiveNumSummary[[2]] #1st Quartile
  result[[paste0(SAVE_COL_PREFIX,SEP,"Minimum")]]  <- FiveNumSummary [[1]] #Min
  
  result[[paste0(SAVE_COL_PREFIX,SEP,"Total")]] <- sum(intraDayData[[COL_NAME]],
                                                       na.rm=removeNAFlag)
  result[[paste0(SAVE_COL_PREFIX,SEP,"n")]] <- which(intraDayData[[COL_NAME]] %>% is.na() %>% !.) %>% length()
  
  result.df <- as.data.frame(result)
  return(result.df)
}

#N.B. BASELINE is subtracted (for historical reasons) from _PosActivity threshold (of 0)
calculateDailyActiveMinutes <- function(intraDayData,
                                        cum.sum=FALSE, # cumulative sum? i.e. calculate sums @ group + above or just @ group
                                        BASELINE,
                                        ACTIVITY_TYPE_KEYWORD,
                                        GROUP_INTERVALS,
                                        COL_NAME="Steps",
                                        SAVE_COL_PREFIX="StepData",
                                        TIMESTAMP_COL_NAME = 'DateTime',
                                        TIME_UNIT="min",
                                        removeNAFlag,
                                        SEP=SEP_CHAR)
{
  activityKeyWord <- paste0(ACTIVITY_TYPE_KEYWORD,SEP,TIME_UNIT)
  BELOW_MIN_KEYWORD <- "BelowMin"
  PERCENTAGE_GROUP_SUFFIX <- "PercentageOfGroups"
  PRECENTAGE_ALLTIME_SUFFIX <- "PercentageOfAllTime"
  PERCENT_FACTOR <- 100
  result <- list()
  CUMSUM_LABEL <- ""
  if(cum.sum)
  {
    CUMSUM_LABEL <- paste0(SEP,"GTE")
  }
  
  
  #add count of MET from steps. Levels from Australian CHF Guidelines
  #https://www.heartfoundation.org.au/images/uploads/publications/Chronic_Heart_Failure_Guidelines_2011.pdf
  
  rColName <- 'sum'
  perUnitTimeSeries <- summarizeToTimeUnit(df=IntraDay.InstanceOf,
                                           unit=TIME_UNIT,
                                           varCol=COL_NAME,
                                           originCol=TIMESTAMP_COL_NAME,
                                           resultCol=rColName)
    
  
  ActivityGroups <-  findInterval(perUnitTimeSeries[[rColName]],
                                  GROUP_INTERVALS)
  
  #### Active Minutes ####
  
  # N.B. ActiveMinutes_Pure is the same as total no?
  activeTimeName <- paste0(SAVE_COL_PREFIX,SEP,activityKeyWord,SEP,"GTE","0Baseline") # hard code since this is always GTE
  result[[activeTimeName]] <- sum(ActivityGroups > (0 - BASELINE),
                                  na.rm=removeNAFlag)
  
  #### Sum & Percentages of Variables ####
  # now for the groups

  # execution:
  for(groupIdx in 0:length(GROUP_INTERVALS))
  {
    #### generate the names ####
    name <- paste0(SAVE_COL_PREFIX,SEP,activityKeyWord,CUMSUM_LABEL)
    isAGroupInterval <- groupIdx > 0
    if(isAGroupInterval)
    { 
      name.suffix <- names(GROUP_INTERVALS)[[groupIdx]]
    }
    else
    {
      name.suffix <- BELOW_MIN_KEYWORD
    }
    name <- paste(name,name.suffix,sep=SEP)
    
    # For percentage variants
    PERCENTAGE_NAME <- paste(name,PERCENTAGE_GROUP_SUFFIX,sep=SEP) # this is for things that fall inside a group
    PERCENTAGE_ALL_NAME <- paste(name,PRECENTAGE_ALLTIME_SUFFIX,sep=SEP) # this is all time
    
    #### calculate metrics ####
    if(cum.sum)
    { # cummulative sum
      sumForGroup <- sum(ActivityGroups >= groupIdx, na.rm=TRUE) # count each minute with activity @ or higher than groupIdx
    }
    else
    { # regular sum
      sumForGroup <- sum(ActivityGroups == groupIdx, na.rm=TRUE) # count each minute with activity @ groupIdx
    }
    
    
    result[[name]] <- sumForGroup
    
    # for group
    
    sumForAllGroups <- sum(ActivityGroups > 0, na.rm=TRUE) # all MET Groups
    
    ## for all groups
    if(isAGroupInterval)
    {
      percentageOfGroupInGroups <- sumForGroup / sumForAllGroups * PERCENT_FACTOR # i.e. all groups (excludes data outside of a group)
      if(is.na(sumForAllGroups) || 0 == sumForAllGroups) # i.e. percentage will yield NaN (no steps)
      {
        percentageOfGroupInGroups <- NA
      }
      result[[PERCENTAGE_NAME]] <- percentageOfGroupInGroups # i.e. for group (excludes outside boundary)
    }
    
    
    ## for all times
    sumForAllTime <- length(ActivityGroups[!is.na(ActivityGroups)])
    percentageOfGroupOutOfAllTime <- sumForGroup / sumForAllTime * PERCENT_FACTOR # i.e. this is for all time
    if(is.na(length(ActivityGroups)) || 0 == length(ActivityGroups)) # i.e. percentage will yield NaN (no steps)
    {
      percentageOfGroupOutOfAllTime <- NA
    }
    result[[PERCENTAGE_ALL_NAME]] <- percentageOfGroupOutOfAllTime # i.e. this is for all time
    
  }
  # convert list to dataframe and return
  
  result.df <- as.data.frame(result)
  return(result.df)
}

# calculate metrics for entire step series
calculateSeriesStepMetrics <- function(fitbitDataFrame,
                                       COL_NAME="Steps",
                                       TIMESTAMP_COL_NAME="DateTime",
                                       SAVE_COL_PREFIX="StepData",
                                       removeNAFlag,
                                       SEP=SEP_CHAR)
{
  result <- list()
  isNotEmptySeries <- isSeriesNotEmpty(fitbitDataFrame,COL_NAME)

  if(isNotEmptySeries)
  {
    # order the series by time stamp
    stepSeries <- fitbitDataFrame[order(fitbitDataFrame[[TIMESTAMP_COL_NAME]]),][[COL_NAME]]
    stepSeries.noNA <- stepSeries
    stepSeries.noNA[is.na(stepSeries.noNA)] <- 0
    ### get longest uninterupted streak
    longStreakVal <- longestStreak(stepSeries.noNA)
    
    ### get highest valued uninterrupted streak
    validSteps <- stepSeries.noNA>0
    splitStepSeries <- split(stepSeries.noNA[validSteps],
                             cumsum(stepSeries.noNA==0)[validSteps])
    highStreakVal <- max(unlist(lapply(splitStepSeries,sum)))
  }
  else
  {
    longStreakVal <- NA
    highStreakVal <- NA
  }
  
  result[[paste0(SAVE_COL_PREFIX,SEP,"LongestActiveStreak")]] <- longStreakVal
  result[[paste0(SAVE_COL_PREFIX,SEP,"HighestValuedStreak")]] <- highStreakVal
  
  ### get daily and hour count variance
  timeUnits <- c('day','hour')
  for(timeUnit in timeUnits)
  {
    if(isNotEmptySeries)
    {
      saveName <- paste0(SAVE_COL_PREFIX,SEP,tools::toTitleCase(timeUnit),"lyCountVariance")
      saveName <- sub('Day','Dai',saveName) # if contains 'Day' then replace to get word 'Daily'
      varR <- calculateByTimeUnitVar(df=fitbitDataFrame,
                                     unit=timeUnit,
                                     varCol=COL_NAME,
                                     originCol=TIMESTAMP_COL_NAME)
    }
    else
    {
      varR <- NA
    }
    result[[saveName]] <- varR
  }
  
  result.df <- as.data.frame(result)
  return(result.df)
}

# calculate metrics for entire heart rate series
calculateSeriesHRMetrics <- function(fitbitDataFrame,
                                     HR_COL_NAME="HeartRate",
                                     STEPS_COL_NAME="Steps",
                                     TIMESTAMP_COL_NAME="DateTime",
                                     SAVE_COL_PREFIX="HeartRateData",
                                     removeNAFlag,
                                     SEP=SEP_CHAR)
{
  result <- list()
  HR_SYM_NAME <- rlang::sym(HR_COL_NAME)
  STEPS_SYM_NAME <- rlang::sym(STEPS_COL_NAME)
  isNotEmptySeries <- isSeriesNotEmpty(fitbitDataFrame,STEPS_COL_NAME) && isSeriesNotEmpty(fitbitDataFrame,HR_COL_NAME)
  
  if(isNotEmptySeries)
  {
    ### get peak exercise recovery
    
    lags <- c(1,2,3) # 1, 2 and 3 minutes
    for(lag in lags)
    {
      name <- paste0(SAVE_COL_PREFIX,SEP,'HRDelta',lag,'min')
      hrDelta <- data.frame(Delta=fitbitDataFrame[[HR_COL_NAME]] %>% diff(lag=lag))
      statSummary <- calculateBasicStatisticalMetrics(hrDelta,
                                                      COL_NAME='Delta',
                                                      SAVE_COL_PREFIX=name,
                                                      removeNAFlag)
      statSummary[paste0(name,SEP,c('n',
                                    'Total',
                                    'Mode'))] <- NULL # drop n & total since these are meaningless here
      result <- c(result,statSummary)
    }
    
    ### get slope of heart rate to steps
    form <- paste0(HR_COL_NAME,"~",STEPS_COL_NAME)
    lm.fb <- fitbitDataFrame %>% lm(form, data=.)
    ## coefs
    coefs <- lm.fb$coefficients
    saveName <- paste0(SAVE_COL_PREFIX,SEP,"lm-HRvSteps-",names(coefs))
    result[saveName] <- coefs
    ## rsqrd
    result[[paste0(SAVE_COL_PREFIX,SEP,"lm-HRvSteps-Rsqrd")]] <- summary(lm.fb)$r.squared
    
    
    ### get minute HR variance
    timeUnits <- c('min')
    for(timeUnit in timeUnits)
    {
      saveName <- paste0(SAVE_COL_PREFIX,SEP,tools::toTitleCase(timeUnit),"uteByMinuteVariance")
      saveName <- sub('Day','Dai',saveName) # if contains 'Day' then replace to get word 'Daily'
      result[[saveName]] <- calculateByTimeUnitVar(df=fitbitDataFrame,
                                                   unit=timeUnit,
                                                   varCol=HR_COL_NAME,
                                                   originCol=TIMESTAMP_COL_NAME)
    }
    
  }
  result.df <- as.data.frame(result)
  return(result.df)
}

calculateOverallSummaries <- function(newIntradayMetrics,
                                      removeNAFlag=TRUE,
                                      keyword_Max="Maximum",
                                      keyword_Min="Minimum",
                                      SEP=SEP_CHAR)
{
  library(stringi)
  # general summary, calculate Mean + SD (sampling distribution)
  overallSummary <- newIntradayMetrics %>% summarize_all(funs(#OverallMode=Statistical.Mode,
                                                              #OverallMedian=median,
                                                              OverallMean=mean,
                                                              OverallStdDev=sd
                                                              ),
                                                         na.rm=removeNAFlag)
  
  # add the unique treatment summaries
  keyWord <- keyword_Max
  overallSummaryMax <- newIntradayMetrics %>% summarize_if(.predicate=grepl(pattern=keyWord,names(newIntradayMetrics),ignore.case=TRUE),
                                                           .funs=funs(OverallMax=max),
                                                           na.rm=removeNAFlag)
  
  keyWord <- keyword_Min
  overallSummaryMin <- newIntradayMetrics %>% summarize_if(.predicate=grepl(pattern=keyWord,names(newIntradayMetrics),ignore.case=TRUE),
                                                           .funs=funs(OverallMin=min),
                                                           na.rm=removeNAFlag)
  
  # combine
  
  overallSummary <- cbind(overallSummaryMax, overallSummaryMin, overallSummary)
  
  # replace the names to something more consistent (adds )
  names(overallSummary) <- stri_replace_last_fixed(str=names(overallSummary),
                                                   replacement=SEP,
                                                   pattern="_")
  
  return(overallSummary)
}


# Make Steps & HeartRate NA during times when fitbit was not being worn
# Mode=0 (does nothing - accepts all times as times when it's worn)
# Mode=1 (removes times when no steps: i.e. Steps < 1)
# Mode=2 (removes times when no heart rate: i.e. HeartRate is NA)
# Mode=3 (removes times when no steps OR no heart rate)
# Mode=4 (removes times when no steps AND no heart rate)
convertUnWornValuesToNA <- function(dayDataFrame,
                                    mode=0,
                                    stepColName="Steps",
                                    hrColName="HeartRate")
{
  stepsThreshold <- 1
  isNoSteps <- is.na(dayDataFrame[,stepColName]) | (dayDataFrame[,stepColName] < stepsThreshold)
  isNoHeartRate <- is.na(dayDataFrame[,hrColName])
  
  if(0 == mode)
  {
    # Do nothing
  }
  else
  {
    if(1 == mode)
    {
      makeNA <- isNoSteps
    }
    else if(2 == mode)
    {
      makeNA <- isNoHeartRate
    }
    else if(3 == mode)
    {
      makeNA <- isNoSteps | isNoHeartRate
    }
    else if(4 == mode)
    {
      makeNA <- isNoSteps & isNoHeartRate
    }
    dayDataFrame[makeNA,c(stepColName,hrColName)] <- NA
  }
  
  return(dayDataFrame)
}

isSeriesNotEmpty <- function(series, colName)
{
  seriesAlone <- series[[colName]]
  seriesNotEmpty <- !(is.null(seriesAlone) || (length(seriesAlone) < 1) || all(is.na(seriesAlone)))
  return(seriesNotEmpty)
}

# make sure to pass data frame with proper columns
addByTimeUnitColumn <- function(df, unit, originCol=TIMESTAMP_COL_NAME, newCol=paste0("by_",unit))
{
  originCol <- rlang::sym(originCol)
  result <- df %>% mutate(!!newCol := (!!originCol) %>% lubridate::ymd_hms() %>% lubridate::floor_date(unit=unit))
  return(result)
}

# make sure to pass data frame with proper columns (N.B. this calls addByTimeUnitColumn for you)
calculateByTimeUnitVar <- function(df, unit, varCol=COL_NAME, originCol=TIMESTAMP_COL_NAME)
{
  rCol <- 'sum'
  summary.df <- summarizeToTimeUnit(df, unit, varCol, originCol, resultCol=rCol)
  var.df <- summary.df %>% summarize(v := var((!!rlang::sym(rCol)), na.rm=TRUE))
  return(var.df$v) # strange yes, but if you try var[[1]] it gives cryptic errors (under certain unclear circumstances)
}

summarizeToTimeUnit <- function(df, unit, varCol=COL_NAME, originCol=TIMESTAMP_COL_NAME, newCol=paste0("by_", unit), resultCol='sum')
{
  df <- df %>% addByTimeUnitColumn(unit=unit, originCol=originCol)
  # to unquote properly
  newCol <- rlang::sym(newCol)
  varCol <- rlang::sym(varCol)
  resultCol <- rlang::sym(resultCol)
  summary.df <- df %>% group_by((!!newCol)) %>% summarize(!!resultCol := sum((!!varCol),na.rm=TRUE))
  names(summary.df) <- gsub("\\(|\\)","",names(summary.df),perl=TRUE)
  return(summary.df) # strange yes, but if you try var[[1]] it gives cryptic errors (under certain unclear circumstances)
}