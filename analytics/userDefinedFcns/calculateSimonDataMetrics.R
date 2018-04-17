### Calculate Further Metrics for Simon's Data
#Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

#local functions

calculateSimonDataMetrics <- function(processedData)
{
  #N.B. returns the most frequent element or the first one for multi-modals unique
  Statistical.Mode <- function(x) { #http://stackoverflow.com/a/8189441
    
        ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  ### for each Participant/Sheet
  oldNumVars <- length(processedData)
  for(participant_Index in 1:nrow(processedData)) 
  { #for each participant
    oldParticipantStepData <- processedData$StepData[[participant_Index]]
    if(!(is.null(oldParticipantStepData) || 
         is.na(oldParticipantStepData)))
    {
      for(day_Index in 1:nrow(oldParticipantStepData))
      {
        IntraDay.InstanceOf <- oldParticipantStepData$Intraday[[day_Index]]
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
      }
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
    }
  }
  
  #shunt the stepData column to the back
  newNumVars <- length(processedData)
  #order as: first #[oldNumVars-1] (to account for Step Data Column) then, skip the StepData Column and grab the rest, and lastly the StepData Column
  processedData <- processedData[c(1:(oldNumVars-1),(oldNumVars+1):newNumVars,oldNumVars)]

  return(processedData)
}

