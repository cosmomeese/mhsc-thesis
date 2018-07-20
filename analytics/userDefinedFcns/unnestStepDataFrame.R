### Function to unnest Step Data 

## Import Required Libraries
# Install if it's not installed on this computer
pkg <- c("tidyr","dplyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyr)
library(dplyr)

#local functions

# old version, kept for compatability & updated to support
# use new functionality as desired, set v to anything except
# deprecated
unnestStepDataFrame <- function(originalOutputDataFrame,
                                useUpdatedVersion=FALSE)
{
  if(useUpdatedVersion)
  {
    unnestedDataFrame <- unnestDataFrame(originalOutputDataFrame,
                                         intradayColsToKeep=c("Steps",
                                                              "HeartRate",
                                                              "Time",
                                                              "Day"))
  }
  else
  {
    #initialize
    unnestedStepData <- data.frame(StudyIdentifier = NULL,
                                   Steps = NULL,
                                   Time = NULL,
                                   Data = NULL)
    
    #for every sheet apply
    for(sheetIndex in 1:nrow(originalOutputDataFrame))
    {
      #unnest
      stepDataForSheet <- originalOutputDataFrame$StepData[[sheetIndex]]
      if( is.data.frame(stepDataForSheet) && ( "Intraday" %in% colnames(stepDataForSheet) ) )
      {
        stepDataAlone <- tidyr::unnest(data = stepDataForSheet, Intraday)
        
        #only keep the ones we want
        stepDataAlone.Trimmed <- data.frame(Steps = stepDataAlone$Steps,
                                            Time = stepDataAlone$Time,
                                            Day = stepDataAlone$Day,
                                            stringsAsFactors = FALSE)
        #tag with the appropriate study identifier
        stepDataAlone.Trimmed$StudyIdentifier <- originalOutputDataFrame$StudyIdentifier[[sheetIndex]]
        
        #addd to the rest of the stepData
        unnestedStepData <- rbind(unnestedStepData,stepDataAlone.Trimmed)
      }
    }
    
    originalOutputDataFrame$StepData <- NULL
    unnestedDataFrame <- merge(x = originalOutputDataFrame,
                               y = unnestedStepData, 
                               by = "StudyIdentifier", 
                               all = TRUE)
  }
  
  return(unnestedDataFrame)
}

nestStepDataFrame <- function(unnestedDataFrame, uniqueKeyColName="UUID")
{
  result <- nestDataFrame(unnestedDataFrame,
                          studyIDCol=c("StudyIdentifier"),
                          nestedColName="StepData")
  return(result)
}

# N.B. is assumed that one ID per row
# N.B. set intradayColsToKeep to NULL to keep all (these columns must be the innermost ones)
unnestDataFrame <- function(nestedDataFrame,
                            nestedColName="StepData",
                            intradayColsToKeep=c("RandomIrrelevantColumn","Steps","HeartRate","Time","Day"),
                            uniqueKeyColName="UUID")
{
  
  # add UUID
  rownames(nestedDataFrame) <- NULL
  nestedDataFrame[[uniqueKeyColName]] <- factor(as.numeric(rownames(nestedDataFrame))) # since these are guaranteed unique
  
  # split into part to nest and to unnest
  stepDataList <- nestedDataFrame[[nestedColName]]
  nestedDataFrame[,names(nestedDataFrame) %in% c(nestedColName)] <- NULL
  
  firstLoop <- TRUE
  #for every sheet apply
  cat("unpacking... this may take some time...\n")
  for(listIdx in 1:length(stepDataList))
  {
    #unnest
    stepDataForSheet <- stepDataList[[listIdx]]
    if( is.data.frame(stepDataForSheet) && ( "Intraday" %in% colnames(stepDataForSheet) ) )
    {
      stepDataAlone <- tidyr::unnest(data = stepDataForSheet, Intraday)
      
      #only keep the ones we want
      stepDataAlone.Trimmed <- stepDataAlone
      if(!is.null(intradayColsToKeep))
      {
        stepDataAlone.Trimmed <- stepDataAlone[,names(stepDataAlone) %in% c(intradayColsToKeep)]
      }
      
      #tag with the appropriate study identifier
      stepDataAlone.Trimmed[[uniqueKeyColName]] <- nestedDataFrame[[listIdx,uniqueKeyColName]]
      
      if(firstLoop)
      {
        stepDataFrame <- stepDataAlone.Trimmed
        firstLoop <- FALSE
      }
      else
      {
        stepDataFrame <- rbind(stepDataFrame,stepDataAlone.Trimmed)
      }
    }
  }
  cat("done unpacking! :)\n")
  
  cat("merging... this may take some time...\n")
  unnestedDataFrame <- merge(x = nestedDataFrame,
                             y = stepDataFrame, 
                             by = uniqueKeyColName, 
                             all = TRUE)
  cat("done merging! :)\n")
  unnestedDataFrame[[uniqueKeyColName]] <- NULL
  
  return(unnestedDataFrame)
}

## N.B. don't rely on the StepData list's name ID's for compatability
## since old versions nested versions aren't labelled that way.
## this was added more for viewing usability
nestDataFrame <- function(unnestedDataFrame,
                          studyIDCol=c("UniversalStudyID","StudyIdentifier","Dataset"),
                          uniqueKeyColName="UUID",
                          nestedColName="FitbitData")
{
  studyIDColOnly <- colnames(unnestedDataFrame) %in% c(studyIDCol)
  columnsToNest <- colnames(unnestedDataFrame) %in% c("Steps",
                                                      "HeartRate",
                                                      "DateTime",
                                                      "Time",
                                                      "Day",
                                                      "BaseDay")
  
  unnDataFrame.notToNest <- unnestedDataFrame[,studyIDColOnly | !columnsToNest]
  # then remove the duplicated entries (1 for each step/heartrate entry)
  cat("removing duplicates... this may take some time...\n")
  unnDataFrame.notToNest <- unnDataFrame.notToNest[!duplicated(unnDataFrame.notToNest),]
  rownames(unnDataFrame.notToNest) <- NULL
  unnDataFrame.notToNest[[uniqueKeyColName]] <- factor(as.numeric(rownames(unnDataFrame.notToNest))) # since these are guaranteed unique
  cat("done removing duplicates! :)\n")
  
  # extra the parts to actually nest
  unnDataFrame.toNest <- unnestedDataFrame[,studyIDColOnly | columnsToNest]
  cat("adding uniqueKeyColName... this may take some time...\n")
  unnDataFrame.toNest <- merge(x=unnDataFrame.toNest,
                               y=unnDataFrame.notToNest[,c(uniqueKeyColName,studyIDCol)],
                               by=studyIDCol,
                               all.x=TRUE)
  cat("done adding uniqueKeyColName.! :)\n")
  unnDataFrame.toNest <- unnDataFrame.toNest[ , !names(unnDataFrame.toNest) %in% studyIDCol] 
  
  # add the column for the collapsedstep data entries
  nestedDataList <- vector("list",length(levels(unnDataFrame.toNest[[uniqueKeyColName]])))
  index <- 1
  for(uuid in levels(unnDataFrame.toNest[[uniqueKeyColName]]))
  {
    dframeSubset <- unnDataFrame.toNest[unnDataFrame.toNest[uniqueKeyColName] == uuid,]
    dframeSubset[[uniqueKeyColName]] <- NULL
    nestedSubDF <- tidyr::nest(data = dplyr::group_by(dframeSubset,Day),
                               .key = "Intraday")
    nestedDataList[[index]] <- nestedSubDF
    index <- index+1
  }
  
  unnDataFrame.notToNest[[nestedColName]] <- nestedDataList
  unnDataFrame.notToNest[[uniqueKeyColName]] <- NULL
  
  return(unnDataFrame.notToNest)
}

