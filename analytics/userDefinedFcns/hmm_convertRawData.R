### Convert Raw Data for Hidden Markov Model Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("dplyr","lubridate")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(lubridate)
library(dplyr)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("unnestStepDataFrame",
             "hmm_common")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

# -------------------------------------------------------

# local functions
hmm_convertRawData <- function(rawData, fitbitDownload=FALSE)
{
  DATA_SET_RAW <- rawData[!is.na(rawData$NYHAClass),] #i.e. m_cData excluding patients w/o NYHAClass
  cat("\nM: Loaded Dataset...")
  
  #return variable
  dataSet <- data.frame()
  
  if(!fitbitDownload)
  {
    # Sort by Study Identifier, Day, then Time (this will be important for extract the series lengths)
    cat("\nM: Unnesting Dataset...")
    data.unnested <- arrange(unnestStepDataFrame(DATA_SET_RAW),  # unnest set
                             StudyIdentifier,  # sort first by ID
                             Day,  # then by day
                             Time)  # then by time in day
    cat("\n   - Finished unnesting!")
    
    ## Extract only the variables we want
    cat("\nM: Discarding variables not required...")
    dataSet <- data.unnested %>% dplyr::select(StudyIdentifier,Steps,HeartRate=HR,Time,Day,NYHAClass)
  
    ## Merge Day & Time Columns to single DateTime
    cat("\nM: Merging Day & Time columns...")
    dataSet <- mergeDayTimeToDateTime(dataSet)
    attr(dataSet$DateTime,"tz") <- "UTC"  # force time zone to UTC (doesn't adjust time, only sets time zone)
  } else
  {
    dataSet <- arrange(DATA_SET_RAW,
                       StudyIdentifier,  # sort first by ID
                       DateTime)  # then by datetime)
  }
  
  ## Normalize the predictors
  cat("\nM: Normalizing variables...")
  dataSet$Steps <- (dataSet$Steps - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale steps
  dataSet$HeartRate <- (dataSet$HeartRate - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale heart rate
  
  cat("\nM: Finished importing DataSet...")
  
	return(dataSet)
}

## HELPER FUNCTIONS

mergeDayTimeToDateTime <- function(df)
{
  # merge time & day as datetime
  datetime <- df$Time
  df['Time'] <- NULL  # clear old time column

  yday(datetime) <- df$Day  # set day of year to day
  df['Day'] <- NULL # clear old day column

  year(datetime) <- year(CONSTANTS$NORMALIZED_STUDY_DAY_START)  # set year to year 9000 (arbitrary but in the future so don't need to deal the vagaries of the past e.g. future changes to tz)
  
  df['DateTime'] <- datetime  # add new combined datetime as new column
  rm(datetime)
  
  return(df)
}

createDepMixS4DataSet <- function(dataSubSet)  # NOTE WE POTENTIALLY REUSE THIS IN TESTING (to help assemble test set)
{
  
  printDebug = FALSE
  if(debugGivenThreshold(DEBUGLVL.INFO))
  {
    printDebug = TRUE
  }
  if(printDebug)
  {
    cat("\n   - Converting training set to depmixS4 compatible data type...")
  }
  
  #return variable
  dataSubSet.class <- data.frame()
  
  # Sort by Study Identifier, Day, then Time (this will be important for extract the series lengths)
  dataSubSet.unnest <- arrange(unnestStepDataFrame(dataSubSet),  # unnest set
                               StudyIdentifier,  # sort first by ID
                               Day,  # then by day
                               Time)  # then by time in day
  
  ## Extract only the variables we want
  dataSubSet.class <- dataSubSet.unnest %>% dplyr::select(StudyIdentifier,Steps,HeartRate=HR,Time,Day)
  
  ## Normalize the predictors
  dataSubSet.class$Steps <- (dataSubSet.class$Steps - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale steps
  dataSubSet.class$HeartRate <- (dataSubSet.class$HeartRate - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale heart rate
  
  ## Add the series lengths as an attribute to the return variable
  attr(dataSubSet.class,'ntimes') <- dataSubSet.N
  
  #class(dataSubSet.class) <- "depmixS4.dataframe"  # convert 'train' to 'depmixS4.dataframe' class
  return(dataSubSet.class)
}

createDepMixS4DataSet <- function(dataSubSet)  # NOTE WE POTENTIALLY REUSE THIS IN TESTING (to help assemble test set)
{
  
  printDebug = FALSE
  if(debugGivenThreshold(DEBUGLVL.INFO))
  {
    printDebug = TRUE
  }
  if(printDebug)
  {
    cat("\n   - Converting training set to depmixS4 compatible data type...")
  }
  
  #return variable
  dataSubSet.class <- data.frame()
  
  # Sort by Study Identifier, Day, then Time (this will be important for extract the series lengths)
  dataSubSet.unnest <- arrange(unnestStepDataFrame(dataSubSet),  # unnest set
                               StudyIdentifier,  # sort first by ID
                               Day,  # then by day
                               Time)  # then by time in day
  
  ## Extract only the variables we want
  dataSubSet.class <- dataSubSet.unnest %>% dplyr::select(StudyIdentifier,Steps,HeartRate=HR,Time,Day)
  
  ## Normalize the predictors
  dataSubSet.class$Steps <- (dataSubSet.class$Steps - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale steps
  dataSubSet.class$HeartRate <- (dataSubSet.class$HeartRate - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS  # rescale heart rate
  
  ## Extract the series lengths
  dataSubSet.table <- table(dataSubSet.class$StudyIdentifier)  # get table of each patient sequence in set
  dataSubSet.N <- as.numeric(dataSubSet.table)  # get length of each patient sequence in set
  dataSubSet.N <- dataSubSet.N[dataSubSet.N != 0]  # drop zero length vectors of each patient sequence in set (i.e. patients not in set)
  #dataSubSet.seqM <- max(dataSubSet.N)  # max sequence length (for fitting sojourn distributions?)
  
  ## Add the series lengths as an attribute to the return variable
  attr(dataSubSet.class,'ntimes') <- dataSubSet.N
  
  #class(dataSubSet.class) <- "depmixS4.dataframe"  # convert 'train' to 'depmixS4.dataframe' class
  return(dataSubSet.class)
}


createHSMMDataSet <- function(dataSubSet)  # NOTE WE POTENTIALLY REUSE THIS IN TESTING (to help assemble test set)
{
  dataSubSet.unnest <- arrange(unnestStepDataFrame(dataSubSet),  # unnest set
                               StudyIdentifier,  # sort first by ID
                               Day,  # then by day
                               Time)  # then by time in day
  
  dataSubSet.table <- table(dataSubSet.unnest$StudyIdentifier)  # get table of each patient sequence in set
  dataSubSet.N <- as.numeric(dataSubSet.table)  # get length of each patient sequence in set
  dataSubSet.N <- dataSubSet.N[dataSubSet.N != 0]  # drop zero length vectors of each patient sequence in set (i.e. patients not in set)
  dataSubSet.seqM <- max(dataSubSet.N)  # max sequence length (important for fitting sojourn gamma distribution
  
  printDebug = FALSE
  if(debugGivenThreshold(DEBUGLVL.INFO))
  {
    printDebug = TRUE
  }
  if(printDebug)
  {
    cat("\n   - Converting training set to hsmm.data data type...")
  }
  
  dataSubSet.class <- list(x = (dataSubSet.unnest$Steps - CONSTANTS$UNSCALEDMIN.STEPS)*CONSTANTS$RESCALEFACTOR.STEPS + CONSTANTS$RESCALEDMIN.STEPS,  # rescale
                           N = dataSubSet.N,
                           table = dataSubSet.table)
  class(dataSubSet.class) <- "hsmm.data"  # convert 'train' to 'hsmm.data' class
  return(dataSubSet.class)
}
