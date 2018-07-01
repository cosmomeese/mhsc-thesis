### Data Analysis Common Definitions & Functions

# SOME BASIC DEFINITIONS

## DEBUG LEVELS

# compress & save important variables in list of constants used

################################################################################
# Common Helper Functions
require(lubridate)
require(tidyverse)

addBMIColumn <- function(fullData) {
  
  # BMI = weight[kg] / (height[m]^2)
  fullData <- fullData %>% mutate(BMI = Weight / ((Height/100)^2))
  return(fullData)
  
}

addMissingClassGroupings <- function(fullData) {
  #### Clean up factor levels ###########################
  
  ## Factor Levels
  # All factors with dirty factor names
  FACTOR_LEVELS <- list()
  FACTOR_LEVELS$ALL <- levels(fullData$NYHAClassMixed)
  # Factors where the given class occurs in any of the dirty factor level names, including as a mixed class
  FACTOR_LEVELS$ANY1 <- grep("(\\b(1|I)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY2 <- grep("(\\b(2|II)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY3 <- grep("(\\b(3|III)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY4 <- grep("(\\b(4|IV)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  # Factors where the given class occurs in any of the dirty factor level names, but not as a mixed class
  FACTOR_LEVELS$PURE1 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY1,FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4)
  FACTOR_LEVELS$PURE2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1)
  FACTOR_LEVELS$PURE3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2)
  FACTOR_LEVELS$PURE4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3)
  # Factor levels used in original Raghad analysis where e.g. I/II or II are considered class II, II/III or III => III, III/IV or IV => IV
  FACTOR_LEVELS$ROUNDDOWN2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1)
  FACTOR_LEVELS$ROUNDDOWN3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$ROUNDDOWN4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2),FACTOR_LEVELS$PURE3)
  # Factor levels where class is mixed I/II, II/III, III/IV
  FACTOR_LEVELS$MIXED12 <- setdiff(FACTOR_LEVELS$ROUNDDOWN2,FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$MIXED23 <- setdiff(FACTOR_LEVELS$ROUNDDOWN3,FACTOR_LEVELS$PURE3)
  FACTOR_LEVELS$MIXED34 <- setdiff(FACTOR_LEVELS$ROUNDDOWN4,FACTOR_LEVELS$PURE4)
  
  
  #### Add MixedNYHAClass (already in Simon Data) #######
  
  # Add NYHAClass, i.e. those using rounddown technique
  # already done (manually) in Simon's data set (in fact we're used it above as the 'cleaned up' factor names
  
  factorLevels2Index <- function(levels)
  {
    temp <- !is.na(factor(fullData$NYHAClassMixed, levels=levels))
    return(temp)
  }
  
  #### Add PureNYHAClass ################################
  
  # Add PureNYHAClass, i.e. just those with true class II and true class III, etc.
  pureNYHAClassVector <- vector(mode="character",nrow(fullData))
  pureFactorIndicies <- factorLevels2Index(c(FACTOR_LEVELS$PURE1,
                                             FACTOR_LEVELS$PURE2,
                                             FACTOR_LEVELS$PURE3,
                                             FACTOR_LEVELS$PURE4))  # get indices for NYHA Classes that are pure 1, 2, 3 or 4 only
  pureNYHAClassVector[pureFactorIndicies] <- as.character(fullData$NYHAClass[pureFactorIndicies])  # select/copy the 'cleaned up' factor names
  fullData$PureNYHAClass <- factor(pureNYHAClassVector, levels=levels(fullData$NYHAClass))  # convert to factor
  rm(pureFactorIndicies,pureNYHAClassVector)
  
  #### Add ExplicitNYHAClass ############################
  
  # Add ExplicitNYHAClass, i.e. those with class I, I/II, II, II/III, III, III/IV, IV
  explicitNYHAClassVector <- vector(mode="character",nrow(fullData))
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE1))] <- "I"  # populate pure I
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED12))] <- "I/II"  # populate mixed I/II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE2))] <- "II"  # populate pure II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED23))] <- "II/III"  # populate mixed II/III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE3))] <- "III"  # populate pure III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED34))] <- "III/IV"  # populate mixed III/IV
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE4))] <- "IV"  # populate pure IV
  fullData$ExplicitNYHAClass <- factor(explicitNYHAClassVector,levels=c("I","I/II","II","II/III","III","III/IV","IV"))
  rm(explicitNYHAClassVector)
  
  # Remove participants where we have missing important (see step) data
  warning("Forcing HF018 to NA since HF018 has no step data - come up with a better way to do this if using different data set")
  fullData$PureNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # drop HF018 since they don't have step data
  fullData$ExplicitNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # as above
  
  return(fullData)
}

createViewableFullData <- function(fullData)
{
  viewableFullData <- fullData; 
  viewableFullData$StepData <- NULL  # drop StepData since this is what causes the problem
  return(viewableFullData)
}

#### N.B. test split & merge instructions ####
# using: http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
####

# takes dataframe (df) with StudyIdentifier column + DateTime column and splits into time and integer day
# dateForTimeCol should be in relevant timezone or with CONSTANTS$NORMALIZED_STUDY_DAY_START where possible (tz is ignored)
# N.B. this function can be ___very___ slow
splitDateTimeToDayAndTime <- function(df, 
                                      dateForTimeCol=as.POSIXct("9000-01-01 00:00:00", tz="UTC"),
                                      dropOldColumns=TRUE)
{
  # remove entries with no valid datetime
  df <- clearNADateTimeColumns(df)
  
  # create new Day, Time & BaseDay columns
  df <- df %>% group_by(StudyIdentifier) %>%
          mutate(Day = lubridate::yday(DateTime) - lubridate::yday(first(DateTime)),
                 Time = DateTime,
                 BaseDay = lubridate::ymd(first(DateTime))
          )
  
  unixEpochStartDate <- "1970-01-01"
  secondsInDay <- 60*60*24 #60 sec * 60 min * 24 hr
  df$Time <- as.POSIXct(as.numeric(dateForTimeCol) + (as.numeric(df$Time) %% secondsInDay),
                        origin = unixEpochStartDate,
                        tz = lubridate::tz(df$Time))

  # BIGGEST PERFORMANCE HIT V
  #year(df$Time) <- year(dateForTimeCol) #reset to default year
  #yday(df$Time) <- yday(dateForTimeCol) #reset to first day
  #tz(df$Time) <- tz(dateForTimeCol) #reset to corresponding time zone
  # BIGGEST PERFORMANCE HIT ^
  
  if(dropOldColumns)
  {
    df$DateTime <- NULL  # drop DateTime    
  }

  df <- df %>% ungroup()
  
  return(df)
}

# takes dataframe (df) with StudyIdentifier column + Day + Time column (& optional BaseDay) and merges into DateTime
# dateForTimeCol should be in relevant timezone or with CONSTANTS$NORMALIZED_STUDY_DAY_START where possible (tz is ignored)
# baseDayList can be NULL or list() containing 
# N.B. this function can be ___very___ slow
mergeDayAndTimeToDateTime <- function(df,
                                      baseDayList=NULL,
                                      defaultBaseDay=as.POSIXct("9000-01-01 00:00:00", tz="UTC"),
                                      dropOldColumns=TRUE)
{
  # remove entries with no valid datetime
  df <- clearNADateTimeColumns(df)
  
  # validate base day
  hasBaseDayCol <- "BaseDay" %in% colnames(df)
  isValidBaseDayArgSupplied <- !is.null(baseDayList) && is.list(baseDayList)
  if(isValidBaseDayArgSupplied &&
     hasBaseDayCol)
  {
      df$BaseDay <- NULL # this is here so addBaseDayToPatient can operate
      hasBaseDayCol <- FALSE # since we need to replace
  }
  
  # add base day column
  if(!hasBaseDayCol)
  {
    # BIGGEST PERFORMANCE HIT V (if no baseDay column)
    df <- addBaseDayToPatient(df=df,
                              baseDayList=baseDayList,
                              defaultBaseDay=defaultBaseDay)
    # BIGGEST PERFORMANCE HIT ^
  }
  
  # we need this for later
  unixEpochStartDate <- "1970-01-01" #out common origin
  
  # since timezones are messed up to use: back up the timezone first - will restore later
  df$TimeTZ <- df$Time
  lubridate::tz(df$Time) <- "UTC"
  
  # since we only want the seconds offset (i.e. not date) from time let's get that
  secondsInDay <- 60*60*24 #60 sec * 60 min * 24 hr
  # create new TimeSec column
  df$TimeSec <- as.numeric(df$Time) %% secondsInDay
  
  browser()
  
  df <- df %>% mutate(BaseDaywithTZ = lubridate::force_tz(BaseDay,
                                                          tzone = attributes(TimeTZ)$tzone,
                                                          roll=TRUE))
  
  # create new DateTime column, combining BaseDay, Day (as added days) & seconds (from from our common origin)
  df$DateTime2 <- as.POSIXct(df$BaseDaywithTZ + lubridate::days(df$Day) + lubridate::seconds(df$TimeSec),
                             origin = unixEpochStartDate)
  
  # restore the timezone
  # N.B. WARN!!!!!! roll must be set to TRUE to avoiding getting NAs during DST-breaks

  
  # delete the now redundant TimeTZ & TimeSec column
  df$TimeTZ <- NULL
  df$TimeSec <- NULL
  df$BaseDaywithTZ <- NULL
  
  if(dropOldColumns)
  {
    df$Time <- NULL  # drop Time column
    df$Day <- NULL  # drop Day column
    df$BaseDay <- NULL  # drop BaseDay column
  }
  
  df <- df %>% ungroup()
  
  return(df)
}

# N.B. colName can be anything so long as it's not already an existing column in dataframes
# returns TRUE if test passes
# returns the combined duplicate test frame if test fails
# identical() for pure all equals test
# all.equal(x,y) gives some idea of what's gone wrong
# attributes(mergeDF) can be used for further debugging (to compare attributes)
#
# REMEMBER TO TEST BOTH WAYS
# i.e.: 
# the default way
# res <- testSplitAndMerge(fitbitData.df)
#      +
# the inverse
# res2 <- testSplitAndMerge(splitDateTimeToDayAndTime(fitbitData.df),testDFStartsInDateTimeMode = FALSE)
testSplitAndMerge <- function(testDF,
                              testDFStartsInDateTimeMode=TRUE,
                              colName="Coder")
{
  # http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
  # visit above for more help
  
  #browser() # so tester can follow along
  
  if(testDFStartsInDateTimeMode)
  {
    splitDF <- splitDateTimeToDayAndTime(testDF)
  }
  else
  {
    splitDF <- testDF # for the interim this one is already split
  }
  mergeDF <- mergeDayAndTimeToDateTime(splitDF)
  if(!testDFStartsInDateTimeMode) # now generate the actual splitDF
  {
    splitDF <- splitDateTimeToDayAndTime(mergeDF)
  }
  
  testDF.noNAs <- clearNADateTimeColumns(testDF)
  
  
  if(testDFStartsInDateTimeMode) # now generate the actual splitDF
  {
    secondCompareDF <- mergeDF
  }
  else
  {
    secondCompareDF <- splitDF
  }
  
  
  #cat("identical?", identical(testDF.noNAs,secondCompareDF), "\n")
  
  
  # Note if this complains that:
  # Error in as.POSIXct.numeric(current) : 'origin' must be supplied
  # may happen because it compares rows in order, 
  # so it might be comparing row 4 in current w/ 
  # row 4 in target which may not be the same format
  # but it will try to coerce them into POSIXct (and fail)
  # due to lack of origin
  #cat("all.equal?", all.equal(testDF.noNAs,secondCompareDF), "\n", sep="\n")
  browser() # so tester can follow along
  result <- list()
  origColName <- 'originalNoNAS'
  splitMergeColName <- 'splitMerged'
  result[[origColName]] <- testDF.noNAs
  result[[splitMergeColName]] <- secondCompareDF

  return(result)
}

# takes dataframe (df) with StudyIdentifier that's missing BaseDay to reconstruct DateTime
# base day list should be a named list (keys=desired StudyIDs) as.POSIXct compatible string
addBaseDayToPatient <- function(df,
                                baseDayList=NULL,
                                defaultBaseDay=as.POSIXct("9000-01-01 00:00:00", tz=""))
{
  validDefaultBaseDay <- !is.null(defaultBaseDay) && lubridate::is.POSIXct(defaultBaseDay)
  if(!validDefaultBaseDay)
  {
    errMsg <- 'Please specify valid base day: e.g. as.POSIXct("9000-01-01 00:00:00", tz="")'
    stop(errMsg)
  }
  if(is.null(baseDayList) || !is.list(baseDayList))
  {
    baseDayList <- list()
  }

  for(studyID in levels(dframe$StudyIdentifier))
  {
    # get valid base day
    baseDay <- try({ as.POSIXct(baseDayList[[studyID]]) },
                   silent = TRUE)
    validBaseDay <- !is.null(baseDay) && lubridate::is.POSIXct(baseDay)
    if(!validBaseDay)
    {
      warnMsg <- paste0("BaseDay given for ",studyID," is invalid, using default value ", defaultBaseDay, " instead")
      warning(warnMsg)
      baseDay <- defaultBaseDay
    }
    
    # create as data frame row
    dframe <- data.frame(StudyIdentifier=studyID,
                         BaseDay=as.POSIXct(baseDay))
    baseDayList[[studyID]] <- dframe
  }
  
  baseDayDFrame <- do.call(rbind, baseDayList)

  # biggest performance hit V
  df <- merge(x=df,
              y=baseDayDFrame,
              by="StudyIdentifier",
              all.x=TRUE
  )
  # biggest performance hit ^
  
  return(df)
}

# input: dataframe (df) with either:
#   DateTime or
#   Day & Time
# output: dataframe with cleared entries where the DateTime/Day&Time is NA
clearNADateTimeColumns <- function(df)
{
  if("DateTime" %in% colnames(df))
  {
    resultDF <- df[!is.na(df$DateTime),]
  }
  else if("Day" %in% colnames(df) 
          && "Time" %in% colnames(df))
  {
    resultDF <- df[!(is.na(df$Day) || is.na(df$Time)),]
  }
  return(resultDF)
}

# input: dataframe (df) with both:
#   StudyIdentifier
#   NYHAClass
# output: dataframe with first NYHAClass element for each patient expanded to all entries for that patient
expandNYHAClassToEntirePatient <- function(df)
{
  df <- df %>% group_by(StudyIdentifier) %>%
              mutate(NYHAClass = first(NYHAClass))
  df <- df %>% ungroup()
  return(df)
}
