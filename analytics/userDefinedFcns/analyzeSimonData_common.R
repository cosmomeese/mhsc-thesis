### Data Analysis Common Definitions & Functions

# SOME BASIC DEFINITIONS

## DEBUG LEVELS

# compress & save important variables in list of constants used

################################################################################
# Common Helper Functions
require(tidyverse)
require(lubridate)
require(stringr)

addBMIColumn <- function(fullData) {
  
  # BMI = weight[kg] / (height[m]^2)
  fullData <- fullData %>% mutate(BMI = Weight / ((Height/100)^2))
  return(fullData)
  
}

# adds based on NYHAClassMixed
addMissingClassGroupings <- function(fullData,
                                     removeHF018=TRUE,
                                     trimExtraPureFacLevels=TRUE,
                                     trimExtraRoundedFacLevels=TRUE) {
  #### Clean up factor levels ###########################
  
  C1 <- "I"
  C1.2 <- "I/II"
  C2 <- "II"
  C2.3 <- "II/III"
  C3 <- "III"
  C3.4 <- "III/IV"
  C4 <- "IV"
  
  mixedClassColName <- "NYHAClassMixed"
  
  ## Factor Levels
  # All factors with dirty factor names
  FACTOR_LEVELS <- list()
  FACTOR_LEVELS$ALL <- levels(fullData[[mixedClassColName]])
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
  
  ## nHelper fcn
  factorLevels2Index <- function(levels)
  {
    temp <- !is.na(factor(fullData[[mixedClassColName]], levels=levels))
    return(temp)
  }
  
  #### Add MixedNYHAClass (already in Simon Data) #######
  
  # Already using
  
  #### Add ExplicitNYHAClass ############################
  
  # Add ExplicitNYHAClass, i.e. those with class I, I/II, II, II/III, III, III/IV, IV
  explicitNYHAClassVector <- vector(mode="character",nrow(fullData))
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE1))] <- C1  # populate pure I
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED12))] <- C1.2  # populate mixed I/II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE2))] <- C2  # populate pure II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED23))] <- C2.3  # populate mixed II/III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE3))] <- C3  # populate pure III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED34))] <- C3.4  # populate mixed III/IV
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE4))] <- C4  # populate pure IV
  fullData$ExplicitNYHAClass <- factor(explicitNYHAClassVector,levels=c(C1,C1.2,C2,C2.3,C3,C3.4,C4))
  rm(explicitNYHAClassVector)
  
  #### Add PureNYHAClass ################################
  
  # Add PureNYHAClass, i.e. just those with true class II and true class III, etc.
  pureNYHAClassVector <- vector(mode="character",nrow(fullData))
  pureFactorIndicies <- factorLevels2Index(c(FACTOR_LEVELS$PURE1,
                                             FACTOR_LEVELS$PURE2,
                                             FACTOR_LEVELS$PURE3,
                                             FACTOR_LEVELS$PURE4))  # get indices for NYHA Classes that are pure 1, 2, 3 or 4 only
  pureNYHAClassVector[pureFactorIndicies] <- as.character(fullData$ExplicitNYHAClass[pureFactorIndicies])  # select/copy the 'cleaned up' factor names
  pureFactorLevels <- c(C1,C2,C3,C4)
  pureNYHAClassVector <- factor(pureNYHAClassVector,
                                levels=pureFactorLevels)  # convert to factor (will use only existing levels in the vector)
  if(trimExtraPureFacLevels)
  {
    # convert to character and back to factor will remove
    # extra unused factor levels
    pureNYHAClassVector <- as.factor(as.character(pureNYHAClassVector))
  }
  fullData$PureNYHAClass <- pureNYHAClassVector
  rm(pureFactorIndicies,pureNYHAClassVector)
  
  #### Add RoundDownNYHAClass ################################
  
  # Add (generated) NYHAClass i.e. those using rounddown technique (see factor lvl definitions above)
  roundDownNYHAClassVector <- vector(mode="character",nrow(fullData))
  roundDownNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE1))] <- C1  # populate roundown (pure) group 1
  roundDownNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$ROUNDDOWN2))] <- C2  # populate roundown group 2
  roundDownNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$ROUNDDOWN3))] <- C3  # populate roundown group 3
  roundDownNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$ROUNDDOWN4))] <- C4  # populate roundown group 4
  roundDownNYHAClassVector <- factor(roundDownNYHAClassVector,
                                        levels=pureFactorLevels)
  if(trimExtraPureFacLevels)
  {
    # convert to character and back to factor will remove
    # extra unused factor levels
    roundDownNYHAClassVector <- as.factor(as.character(roundDownNYHAClassVector))
    
  }
  fullData$RoundDownNYHAClass <- roundDownNYHAClassVector
  rm(roundDownNYHAClassVector)
  
  #### Remove HF018 ####
  
  if(removeHF018)
  {
    # Remove participants where we have missing important (see step) data
    warning("Forcing HF018 to NA since HF018 has no step data - come up with a better way to do this if using different data set")
    fullData$PureNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # drop HF018 since they don't have step data
    fullData$ExplicitNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # as above
    fullData$RoundDownNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # as above
  }
  
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
    dplyr::arrange(DateTime, .by_group=TRUE) %>%
    mutate(Day = lubridate::yday(DateTime) - lubridate::yday(first(DateTime)),
           Time = DateTime,
           BaseDay = lubridate::floor_date(first(DateTime),
                                           unit="day")
    )
  
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
  
  #browser() # so tester can follow along
  
  # add base day column
  if(!hasBaseDayCol)
  {
    # BIGGEST PERFORMANCE HIT V (if no baseDay column)
    df <- addBaseDayToPatient(df=df,
                              baseDayList=baseDayList,
                              defaultBaseDay=defaultBaseDay)
    # BIGGEST PERFORMANCE HIT ^
  }
  
  # since timezones are messed up to use: back up the timezone first - will restore later
  lubridate::tz(df$Time) <- "UTC"
  
  # since we only want the seconds offset (i.e. not date) from time let's get that
  secondsInDay <- 60*60*24 #60 sec * 60 min * 24 hr
  # create new TimeSec column
  df$TimeSec <- as.numeric(df$Time) %% secondsInDay
  
  #browser() # so tester can follow along
  
  # create new DateTime column, combining BaseDay, Day (as added days) & seconds (from from our common origin)
  df$DateTime <- df$BaseDay + lubridate::days(df$Day) + lubridate::seconds(df$TimeSec)
  
  # N.B. WARN!!!!!! roll must be set to TRUE to avoiding getting NAs during DST-breaks
  
  # delete the now redundant TimeSec column
  df$TimeSec <- NULL
  
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
                         BaseDay=as.POSIXct(baseDay,
                                            tz=lubridate::tz(baseDay)))
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

# adds BaseDay column from DateTime Column but based on first element in df Patient group (by StudyIdentifier)
# N.B. also requires Time & Day columns
addBaseDayFromTime <- function(df)
{
  # remove entries with no valid datetime
  df <- clearNADateTimeColumns(df)
  
  # create new Day, Time & BaseDay columns
  df <- df %>% group_by(StudyIdentifier) %>% 
    dplyr::arrange(Day, Time, .by_group=TRUE) %>%
    mutate(BaseDay = lubridate::floor_date(first(Time),
                                           unit="day")
    )
  
  df <- df %>% ungroup()
  
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

# works for both handedness and wristband
# N.B. order matters for eligibleFactors (first will become 0)
# defaults to handedness
cleanFactorColumnForAnalysis <- function(fullData,
                                         colName="Handedness",
                                         eligibleFactors=c("Left","Right")) {
  
  # remove all non-valid factors
  fullData[[colName]] <- factor(fullData[[colName]],
                              levels=eligibleFactors,
                              ordered=TRUE)
  # conver to numeric between with value 0|1
  fullData[[colName]] <- as.numeric(fullData[[colName]]) - 1
  
  return(fullData)
  
}

# Left = 0, Right = 1
cleanHandednessColumnForAnalysis <- function(fullData) {
  cName <- "Handedness"
  fctrs <- c("Left","Right")
  result <- cleanFactorColumnForAnalysis(fullData,
                                         colName=cName,
                                         eligibleFactors=fctrs)
  return(result)
}

# Left = 0, Right = 1
cleanWristbandColumnForAnalysis <- function(fullData) {
  cName <- "WristbandPreference"
  fctrs <- c("Left","Right")
  result <- cleanFactorColumnForAnalysis(fullData,
                                         colName=cName,
                                         eligibleFactors=fctrs)
  return(result)
}

# Male = 0, Female = 1
cleanSexColumnForAnalysis <- function(fullData) {
  cName <- "Sex"
  fctrs <- c("M","F")
  result <- cleanFactorColumnForAnalysis(fullData,
                                         colName=cName,
                                         eligibleFactors=fctrs)
  return(result)
}

# use to verify addMissingClassGroups (or to get simply view the class groupings once calculated)
# requires columns:
# StudyIdentifier
# NYHAClassMixed
# PureNYHAClass
# ExplicitNYHAClass
# RoundDownNYHAClass
getClassesByStudyID <- function(fullData) {
  classesByStudyID <- fullData %>% 
                        group_by(StudyIdentifier) %>% 
                          dplyr::summarize(NCMixed = first(NYHAClassMixed),
                                    NCPure = first(PureNYHAClass),
                                    NCExplicit = first(ExplicitNYHAClass),
                                    NCRound = first(RoundDownNYHAClass))
  return(classesByStudyID)
}
