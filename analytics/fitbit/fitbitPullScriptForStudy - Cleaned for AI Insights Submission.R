## Fitbit Script## 
## V1.0 2018-01-23 Jonathan-F. Baril

## Install & Load Required Library Packages
# Install if it's not installed on this computer

# Regular Packages
pkg <- c("ggplot2","dplyr","tidyr","readr","tools")
#pkg <- c(pkg,"httr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

# Github Packages
pkg <- c(#"teramonagi/fitbitr",
         "cosmomeese/httr",
         "cosmomeese/fitbitr")
#pkg <- c("avsecz/fitbitr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  devtools::install_github(new.pkg)
}
rm(pkg,new.pkg)

# Load Libraries

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(base64enc)
library(tools)
library(fitbitr)
library(httr)

cat("N.B.: There are presently problems with installing the non-official httr copy from github,\n you may need to restart rstudio a few times between installs if functionality is missing")
#N.B. cannot not install official httr since it is a dependency for devtools (used for install_github).
# Solution is to
# 0. restart RStudio (and make sure no packages are loaded, etc.)
# 1. uninstall current httr (then comment out all httr from above): remove.packages('httr')
# 2. restart RStudio
# 3. install official/httr using (then comment out official/httr from above): install.packages('httr')
# 5. install cosmomeese/httr (uncomment out cosmomeese/httr from above): devtools::install_github('cosmomeese/httr')
# 6. restart RStudio

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","hmm_common.R",
             sep=""))
rm(sourceDir)

# START ###############################################################

# As a global variable

# App: ?
#FITBIT_KEY    <- "REDACTED"
#FITBIT_SECRET <- "REDACTED"
#FITBIT_CALLBACK <- "http://localhost:1410/" # May be "http://localhost:1410"

# App: Raghad/Lubdub
#FITBIT_KEY    <- "REDACTED"
#FITBIT_SECRET <- "REDACTED"
#FITBIT_CALLBACK <- "http://localhost:1410/" # May be "http://localhost:1410"

# App: Medly Research
FITBIT_KEY    <- "REDACTED"
FITBIT_SECRET <- "REDACTED"
FITBIT_CALLBACK <- "http://localhost:1410/" # May be "http://localhost:1410"
DATA_FILEPATH <- "fitbit/REDACTEDData-Clean-OnePerEmail.csv"
SAVE_FILEPATH <- "H:/REDACTED data/REDACTEDDataRawPull.Rdata"

# Patients

pullFitbitData <- function(rDataFilePath=SAVE_FILEPATH,createFromCSVFilePath=NULL)
{
  if(!is.null(createFromCSVFilePath))
  {
    importFilePath = createFromCSVFilePath
  } else {
    importFilePath = rDataFilePath
  }
  
  dataAndTokens <- importDownloadDataAndTokens(importFilePath)
  
  missingTokensReturn <- getMissingTokens(dataAndTokens)
  
  dataAndTokens <- missingTokensReturn$dataAndTokens
  
  if(missingTokensReturn$tokensWereUpdated)
  {
    exportDownloadedDataAndTokens(rDataFilePath,dataAndTokens)
  }
  
  dataAndTokens <- updateData(dataAndTokens)
  
  exportDownloadedDataAndTokens(rDataFilePath,
                              dataAndTokens)
}

getMissingTokens <- function(dataAndTokens)
{
  mDAndT.list <- dataAndTokens$metaDataAndTokens.list
  fitDat.df <- dataAndTokens$fitbitData.df
  firstMissingToken = TRUE
  
  for(studyID in names(mDAndT.list))
  {
    #cat(studyID,"\n")
    #cat("\n")
    
    # get participant data and token
    participantDaTok <- mDAndT.list[[studyID]]
    
    # get token, generate if needed
    if(is.na(participantDaTok$Token.Wrapped))
    { # i.e. missing token then we need to get it
      if(firstMissingToken)
      {
        # remind user that they need to make sure Fitbit is logged out (in default browser)
		# TODO: this is really cheap solution - see if there's a better way to do this when you have time 
        invisible(readline(prompt="W: REMINDER! Make sure Fitbit is logged out (in default browser), only then press [enter] to continue:\n"))
        firstMissingToken = FALSE
      }
      
      wrappedTokenObject <- getTokenForParticipant(mDAndT.list,
                                                   participantDaTok$StudyIdentifier,
                                                   participantDaTok$FitbitEmail)
      
      cat("M: Storing Token for ", studyID, "\n")
      
      # select [1] for first obs of participantDaTok dataframe
      participantDaTok[['Token.Wrapped']] <- NA
      participantDaTok[['Token.Wrapped']] <- list(NA)
      #participantDaTok[['Token.Wrapped']][[1]] <- vector(mode="list",
      #                                                   length=length(wrappedTokenObject))
      participantDaTok[['Token.Wrapped']][[1]] <- wrappedTokenObject
      mDAndT.list[[studyID]] <- participantDaTok
    }
  }
  
  return(list(dataAndTokens=list(metaDataAndTokens.list=mDAndT.list,
                                 fitbitData.df=fitDat.df),
              tokensWereUpdated=!firstMissingToken)
  )
}

updateData <- function(dataAndTokens)
{
  mDAndT.list <- dataAndTokens$metaDataAndTokens.list
  fitDat.df <- dataAndTokens$fitbitData.df
  
  for(studyID in names(mDAndT.list))
  {
    cat("\n", studyID,"\n")
    
    # get participant data and token
    participantDaTok <- mDAndT.list[[studyID]]
    
    # get last download date, generate if needed
    if(is.na(participantDaTok$LastDownloadDate))
    { # i.e. missing last download date, i.e. never downloaded
      participantDaTok$LastDownloadDate <- participantDaTok$StartDate
      mDAndT.list[[studyID]]$LastDownloadDate <- participantDaTok$LastDownloadDate
    }
    
    # download each date
    # get sequence of dates
    dates <- as.Date(NA)
    if(participantDaTok$LastDownloadDate <= participantDaTok$EndDate)
    {
      dates <- seq.Date(from=participantDaTok$LastDownloadDate,
                        to=participantDaTok$EndDate,
                        by="days")
      
      cat("M: Downloading new data for dates") # no new line
      detailLevel <- "1min"
      dataPointsPerDay <- 1440 # N.B. calculate given detail level specified above?
      lastDownloadRowIndex <- 0
      numDays <- dataPointsPerDay * length(dates)
      df_download <- getDefaultFitbitDataFrameWithSize(numDays,mDAndT.list)
      
      for(dateIndex in 1:length(dates)) #dates between study start date & study end date
      {
        date <- dates[[dateIndex]]
        
        cat("\n   - Date:", as.character(date))
        
        # select [1] for first obs of participantDaTok dataframe
        wrappedTokenObject <- mDAndT.list[[studyID]][['Token.Wrapped']][[1]] # get token
        wrappedTokenObject$token$refresh()   # refresh token (this is probably not needed)
        df_steps <- NA  # wipe previous
        df_steps <- fitbitr::get_activity_intraday_time_series(wrappedTokenObject, 
                                                               "steps",
                                                               date,
                                                               detail_level=detailLevel)
        
        numStepDataPoints <- ifelse(is.null(df_steps),
                                    0,
                                    nrow(df_steps))
        
        df_heart <- NA # wipe previous
        df_heart <- fitbitr::get_heart_rate_intraday_time_series(wrappedTokenObject,
                                                                 date,
                                                                 detail_level=detailLevel)
        wrappedTokenObject$token$refresh()  # refresh token (to help guarantee it's fresh for next time)
        # select [1] for first obs of participantDaTok dataframe
        mDAndT.list[[studyID]][['Token.Wrapped']][[1]] <- wrappedTokenObject  # saved refreshed token
        if(length(df_heart) > 0) {
          # i.e. no heart rate in time period
          df_join <- dplyr::left_join(df_steps,
                                      df_heart,
                                      by=c("dataset_time" = "time"))
        } else {

  ##### TODO SEE BELOW #####
          # TODO pulled less datapoints than expected (17 vs 1440)
          # Keep current index & increment then drop unused rows?
          # also once done testing change REDACTED end date back to REDACTED on REDACTEDData-Clean.csv
          df_join <- df_steps
          
          df_join$value.y <- rep(NA,numStepDataPoints)
        }
        
        #df_downloadRowIndicesVector <- (1+dataPointsPerDay*(dateIndex-1)):(dataPointsPerDay*dateIndex)  # if all days perfect
        df_downloadRowIndicesVector <- (lastDownloadRowIndex + 1):(lastDownloadRowIndex + numStepDataPoints)
        # when possible update below to be more intelligent (don't forget refactoring later)
        df_download[df_downloadRowIndicesVector,] <- data.frame(StudyIdentifier=rep(participantDaTok$StudyIdentifier,numStepDataPoints),
                                                                Steps=as.integer(df_join$dataset_value),
                                                                HeartRate=as.integer(df_join$value.y),
                                                                NYHAClass=factor(rep(participantDaTok$NYHA,numStepDataPoints),
                                                                                 levels=levels(CONSTANTS$NYHA_CLASS_VEC)),
                                                                DateTime=as.POSIXct(as.character(paste(df_join$dateTime,
                                                                                                       df_join$dataset_time)),
                                                                                    # tz='UTC', # warning Fitbit API has no tz associated (returns Fitbit time) therefore any 'dumb' selection will be wrong
                                                                                    format="%Y-%m-%d %H:%M:%S"),  # N.B. cast as local, EDT timezone
                                                                stringsAsFactors=FALSE)
        #colnames(df_download) <- c("time","steps","hr")
        
        #lastDownloadRowIndex <- lastDownloadRowIndex + numStepDataPoints
        lastDownloadRowIndex <- tail(df_downloadRowIndicesVector, n=1)
      }
      
      # Merge with old dataframe of points
      fitDat.df <- rbind(df_download,fitDat.df)
      
      fitDat.df <- dplyr::distinct_(fitDat.df,"StudyIdentifier","DateTime", .keep_all=TRUE)
      
    }
    else
    {
      cat("M: Skipping downloading new data for this participant since last Download Date (",
          as.character(participantDaTok$LastDownloadDate),
          ") is after participant study End Date (",
          as.character(participantDaTok$EndDate), ")\n",
          sep="")
    }
  }
  
  fitDat.df <- dplyr::arrange(fitDat.df,StudyIdentifier,DateTime)
  
  return(list(metaDataAndTokens.list=mDAndT.list,
              fitbitData.df=fitDat.df))
}

getTokenForParticipant <- function(metaDataAndTokens.list,
                                   studyIdentifier="unspecified",
                                   email="unknown")
{
  # https://github.com/teramonagi/fitbitr
  
  KEY <- attr(metaDataAndTokens.list, "FITBIT_KEY")
  SECRET <- attr(metaDataAndTokens.list, "FITBIT_SECRET")
  CALLBACK <- attr(metaDataAndTokens.list, "FITBIT_CALLBACK")
  LOCALE <- NULL # i.e. use default: US English. See https://dev.fitbit.com/reference/web-api/basics/#language
  LANGUAGE <- NULL # i.e. use default: metric units
  
  # BELOW WE EMULATE THE PROCESS FOR GETTING A TOKEN AS PER teramonagi/fitbitr BUT WITH use_basic_auth=TRUE
  
  # What types of data do we allow to access -> as possible as we can
  scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")
  
  content_type <- httr::content_type("application/x-www-form-urlencoded")
  
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  endpoint <- httr::oauth_endpoint(request, authorize, access)
  header <- httr::add_headers(Authorization=paste0("Basic ", base64encode(charToRaw(paste0(KEY, ":", SECRET)))))
  myapp <- httr::oauth_app(appname="REDACTED Research",
                           key=KEY,
                           secret=SECRET,
                           redirect_uri=CALLBACK)
  
  
  # query <- list(email = "REDACTED@gmail.com", password="test") # no place to put query below that will pass through?
  cat("\nM: Request new token for participant ",
      as.character(studyIdentifier), " - use email: ", email, sep="")

  token <- httr::oauth2.0_token(endpoint,
                                myapp,
                                scope=scope,
                                use_basic_auth=TRUE,
                                config_init=c(header, content_type),
                                cache=FALSE,
                                query_authorize_extra=list(email=email,
                                                           prompt="login"))
  token$refresh() # for good measure?
  
  cat("\nM: Received token response")
  #&email=REDACTED@gmail.com
  return(
    list(
      token=token,
      locale=LOCALE,
      language=LANGUAGE
  ))
}

importDownloadDataAndTokens <- function(filePath,
                                        fitbit_key=FITBIT_KEY,
                                        fitbit_secret=FITBIT_SECRET,
                                        fitbit_callback=FITBIT_CALLBACK)
{
  cat("\nM: Importing data and tokens from ", filePath, " (relative to current working directory)...", sep="")
  
  createNewDataSet = FALSE
  if(!(is.null(filePath) || is.na(filePath) || (nchar(filePath) < 1)))
  {
    if(length(grep("Rdata",tools::file_ext(filePath),ignore.case=TRUE,value=TRUE)) > 0)
    {
      load(filePath)
    } else if(length(grep("csv",tools::file_ext(filePath),ignore.case=TRUE,value=TRUE)) > 0) {
      createNewDataSet = TRUE
    } else {
      stop("E: must import a Rdata dataset. If none exists supply filePath=NULL to create one from DATA_FILEPATH")
    }
  }
  
  if(createNewDataSet)
  {
    warning("No filePath specified. Starting with blank/default data and tokens.") 
    #FITBIT_KEY
    #FITBIT_SECRET
    #FITBIT_CALLBACK
    
    # Read in Sample Data
    filePath <- DATA_FILEPATH
    metaDataAndTokens.df <- read_csv(filePath)
    
    # Clear extra imported attributes
    attr(metaDataAndTokens.df, "spec") <- NULL

    # Format & Add Additional Variables
    
    metaDataAndTokens.df$StudyIdentifier <- as.factor(metaDataAndTokens.df$`Study Identifier`)
    metaDataAndTokens.df$`Study Identifier` <- NULL # rename Study Identifier
    metaDataAndTokens.df$Token.Wrapped <- NA
    metaDataAndTokens.df$LastDownloadDate <- NA
    metaDataAndTokens.df$FitbitEmail <- metaDataAndTokens.df$'Fitbit Email'
    metaDataAndTokens.df$'Fitbit Email' <- NULL
    
    # Rename table rows
    #row.names(metaDataAndTokens) <- metaDataAndTokens$StudyIdentifier
    
    # Reshape into list
    metaDataAndTokens.list <- split(metaDataAndTokens.df, 
                                    seq(nrow(metaDataAndTokens.df)))
    metaDataAndTokens.list <- setNames(split(metaDataAndTokens.df, 
                                             seq(nrow(metaDataAndTokens.df))),
                                       metaDataAndTokens.df$StudyIdentifier)

    # Store Metadata
    attr(metaDataAndTokens.list, "FITBIT_KEY") <- fitbit_key
    attr(metaDataAndTokens.list, "FITBIT_SECRET") <- fitbit_secret
    attr(metaDataAndTokens.list, "FITBIT_CALLBACK") <- fitbit_callback
    
    # Test Row Retrieval
    #slot <- metaDataAndTokens.list[["A"]]
    
    # Create empty Fitbit Data Dataframe
    fitbitData.df <- getDefaultFitbitDataFrameWithSize(0,metaDataAndTokens.list)
  }
  
  if(!exists("metaDataAndTokens.list"))
  {
    stop("E: No data and tokens variable ('metaDataAndTokens.list') exists. If desired run function with filepath=NULL to create the variable.")
  }
  if(!exists("fitbitData.df"))
  {
    stop("E: No data and tokens variable ('fitbitData.df') exists. If desired run function with filepath=NULL to create the variable.")
  }
  
  cat("\nM: Finished importing/loading up data and tokens", sep="")

  return(list(metaDataAndTokens.list=metaDataAndTokens.list,
              fitbitData.df=fitbitData.df))
}

exportDownloadedDataAndTokens <- function(filePath,
                                          dataAndTokens)
{
  
  dataAndTokens$metaDataAndTokens.list <- refreshAllTokens(dataAndTokens$metaDataAndTokens.list)  # for good measure
  
  metaDataAndTokens.list=dataAndTokens$metaDataAndTokens.list
  fitbitData.df=dataAndTokens$fitbitData.df
  FITBIT_KEY=attr(dataAndTokens$metaDataAndTokens.list,"FITBIT_KEY") # this makes me uncomfortable but we have to guarantee it exists to save
  FITBIT_SECRET=attr(dataAndTokens$metaDataAndTokens.list,"FITBIT_SECRET")
  FITBIT_CALLBACK=attr(dataAndTokens$metaDataAndTokens.list,"FITBIT_CALLBACK")
  
  cat("\nM: Saving data and tokens...")
  saveVariables <- c("metaDataAndTokens.list",
                     "fitbitData.df",
                     "FITBIT_KEY",
                     "FITBIT_SECRET",
                     "FITBIT_CALLBACK")
  save(list = saveVariables,
       file = filePath,
       ascii = TRUE,
       #version = NULL, 
       #envir = parent.frame(),
       #compress = isTRUE(!ascii), 
       #compression_level,
       #eval.promises = TRUE, 
       precheck = TRUE)
  cat("\nM: Finished saving to ", filePath, " (relative to current working directory)...", sep="")
}

refreshAllTokens <- function(metaDataAndTokens.list)
{
  cat("\nM: Retrieving all new refresh tokens...")
  
  for(listIndex in length(metaDataAndTokens.list))
  {
    tok <- metaDataAndTokens.list[[listIndex]][['Token.Wrapped']][[1]]
    if(is.list(tok)) # by inteference takes care of !is.na
    {
      tok$token$refresh()
      metaDataAndTokens.list[[listIndex]][['Token.Wrapped']][[1]] <- tok
    }
  }
  return(metaDataAndTokens.list)
}

getDefaultFitbitDataFrameWithSize <- function(size,metaDataAndTokens.list,fill=NA)
{
  frame <- data.frame(StudyIdentifier=factor(rep(fill,size),levels=names(metaDataAndTokens.list)),
                       Steps=as.integer(rep(fill,size)),
                       HeartRate=as.integer(rep(fill,size)),
                       NYHAClass=factor(rep(fill,size),levels=levels(CONSTANTS$NYHA_CLASS_VEC)),
                       DateTime=as.POSIXct(as.character(rep(fill,size))),
                       #Handedness,
                       #WristbandPreference,
                       #NYHAClassMixed,
                       #NYHAClass,  # duplicate
                       #EjectionFraction,
                       #HFDiagnosisYear,
                       #HFTreatmentsToDate,
                       #RegularPhysicalActivities,
                       #Exercises,
                       #DeviceID,
                       #ID,
                       #CPSDate,
                       #Sex,
                       #Age,
                       #Height,
                       #Weight,
                       #SBP.Resting,
                       #DBP.REsting,
                       #HR.Resting,
                       #O2Sat.Resting,
                       #FEV.Resting,
                       #FEV.Resting.Percentage,
                       #FVC.Resting,
                       #FVC.Resting.Percentage,
                       #SBP,
                       #DBP,
                       #HR,
                       #HR.1min,
                       #HR.1minDrop,
                       #O2Sat,
                       #Duration,
                       #Watts,
                       #Watts.Predicted.Percentage,
                       #TestEnd.Reason
                       #TestEnd.Symptom
                       #VO2.perBodyWeight,
                       #VO2.Predicted.Relative,
                       #VO2.Predicted.Relative.Percentage,
                       #VO2.Predicted.Absolute,
                       #VO2.Predicted.Absolute.Percentage,
                       #VE.Peak
                       #VCO2.Peak
                       #VEperVCO2.Peak
                       #VEperVCO2.atAT
                       #AT
                       #VO2.Peak.Measured.Percentage,
                       #VO2.Peak.Predicted.Percentage,
                       #RER.Peak
                       #RPEper20.Peak
                       #PETCO2.Peak
                       #OUES,
                       #TotalRiskScore,
                       stringsAsFactors=FALSE)
  return(frame)
}