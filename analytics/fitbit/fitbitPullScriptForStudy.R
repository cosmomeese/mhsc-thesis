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

# START ###############################################################

# As a global variable

cat('sourcing default keys')
sourceDir <- 'fitbit'
source(paste(sourceDir,"/","fitbitAPIKeys.R",
             sep=""))

# Above file simply contains the following four lines (top remains commented) and 
# with the corresponding values filled out based on the info in your Fitbit developer account)
##### START CONTENTS OF fitbitAPIKeys.R file

## App: App Name
#FITBIT_KEY    <- "App Key"
#FITBIT_SECRET <- "App Secret Key"
#FITBIT_CALLBACK <- "App Callback"

##### END CONTENTS OF fitbitAPIKeys.R file

# Filepaths: Test Cases/Examples
#CSV_FILEPATH_EG <- "H:/fitbitLogins/fitbitLoginsForScript-Part1-ExampleTest.csv"
#CSV_FILEPATH_EG2 <- "H:/fitbitLogins/fitbitLoginsForScript-Part2-ExampleTest - New Data.csv"
#RDATA_SAVE_FILEPATH_EG <- "H:/fitbitLogins/DataPullExample.Rdata"

# Filepaths: Study Files
#CSV_FILEPATH <- "H:/fitbitLogins/fitbitLogins-Clean-OnePerEmail.csv"
CSV_FILEPATH <- "H:/fitbitLogins/fitbitLoginsForScript-Part1-NoMissingPasswords.csv"
CSV_FILEPATH2 <- "H:/fitbitLogins/fitbitLoginsForScript-Part2-NoMissingPasswords.csv"
CSV_FILEPATH3 <- "H:/fitbitLogins/fitbitLoginsForScript-Part3-NoMissingPasswords.csv"
CSV_FILEPATH4 <- "H:/fitbitLogins/fitbitLoginsForScript-Part4-NoMissingPasswords.csv"
CSV_FILEPATH5 <- "H:/fitbitLogins/fitbitLoginsForScript-Part5-NoMissingPasswords.csv"
CSV_FILEPATH_ALL <- "H:/fitbitLogins/fitbitLoginsForScript-Part1to5-NoMissingPasswords.csv"
CSV_FILEPATH_OVERWITE <- "H:/fitbitLogins/fitbitLoginsForScript-PartX-ProblemChildren.csv"

RDATA_SAVE_FILEPATH <- "H:/jfbData/JFBDataRawPull.Rdata"

# Patients

#### MAIN FUNCTION ####

# rDataFilePath : the save path for RData
# addFromCSVFilePath : the import path for new CSV
# if rDATA is the only path specified then load only RDATA & update it
# if rDATA & CSV is specified then:
#    if rDATA file exists then load rDATA, add new values in CSV & update it
#    if rDATA file does not exist then create new rDATA file from CSV.
# e.g.: 
# to setup:
# fitbitData <- pullFitbitData(RDATA_SAVE_FILEPATH,CSV_FILEPATH_ALL)
#
# to deal with specific problematic IDs (populate CSV_FILEPATH_OVERWITE file appropriately)
# fitbitData <- pullFitbitData(RDATA_SAVE_FILEPATH,CSV_FILEPATH_OVERWITE,overwriteTokenMetaDataFromCSV=TRUE)
#
# to refresh after all setup
# fitbitData <- pullFitbitData(RDATA_SAVE_FILEPATH,NULL)
pullFitbitData <- function(rDataFilePath=RDATA_SAVE_FILEPATH,
                           addFromCSVFilePath=CSV_FILEPATH_ALL,
                           resetDownloadDates=FALSE,
                           overwriteTokenMetaDataFromCSV=TRUE)
{
  if(!is.null(rDataFilePath)) #if rDATAFilePath is given
  {
    importRDataFilePath <- rDataFilePath # compute the seperate import path if required
    if(!file.exists(rDataFilePath))
    {
      importRDataFilePath <- NULL
    }
  }
  else #if rDATAFilePath is not given
  {
    stop("must specify rDATAFilePath for rDATAFile output")
  }
  
  dataAndTokens <- importDownloadDataAndTokens(rFilePath=importRDataFilePath,
                                               csvFilePath=addFromCSVFilePath,
                                               overwriteTokenMetaDataFromCSV=overwriteTokenMetaDataFromCSV
  )
  
  missingTokensReturn <- getMissingTokens(dataAndTokens)
  
  dataAndTokens <- missingTokensReturn$dataAndTokens
  
  if(missingTokensReturn$tokensWereUpdated)
  { # back up before running update
    exportDownloadedDataAndTokens(rDataFilePath,dataAndTokens)
  }
  
  if(resetDownloadDates)
  {
    cat("resetting last downloaded dates")
    dataAndTokens <- clearLastDownloadDates(dataAndTokens)
  }
  
  # update data
  dataAndTokens <- updateData(dataAndTokens)
  
  # save updated data
  exportDownloadedDataAndTokens(rDataFilePath,
                                dataAndTokens)
  
  return(dataAndTokens)
}

### SUPPORTING FUNCTIONS ####

importFitbitDataOnly <- function(USE.CHOOSEDIALOG=force.ChooseDialog,
                                 PULL.NEW.DATA=FALSE)
{
  ##### TODO ####
  ## Specify File Location
  if(USE.CHOOSEDIALOG)
  {
    rDataFileName <- NULL
    csvFileName <- NULL
    rDataDialogTitle <- "Select RData File (Part 1)"
    csvDialogTitle <- "Select CSV Data File (Part 2)"
    basePath <- NULL
    baseToFilePath <- NULL
    
  }
  else
  {
    basePath <- getwd() #if local
    #basePath <- "" #for network drive... tbd
    baseToFilePath <- "data(gitignored)/" #again if needed
    rDataFileName <- RDATA_SAVE_FILEPATH
    csvFileName <- NULL
    rDataDialogTitle <- NULL
    csvDialogTitle <- NULL
  }
  
  fitbitData <- NA
  if(PULL.NEW.DATA)
  {
    fitbitData <- fitbitData(rDataFilePath=rDataFileName,
                             addFromCSVFilePath=csvFileName)$fitbitData.df
  }
  else
  {
    
  }
  
  
  ## Specify Important Excel Import Parameters
  initialRowsToSkip <- 2 #skip the first two
  excludeSheets = c("Summary","Daily","template")
  #columnTypes <- c("text","numeric","skip","skip")
  columnTypes = NULL
  ## END SPECIFY IMPORTANT EXCEL IMPORT PARAMETERS
  
  ## Perform Data Import
  
  ## Specify File Location
  if(is.null(fileName))
  {
    if(is.null(CHOOSE.DIALOGTITLE))
    {
      CHOOSE.DIALOGTITLE<-"Select file"
    }
    FILTS <- matrix(c("Excel",".xls","Excel", ".xlsx", "All Files", "*"),3,2,byrow = TRUE)
    cat("\nOpened Dialog Box (may be hidden behind RStudio - thanks Studio :S) -> Please ",CHOOSE.DIALOGTITLE, "\n", sep = "")
    fullFilePath <- tcltk::tk_choose.files(default = "",
                                           caption = CHOOSE.DIALOGTITLE,
                                           multi = FALSE,
                                           filters = FILTS,
                                           index = 1)
    if(0 == length(fullFilePath)) #if user hasn't selected anything (i.e. cancelled) then error
    {
      cat("Cancelled File Selection Dialog or Failed to Retrieve File","\n",sep="")
      stop('Filename not specified or chosen. (fcn: importExcelSheets)')
    }
    else
    {
      cat("Retrieved file @ \n", fullFilePath, "\n\n", sep = "")
    }
  }
  else
  {
    if(is.null(basePath))
    {
      fullFilePath <- fileName #if no basePath
    }
    else
    {
      if(is.null(baseToFilePath)) #if baseToFilePath is unspecified by this point but basePath & fileName are specified it's fair to assume that there is nothing special here so given how we construct the fullFilePath we replace this with an empty string
      {
        baseToFilePath = "" 
      }
      
      fullFilePath <- paste(basePath,"/",baseToFilePath,fileName,
                            sep="") #if basePath & baseToFilePath are specified (even by default) then combine them all to get complete path
    }
  }
  
  return(importedData)
}

getMissingTokens <- function(dataAndTokens)
{
  mDAndT.list <- dataAndTokens$metaDataAndTokens.list
  fitDat.df <- dataAndTokens$fitbitData.df
  firstMissingToken = TRUE
  
  for(idx in 1:length(mDAndT.list))
  {
    #cat(studyID,"\n")
    #cat("\n")
    
    # get participant data and token
    participantDaTok <- mDAndT.list[[idx]]
    
    # get token, generate if needed
    if(is.na(participantDaTok$Token.Wrapped))
    { # i.e. missing token then we need to get it
      if(firstMissingToken)
      {
        # remind user that they need to make sure Fitbit is logged out (in default browser)
        invisible(readline(prompt="W: REMINDER! Make sure Fitbit is logged out (in default browser), only then press [enter] to continue:\n"))
        firstMissingToken = FALSE
      }
      
      pwd <- ""
      if("Password" %in% colnames(participantDaTok))
      {
        pwd <- participantDaTok$Password
      }
      wrappedTokenObject <- getTokenForParticipant(mDAndT.list,
                                                   participantDaTok$StudyIdentifier,
                                                   participantDaTok$FitbitEmail,
                                                   pwd)
      
      studyID <- as.character(participantDaTok$StudyIdentifier)
      cat("M: Storing Token for ", studyID, "\n")
      
      # select [1] for first obs of participantDaTok dataframe
      participantDaTok[['Token.Wrapped']] <- NA
      participantDaTok[['Token.Wrapped']] <- list(NA)
      #participantDaTok[['Token.Wrapped']][[1]] <- vector(mode="list",
      #                                                   length=length(wrappedTokenObject))
      participantDaTok[['Token.Wrapped']][[1]] <- wrappedTokenObject
      mDAndT.list[[idx]] <- participantDaTok
      participantDaTok$Password <- NA
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
  
  #convert fitDat.df NYHA class to character in case we add new stuff
  fitDat.df$NYHAClass <- as.character(fitDat.df$NYHAClass)
  mDAndT.names <- names(mDAndT.list)
  
  for(idx in 1:length(mDAndT.list))
  {
    
    # get participant data and token
    participantDaTok <- mDAndT.list[[idx]]
    
    # get studyID
    studyID <- as.character(participantDaTok$StudyIdentifier)
    cat("\n", studyID, "(MetaData Token Name:", mDAndT.names[[idx]], ")", "\n")
    
    # get download start date (which should be last download date), generate if needed
    if(is.na(participantDaTok$LastDownloadDate))
    { # i.e. missing last download date, i.e. never downloaded
      downloadStartDate <- participantDaTok$StartDate
    }
    else
    {
      downloadStartDate <- participantDaTok$LastDownloadDate
    }
    
    # download each date
    # get sequence of dates
    dates <- as.Date(NA)
    currentTime <- as.POSIXct(format(Sys.time()), tz="UTC") # get current system time but transfer local time to GMT w/o adjustment; https://stackoverflow.com/a/30905052
    downloadEndDate <- as.Date(currentTime)-1
    if(!is.na(participantDaTok$EndDate))
    { # i.e. missing last download date, e.g. still in progress
      downloadEndDate <- participantDaTok$EndDate
    }
    else
    {
      cat("M: No study end date specified for participant, \n   defaulting to downloading all data up to (& including) yesterday (",
          as.character(downloadEndDate), ")\n",
          sep="")
    }
    
    
    if(downloadStartDate <= downloadEndDate)
    {
      dates <- seq.Date(from=downloadStartDate,
                        to=downloadEndDate,
                        by="days")
      
      cat("M: Downloading new data for dates") # no new line
      detailLevel <- "1min"
      dataPointsPerDay <- 1440 # fixed based on 1 minute interval
      lastDownloadRowIndex <- 0
      numDays <- dataPointsPerDay * length(dates)
      df_download <- getDefaultFitbitDataFrameWithSize(numDays,mDAndT.list)
      
      isContinueDownload <- TRUE
      lastDateDownloaded <- downloadStartDate
      lastDateDownloadIndex <- 0
      for(dateIndex in 1:length(dates)) #dates between study start date & study end date
      {
        if(isContinueDownload)
        {
          currDate <- dates[[dateIndex]]
          
          df_steps <- NULL  # wipe previous
          df_heart <- NULL # wipe previous
          
          # select [1] for first obs of participantDaTok dataframe
          wrappedTokenObject <- mDAndT.list[[idx]][['Token.Wrapped']][[1]] # get token
          
          # API requests
          
          
          
          #=== TOKEN REFRESH #====
          wrappedTokenObject <- safeRefresh(wrappedTokenObject)
          #=== END TOKEN REFRESH
          
          if(any(!is.na(wrappedTokenObject))) # i.e. list can contain NA's but if wrappedTokenObject proper is NA then will fail out
          {
            #=== STEPS #====
            potentialErr <- tryCatch({
              # get step data
              df_steps <- fitbitr::get_activity_intraday_time_series(wrappedTokenObject, 
                                                                     "steps",
                                                                     currDate,
                                                                     detail_level=detailLevel)
            },
            # supress warning
            #warning = apiReqWarnFcn, # suppressed
            error = apiReqErrFcn,
            finally = {}) # END tryCatch
            #=== END STEPS
            
            #=== HEART RATE #====
            potentialErr <- tryCatch({
              # get heart rate data
              df_heart <- fitbitr::get_heart_rate_intraday_time_series(wrappedTokenObject,
                                                                       currDate,
                                                                       detail_level=detailLevel)
            },
            # supress warning
            warning = apiReqWarnFcn, 
            error = apiReqErrFcn,
            finally = {}) # END tryCatch
            #=== END HEART RATE
            
            #=== TOKEN REFRESH 2 #====
            wrappedTokenObject <- safeRefresh(wrappedTokenObject)
            #=== TOKEN REFRESH 2
          }
          
          mDAndT.list[[idx]][['Token.Wrapped']][[1]] <- wrappedTokenObject  # saved refreshed token
          
          if(is.null(df_steps) || is.null(df_heart))
          {
            isContinueDownload <- FALSE
            
            # update that we've downloaded
            cat("\n   - Unable to download:", as.character(currDate))
          }
          else
          {
            lastDateDownloadIndex <- dateIndex
            
            # update that we've downloaded
            cat("\n   - Downloaded Date:", as.character(currDate))
            
            numStepDataPoints <- ifelse(is.null(df_steps) || !is.data.frame(df_steps),
                                        0,
                                        nrow(df_steps))
            
            # select [1] for first obs of participantDaTok dataframe
            
            
            # join heart & steps together (or if no df_heart add all NA heart column)
            if(!is.null(df_heart) && is.data.frame(df_heart) && nrow(df_heart) > 0) {
              # i.e. no heart rate in time period
              df_join <- dplyr::left_join(df_steps, 
                                          df_heart,
                                          by=c("dataset_time" = "time"))
            } else {
              
              ##### TODO? SEE BELOW #####
              # TODO pulled less datapoints than expected (17 vs 1440)
              # Keep current index & increment then drop unused rows?
              # also once done testing change firstlubdub@gmail.com end date back to 2016-04-19 on RaghadData-Clean.csv
              df_join <- df_steps
              
              df_join$value.y <- rep(NA,numStepDataPoints)
            }
            
            #df_downloadRowIndicesVector <- (1+dataPointsPerDay*(dateIndex-1)):(dataPointsPerDay*dateIndex)  # if all days perfect
            df_downloadRowIndicesVector <- (lastDownloadRowIndex + 1):(lastDownloadRowIndex + numStepDataPoints)
            # when possible update below to be more intelligent (don't forget refactoring later)
            df_download[df_downloadRowIndicesVector,] <- data.frame(StudyIdentifier=rep(participantDaTok$StudyIdentifier,numStepDataPoints),
                                                                    Steps=as.integer(df_join$dataset_value),
                                                                    HeartRate=as.integer(df_join$value.y),
                                                                    NYHAClass=as.character(rep(participantDaTok$NYHA,numStepDataPoints)),
                                                                    DateTime=as.POSIXct(as.character(paste(df_join$dateTime,
                                                                                                           df_join$dataset_time)),
                                                                                        # tz='UTC', # warning Fitbit API has no tz associated (returns Fitbit time) therefore any 'dumb' selection will be wrong
                                                                                        format="%Y-%m-%d %H:%M:%S"),  # N.B. cast as local, EDT timezone
                                                                    stringsAsFactors=FALSE)
            #colnames(df_download) <- c("time","steps","hr")
            
            #lastDownloadRowIndex <- lastDownloadRowIndex + numStepDataPoints
            lastDownloadRowIndex <- tail(df_downloadRowIndicesVector, n=1)
            
            # Merge with old dataframe of points
            fitDat.df <- rbind(df_download,fitDat.df) # note merge in this order to ensure dplyr::distinct keeps the newly downloaded values
            
            fitDat.df <- dplyr::distinct_(fitDat.df,"StudyIdentifier","DateTime", .keep_all=TRUE)
            
          }
          
        }
      }
      
      ### Update last download date
      
      # set last download date to the last successful download (previous index)
      if(lastDateDownloadIndex > 0) # only if a previous index exists
      {
        lastDateDownloaded <- dates[[lastDateDownloadIndex]]
      }
      else # we didn't download anything new, so keep original lastDownloadDate
      {
        lastDateDownloaded <- participantDaTok$LastDownloadDate
        cat("\nM: Unable to download any new data for participant, setting lastDownloadDate to (",
            as.character(lastDateDownloaded), ")\n",
            sep="")
      }
      participantDaTok$LastDownloadDate <- lastDateDownloaded
      mDAndT.list[[idx]]$LastDownloadDate <- participantDaTok$LastDownloadDate
      
    }
    else
    {
      cat("M: Skipping downloading new data for this participant since last Download Date (",
          as.character(downloadStartDate),
          ") is after participant study End Date (",
          as.character(downloadEndDate), ")\n",
          sep="")
    }
    cat("\n")
    
    cat("\nM: Adding/Updating NYHA class\n")
    # update NYHAClass
    nyhaClass <- mDAndT.list[[idx]]$NYHA # get NYHA Class
    fitDat.df$NYHAClass <- as.character(fitDat.df$NYHAClass) # convert to char as we can add potential non-Factor
    fitDat.df$NYHAClass[fitDat.df$StudyIdentifier == studyID] <- nyhaClass # add NYHA class
  }
  
  # convert fitDat.df to factor
  fitDat.df$NYHAClass <- as.factor(fitDat.df$NYHAClass) # convert to factor
  
  fitDat.df <- dplyr::arrange(fitDat.df,StudyIdentifier,DateTime)
  # drop all empty rows
  fitDat.df <- fitDat.df[rowSums(is.na(fitDat.df)) != ncol(fitDat.df),]
  # drop all rows with empty datetimes
  fitDat.df <- fitDat.df[!is.na(fitDat.df$DateTime),]
  
  ## ensure final result is arranged by StudyID then DateTime
  fitDat.df <- fitDat.df %>% dplyr::arrange(StudyIdentifier,DateTime)
  
  return(list(metaDataAndTokens.list=mDAndT.list,
              fitbitData.df=fitDat.df))
}

getTokenForParticipant <- function(metaDataAndTokens.list,
                                   studyIdentifier="unspecified",
                                   email="unknown",
                                   password="")
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
  myapp <- httr::oauth_app(appname="Medly Research",
                           key=KEY,
                           secret=SECRET,
                           redirect_uri=CALLBACK)
  
  
  # query <- list(email = "cosmomeese+fitbit@gmail.com", password="test") # no place to put query below that will pass through?
  cat("\nM: Request new token for participant ",
      as.character(studyIdentifier), "\n\t\t - use email: ", email, sep="")
  if(!is.null(password) && !is.na(password) && nchar(password) > 0)
  {
    cat("\n\t\t - with password: ", password, sep="")
  }
  
  token <- httr::oauth2.0_token(endpoint,
                                myapp,
                                scope=scope,
                                use_basic_auth=TRUE,
                                config_init=c(header, content_type),
                                cache=FALSE,
                                auth_page_query_params=list(email=email,
                                                            prompt="login"))
  result <- list(
    token=token,
    locale=LOCALE,
    language=LANGUAGE
  )
  
  result <- safeRefresh(result) # for good measure?
  
  cat("\nM: Received token response\n")
  #&email=cosmomeese@gmail.com
  return(result)
}

# rFilePath : the import path for RData
# csvFilePath : the import path for new CSV
# if rDATA is the only path specified then load only RDATA
# if csvFilePath is the only path specified then load only CSV
# if rDATA & CSV is specified then load rDATA + add new values from CSV
importDownloadDataAndTokens <- function(rFilePath,
                                        csvFilePath,
                                        overwriteTokenMetaDataFromCSV=FALSE,
                                        fitbit_key=FITBIT_KEY,
                                        fitbit_secret=FITBIT_SECRET,
                                        fitbit_callback=FITBIT_CALLBACK)
{
  
  importMsgPrefix <- "\nM: Importing data and tokens from "
  importMsgSuffix <- " (relative to current working directory)..."
  
  if(overwriteTokenMetaDataFromCSV && !is.null(csvFilePath))
  {
    overwriteWarnMsg <- paste0("W: DANGER AHEAD BEWARE!!!\n",
                               "You have selected to overwrite potential tokens.\n",
                               "Doing so will require you to re-authorize these tokens.\n",
                               "Enter 'Y' to confirm you want to do this | press CTRL+C or enter any other symbol to exit safely.\n")
    invisible(keyEntry <- readline(prompt=overwriteWarnMsg))
    if(toupper(keyEntry) != 'Y')
    {
      stopMsg <- "\r Program stopped safely; user requested exit."
      completeStopMsg <- simpleError(sprintf("\r%s\r",
                                             paste(rep(" ", getOption("width")-1L),
                                                   stopMsg,
                                                   collapse=" ")
      ))
      stop(completeStopMsg)
    }
  }
  
  importedRDATA <- FALSE
  uploadNewCSVValues <- FALSE
  
  # if valid rFilePath then load RData & notify
  if(!(is.null(rFilePath) || is.na(rFilePath) || (nchar(rFilePath) < 1)) &&
     length(grep("Rdata",tools::file_ext(rFilePath),ignore.case=TRUE,value=TRUE)) > 0)
  { 
    cat(importMsgPrefix, rFilePath, importMsgSuffix, sep="")
    load(rFilePath)
    importedRDATA <- TRUE
    
    errMsgPrefix <- "E: No data and tokens variable ('"
    errMsgSuffix <- "') exists. Imported RData file possibly corrupt."
    if(!exists("metaDataAndTokens.list"))
    {
      stop(paste(errMsgPrefix,"metaDataAndTokens.list",errMsgSuffix))
    }
    if(!exists("fitbitData.df"))
    {
      stop(paste(errMsgPrefix,"fitbitData.df",errMsgSuffix))
    }
  }
  
  # if valid csv then flag to add csv & notify
  if(!(is.null(csvFilePath) || is.na(csvFilePath) || (nchar(csvFilePath) < 1)) &&
     length(grep("csv",tools::file_ext(csvFilePath),ignore.case=TRUE,value=TRUE)) > 0)
  { # i.e. if valid CSV
    cat(importMsgPrefix, csvFilePath, importMsgSuffix, sep="")
    uploadNewCSVValues <- TRUE
  }
  
  # if no  valid RDATA or csv specified, then bug out
  if(!(importedRDATA || uploadNewCSVValues))
  {
    stop("E: must import a dataset. Specify either rFilePath or csvFilePath")
  }
  
  #### Add CSV Items ####
  if(uploadNewCSVValues)
  {
    # Read in Sample Data
    newCSV.df <- read_csv(csvFilePath)
    
    # Clear extra imported attributes
    attr(newCSV.df, "spec") <- NULL
    
    # Format & Add Additional Variables
    
    newCSV.df$StudyIdentifier <- as.factor(newCSV.df$`Study Identifier`)
    newCSV.df$`Study Identifier` <- NULL # rename Study Identifier
    newCSV.df$Token.Wrapped <- NA
    newCSV.df$LastDownloadDate <- NA
    newCSV.df$FitbitEmail <- newCSV.df$'Fitbit Email'
    newCSV.df$'Fitbit Email' <- NULL
    newCSV.df$StartDate <- as.Date(newCSV.df$StartDate,
                                   format="%d-%b-%y")
    newCSV.df$EndDate <- as.Date(newCSV.df$EndDate,
                                 format="%d-%b-%y")
    
    # Rename table rows
    #row.names(newCSV.list) <- newCSV.list$StudyIdentifier
    
    # Reshape into list
    newCSV.list <- split(newCSV.df, 
                         seq(nrow(newCSV.df)))
    newCSV.list <- setNames(split(newCSV.df, 
                                  seq(nrow(newCSV.df))),
                            newCSV.df$StudyIdentifier)
    
    if(!importedRDATA)
    {
      # Store Metadata
      attr(newCSV.list, "FITBIT_KEY") <- fitbit_key
      attr(newCSV.list, "FITBIT_SECRET") <- fitbit_secret
      attr(newCSV.list, "FITBIT_CALLBACK") <- fitbit_callback
      metaDataAndTokens.list <- newCSV.list
    }
    else
    {
      if(overwriteTokenMetaDataFromCSV)
      {
        metaDataAndTokens.list <- modifyList(metaDataAndTokens.list,
                                             newCSV.list)
      }
      else #overwriteTokenMetaDataFromCSV (keep old ones)
      {
        # isolate new values
        newEntryNames <- setdiff(names(newCSV.list),names(metaDataAndTokens.list))
        
        # append new values
        metaDataAndTokens.list <- modifyList(metaDataAndTokens.list,
                                             newCSV.list[newEntryNames])
      }
      
      # temp save attributes
      savedAttributes <- attributes(metaDataAndTokens.list)
      # reorder metaDataAndTokens.list (but causes loss of attributes, namely Fitbit Key, Secret & Callback)
      metaDataAndTokens.list <- metaDataAndTokens.list[order(names(metaDataAndTokens.list))]
      # replace names in saved attributes (otherwise these are lost)
      namesAttributeName <- "names"
      savedAttributes[[namesAttributeName]] <- names(metaDataAndTokens.list)
      # restore save attributes
      attributes(metaDataAndTokens.list) <- savedAttributes
    }
    
    # Test Row Retrieval
    #slot <- metaDataAndTokens.list[["A"]]
    
    # Create empty Fitbit Data Dataframe
    newFrame <- getDefaultFitbitDataFrameWithSize(0,newCSV.list)
    if(exists("fitbitData.df"))
    {
      if(overwriteTokenMetaDataFromCSV)
      {
        warning("W: MetaDataTokens updated. fitbitData.df may still contain extra undesired dates if token start/end dates have changed")
      }
      newFrame <- rbind(fitbitData.df,newFrame)
    }
    fitbitData.df <- newFrame
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
  
  cat("\nM: Finished saving to ", filePath, " (relative to current working directory)...\n", sep="")
}

safeRefresh <- function(wrappedTokenObject)
{
  potentialErr <- tryCatch({
    # refresh token (just in case. this is admittedly probably not needed)
    wrappedTokenObject$token$refresh()
  },
  # supress warning
  #warning = apiReqWarnFcn, # suppressed
  error = apiReqErrFcn,
  finally = {}) # END tryCatch
  
  # Reset wrapped object token if appropriate error is returned
  if("RESET_TOKEN" %in% names(potentialErr))
  {
    wrappedTokenObject <- NA
  }
  return(wrappedTokenObject)
}

refreshAllTokens <- function(metaDataAndTokens.list)
{
  cat("\nM: Retrieving all new refresh tokens...")
  
  for(listIndex in 1:length(metaDataAndTokens.list))
  {
    tok <- metaDataAndTokens.list[[listIndex]][['Token.Wrapped']][[1]]
    if(is.list(tok)) # by inteference takes care of !is.na
    {
      tok <- safeRefresh(tok)
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
                      NYHAClass=as.character(rep(fill,size)),
                      #levels=levels(CONSTANTS$NYHA_CLASS_VEC)),
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

# associated trycatch functions
apiReqWarnFcn <- function(war) {
  cat("\n   - W: Warning while downloading ", war$message, "\n")
  return(NULL)
}

apiReqErrFcn <- function(err) {
  result <- NULL
  
  tooManyRequestsErr <- "httr::content(response, : Too Many Requests (RFC 6585) (HTTP 429)"
  badRequestErrCall <- "refresh_oauth2.0(self$endpoint, self$app, self$credentials, self$params$user_params, self$params$use_basic_auth)"
  badRequestErrMsg  <- "Bad Request (HTTP 400)."
  
  
  cat("\n   - E: Error while downloading (",participantDaTok$StudyIdentifier,")")
  if(exists('currDate'))
  {
    cat("(", as.character(date), ")")
  }
  
  if(length(grep(tooManyRequestsErr,err$message, fixed = TRUE)))
  {
    cat(". Exceeded rate limit. Try again next hour.\n")
  }
  else if(length(grep(badRequestErrMsg,err$message, fixed = TRUE)) &&
          length(grep(badRequestErrCall,deparse(err$call, width.cutoff = 500), fixed = TRUE)))
  {
    cat(". Bad token")
    
    if(exists('participantDaTok'))
    {
      cat(" for ", participantDaTok$FitbitEmail)
    }
    cat(". \n\tClearing spoiled token.\n")
    result$RESET_TOKEN <- TRUE
  }
  else
  {
    cat("\n")
    err$message <- paste("Recast as warning: \n\t", err$message, "\n   Context:", err$call)
    warning(err$message) # i.e. mask the error (but return it)
  }
  #if(!startsWith(err$message,""))
  #{
  #  stop(err)
  #}
  return(result)
}

#### MANAGEMENT FCNS (SOME ARE ALSO ABOVE) #### 

clearLastDownloadDates <- function(dataAndTokens,metaDataAndTokens.list.directly=FALSE)
{
  lDDVariableName <- "LastDownloadDate"
  
  if(metaDataAndTokens.list.directly)
  {
    metaDataList <- dataAndTokens
  }
  else
  {
    mDAndTid <- "metaDataAndTokens.list"
    metaDataList <- dataAndTokens[[mDAndTid]]
  }
  
  for(idx in 1:length(metaDataList))
  {
    if(lDDVariableName %in% names(metaDataList[[idx]]))
    {
      metaDataList[[idx]][[lDDVariableName]] <- NA
    }
  }
  
  if(metaDataAndTokens.list.directly)
  {
    dataAndTokens <- metaDataList
  }
  else
  {
    dataAndTokens[[mDAndTid]] <- metaDataList
  }
  
  return(dataAndTokens)
}

makeSummaryCheckDF <- function(fitbit.df)
{
  df <- fitbit.df
  
  df <- df %>% group_by(StudyIdentifier) %>%
    dplyr::summarise(StartTime = first(DateTime),
                     EndTime = last(DateTime),
                     NumEle = length(DateTime),
                     NYHA.Start = first(NYHAClass),
                     NYHA.End = last(NYHAClass))
  
  return(df)
}

# optional newCSV.df can be the filepath to the appropriately formated CSV (with Study Identifier and NYHA columns)
updateAndOverwriteNYHAClass <- function(newCSV.df="H:/fitbitLogins/fitbitLogins-Clean-OnePerEmail.csv",
                                        fitbitData.df, metaDataAndTokens.list)
{
  # to test
  if(is.character(newCSV.df))
  {
    newCSV.df <- read_csv("H:/fitbitLogins/fitbitLogins-Clean-OnePerEmail.csv")
    newCSV.df$StudyIdentifier <- as.factor(newCSV.df$`Study Identifier`)
    newCSV.df$`Study Identifier` <- NULL # rename Study Identifier
  }
  validDF <- is.data.frame(newCSV.df) &&
             ("StudyIdentifier" %in% names(newCSV.df)) &&
             ("NYHA" %in% names(newCSV.df))
  if(!validDF)
  {
    error("newCSV.df has invalid columns, must have 'StudyIdentifier' and 'NYHA' columns")
  }
  
  ## Part 1: Update metaDataAndToken.list
  
  for(index in seq_along(metaDataAndTokens.list))
  {
    ele <- metaDataAndTokens.list[[index]]
    cat(as.character(ele$StudyIdentifier), " replaced NYHAClass", ele$NYHA)
    if(as.character(ele$StudyIdentifier) %in% newCSV.df$StudyIdentifier)
    {
      ele$NYHA <- as.character(newCSV.df$NYHA[as.character(newCSV.df$StudyIdentifier) == as.character(ele$StudyIdentifier)][[1]])
    }
    cat(" with ", ele$NYHA, "\n")
    metaDataAndTokens.list[[index]] <- ele
  }
  
  ## Part 2: Update fitbitdata.df
  
  trueFactorLevels <- levels(fitbitData.df$StudyIdentifier)
  # match the factor levels
  newCSV.df <- newCSV.df %>% mutate(StudyIdentifier = factor(as.character(StudyIdentifier), levels=trueFactorLevels))
  
  # remove factor levels from original fitbitData.df so we can modify
  fitbitData.df$NYHAClass <- as.character(fitbitData.df$NYHAClass)
  
  fitbitData.df <- merge(x=fitbitData.df,
                         y=(newCSV.df %>% subset(select=c("NYHA", "StudyIdentifier"))),
                         by="StudyIdentifier",
                         all.x=TRUE,
                         sort=FALSE)
  
  fitbitData.df$NYHAClass <- fitbitData.df$NYHA
  fitbitData.df$NYHA <- NULL
  fitbitData.df$NYHAClass <- as.factor(fitbitData.df$NYHAClass)
  

  
  return(list(metaDataAndTokens.list=metaDataAndTokens.list,
              fitbitData.df=fitbitData.df))
}