### Import Script for Fitbit/CPS Meta Data (Simon Bromberg's Thesis)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheets.R",
				sep=""))
rm(sourceDir)

#local functions

importSimonMetaData <- function(VIEW=FALSE) 
{
  
  ## Specify File Location
  basePath <- getwd() #if local
  #basePath <- "" #for network drive... tbd
  baseToFilePath <- "data(gitignored)/" #again if needed
  fileName <- "structure_ParticipantStudyInfo.xlsx"
  initialRowsToSkip <- 1 #skip the first one (since we specify col_names)
  #date helper function for later
    dateInputFormat = "%m/%d/%Y"
    as.DateWithSimonsFormat <- function(X,dIF=dateInputFormat,...){
      return(as.Date(X,dIF))
    }
  
    columnNames = c("StudyIdentifier", #Study Identifier
                    "Sex", #Sex (M/F)
                    "Weight", #Weight
                    "Height", #Height
                    "Age", #Age
                    "Handedness", #Handedness
                    "WristbandPreference", #wristband Preference
                    "NYHAClassMixed", #NYHA Class #(mixed)
                    "NYHAClass", #NYHA
                    "EjectionFraction", #EF
                    "HFDiagnosisYear", #HF Diagnosis date (year)
                    "HFTreatmentsToDate", #HF Treatement to Date
                    "RegularPhysicalActivities", #Regular Physical Activities
                    "Exercises", #Exercises
                    "ParticipationStartDate", #Start Date
                    "ParticipationEndDate", #End Date
                    "NumDaysInStudy", #[#days]
                    "DateGivenKit", #Date Given Kit
                    "DateReturnedKit", #Date Received Kit
                    "NumDaysWithKit", #days between end and return
                    "FitbitEmail", #Fitbit Email
                    "DeviceID", #Device #
                    "FedExTrackingNumber", #FedEx Tracking #
                    "GiftCardStatus", #Gift Card Status
                    "skip", #Fitbit Profile Settings Updated
                    "Recruiter", #Recruiter
                    "Downloaded", #Downloaded
                    "CardioPulmonaryTestCollected", #CPT collected
                    "Notes") #Notes
    
    columnTypes = c("text", #Study Identifier
                    "skip", #Sex (M/F)
                    "skip", #Weight
                    "skip", #Height
                    "skip", #Age
                    "text", #Handedness
                    "text", #wristband Preference
                    "text", #NYHA Class #(mixed)
                    "text", #NYHA
                    "numeric", #EF
                    "numeric", #HF Diagnosis date (year)
                    "text", #HF Treatement to Date
                    "text", #Regular Physical Activities
                    "numeric", #Exercises
                    "skip", #Start Date
                    "skip", #End Date
                    "skip", #[#days]
                    "skip", #Date Given Kit
                    "skip", #Date Received Kit
                    "skip", #days between end and return
                    "skip", #Fitbit Email
                    "text", #Device #
                    "skip", #FedEx Tracking #
                    "skip", #Gift Card Status
                    "skip", #Fitbit Profile Settings Updated
                    "skip", #Recruiter
                    "skip", #Downloaded
                    "skip", #CPT collected
                    "skip") #Notes
  
  names(columnTypes) <- columnNames #key columnType values to columnName keys (not really used but)
  
  columnTypeConversionFcns = c(as.character, #Study Identifier
                                as.factor, #Sex (M/F)
                                as.numeric, #Weight
                                as.numeric, #Height
                                as.numeric, #Age
                                as.factor, #Handedness
                                as.factor, #wristband Preference
                                as.factor, #NYHA Class #(mixed)
                                as.factor, #NYHA
                                as.numeric, #EF
                                as.numeric, #HF Diagnosis date (year)
                                as.character, #HF Treatement to Date
                                as.character, #Regular Physical Activities
                                as.logical, #Exercises
                                as.DateWithSimonsFormat, #Start Date
                                as.DateWithSimonsFormat, #End Date
                                as.numeric, #[#days]
                                as.DateWithSimonsFormat, #Date Given Kit
                                as.DateWithSimonsFormat, #Date Received Kit
                                as.numeric, #days between end and return
                                as.character, #Fitbit Email
                                as.factor, #Device #
                                as.character, #FedEx Tracking #
                                as.character, #Gift Card Status
                                as.factor, #Fitbit Profile Settings Updated
                                as.factor, #Recruiter
                                as.factor, #Downloaded
                                as.factor, #CPT collected
                                as.character) #Notes
  names(columnTypeConversionFcns) <- columnNames #key columnType values to columnName keys
  
  ## Perform Data Import
  
  importedData.sheets <- importExcelSheets(fileName,basePath,baseToFilePath,
                                           skip = initialRowsToSkip,
                                           col_types = columnTypes,
                                           col_names = columnNames,
                                           col_conversionFcns = columnTypeConversionFcns)
  

  
  # then assign final data to return variable
  importedData <- importedData.sheets
  
  return(importedData)
}
