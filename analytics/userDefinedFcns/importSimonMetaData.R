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
  initialRowsToSkip <- 0 #skip the first two
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
                  "numeric", #HF Diagnosis date
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
                  "skip", #Fitbit Profile Settings Updated
                  "skip", #Downloaded
                  "skip", #CPT collected
                  "skip") #Notes
  
  ## Perform Data Import
  
  importedData.sheets <- importExcelSheets(fileName,basePath,baseToFilePath,
                                            skip = initialRowsToSkip,
                                            col_types = columnTypes)
    
  importedData <- importedData.sheets[1]
  ## !Assign to the variable name you want
  
  return(importedData)
}
