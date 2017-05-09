### Import Script for Fitbit Step Data (Simon Bromberg's Thesis)

## Import Required Libraries
sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheet.R",
				sep=""))
rm(sourceDir)
       
#local functions

importSimonStepData <- function(VIEW=FALSE)
{
  
  ## Specify File Location
  basePath <- getwd() #if local
  #basePath <- "" #for network drive... tbd
  baseToFilePath <- "data(gitignored)/" #again if needed
  fileName <- "structure_ThesisDummyData.xlsx"
  initialRowsToSkip <- 2 #skip the first two
  sheet <- "T001"
  columnTypes <- c("text","text","skip","skip") 
  
  ## Perform Data Import
  
  importedData <- importExcelSheet(fileName,basePath,baseToFilePath,
                                   skip = initialRowsToSkip,
                                   sheet = sheet,
                                   col_types = columnTypes)
  
  ## Reformat Data for Use
  
  inputFormat <- "%d/%m/%Y %H:%M:%S"
  importedData$DateTime <- as.POSIXlt(importedData[[1]], 
										format = inputFormat) #access 1's column & change from 'character' (string) type to date 
  
  #Time
  outputFormat <- "%H:%M:%S"
  importedData['Time'] <- format(importedData$DateTime,
									format=outputFormat) #access new DateTime column & change from POSIXlt format to time
  
  #Date  
  outputFormat <- "%d/%m/%Y"
  importedData['Date'] <- format(importedData$DateTime,
									format=outputFormat) #access new DateTime column & change from POSIXlt format to time
  
  #Steps
  importedData['Steps'] <- importedData$Steps
  
  ## Remove unneeded Data
  
  importedData$steps <- NULL
  importedData$DateTime <- NULL
  
  return(importedData) #change the name on the right side of the assignment to whatever you want
}

