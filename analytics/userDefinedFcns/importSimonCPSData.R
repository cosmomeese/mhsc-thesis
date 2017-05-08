### Import Script for Fitbit CPS Data (Simon Bromberg's Thesis)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheet.R",sep=""))
rm(sourceDir)

#local functions

importSimonCPSData <- function(VIEW=FALSE) 
{
  
  ## Specify File Location
  basePath <- getwd() #if local
  #basePath <- "" #for network drive... tbd
  baseToFilePath <- "data(gitignored)/" #again if needed
  fileName <- "structure_CPSDummyData.xlsx"
  initialRowsToSkip <- 2 #skip the first two
  
  ## Perform Data Import
  
  importedData <- importExcelSheet(fileName,basePath,baseToFilePath,
                                   skip = initialRowsToSkip)
    
  ## !Assign to the variable name you want
  
  return(importedData)
}
