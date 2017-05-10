### Import Script for Fitbit CPS Data (Simon Bromberg's Thesis)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheets.R",
				sep=""))
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
  
  importedData.sheets <- importExcelSheets(fileName,basePath,baseToFilePath,
                                            skip = initialRowsToSkip)
    
  importedData <- importedData.sheets[1]
  
  return(importedData)
}
