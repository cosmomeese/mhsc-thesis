### Import Script for Fitbit Step Data (Simon Bromberg's Thesis)

## Import Required Libraries
sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheets.R",
				sep=""))
rm(sourceDir)
       
#local functions

importSimonStepData <- function(VIEW=FALSE)
{
  
  ## Specify File Location
  basePath <- getwd() #if local
  #basePath <- "" #for network drive... tbd
  baseToFilePath <- "data(gitignored)/" #again if needed
  fileName1 <- "structure_ThesisDummyData_1.xlsx"
  fileName2 <- "structure_ThesisDummyData_2.xlsx"
  initialRowsToSkip <- 2 #skip the first two
  excludeSheets = c("Daily","template")
  #columnTypes <- c("text","numeric","skip","skip")
  columnTypes = NULL
  
  ## Perform Data Import
  
  #First sheet
  importedData.part1 <- importExcelSheets(fileName1,basePath,baseToFilePath,
                                    skip = initialRowsToSkip,
                                    excludeSheets = excludeSheets,
                                    col_types = columnTypes)
  #Second sheet (oh Simon...)
  importedData.part2 <- importExcelSheets(fileName2,basePath,baseToFilePath,
                                    skip = initialRowsToSkip,
                                    excludeSheets = excludeSheets,
                                    col_types = columnTypes)
  #Merge sheets together
  importedData <-c(importedData.part1,importedData.part2)
  
  
  return(importedData) #change the name on the right side of the assignment to whatever you want
}

