### Import Script for Fitbit Step Data (Simon Bromberg's Thesis)

## Import Required Libraries
sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheets.R",
				sep=""))
rm(sourceDir)
       
#local functions

importSimonStepData <- function(VIEW=FALSE,USE.CHOOSEDIALOG=FALSE)
{
  
  ## Specify File Location
  if(USE.CHOOSEDIALOG)
  {
    fileName1 <- NULL
    fileName2 <- NULL
    dialogTitle1 <- "Select Step Data File (Part 1)"
    dialogTitle2 <- "Select Step Data File (Part 2)"
    basePath <- NULL
    baseToFilePath <- NULL

  }
  else
  {
    basePath <- getwd() #if local
    #basePath <- "" #for network drive... tbd
    baseToFilePath <- "data(gitignored)/" #again if needed
    fileName1 <- "structure_ThesisDummyData_1.xlsx"
    fileName2 <- "structure_ThesisDummyData_2.xlsx"
    dialogTitle1 <- NULL
    dialogTitle2 <- NULL
  }
  ## Specify Important Excel Import Parameters
  initialRowsToSkip <- 2 #skip the first two
  excludeSheets = c("Summary","Daily","template")
  #columnTypes <- c("text","numeric","skip","skip")
  columnTypes = NULL
  ## END SPECIFY IMPORTANT EXCEL IMPORT PARAMETERS
  
  ## Perform Data Import
  
  #First sheet
  importedData.part1 <- importExcelSheets(fileName1,basePath,baseToFilePath,
                                    skip = initialRowsToSkip,
                                    excludeSheets = excludeSheets,
                                    col_types = columnTypes,
                                    CHOOSE.DIALOGTITLE = dialogTitle1)
  #Second sheet (oh Simon...)
  importedData.part2 <- importExcelSheets(fileName2,basePath,baseToFilePath,
                                    skip = initialRowsToSkip,
                                    excludeSheets = excludeSheets,
                                    col_types = columnTypes,
                                    CHOOSE.DIALOGTITLE = dialogTitle2)
  #Merge sheets together
  importedData <-c(importedData.part1,importedData.part2)
  
  
  return(importedData)
}

