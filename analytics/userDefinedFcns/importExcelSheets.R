### Import Generic Excel Sheet

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("readxl")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(readxl)

importExcelSheets <- function(fileName, basePath = getwd(), baseToFilePath = "", view=FALSE, excludeSheets=NULL, col_conversionFcns=NULL, ...)
{#NOTE: the ... means any other variables we pass to it
  
  ## Specify File Location
  fullFilePath <- paste(basePath,"/",baseToFilePath,fileName,
						sep="")
  
  ## Perform Data Import

  # Get Sheets
  sheets <- readxl::excel_sheets(fullFilePath)
  # Remove exclude sheets
  sheets <- sheets[!sheets %in% excludeSheets]
  
  importedData <- sapply(sheets,readxl::read_excel,
                                simplify = FALSE,
                                USE.NAMES = TRUE,
                                path = fullFilePath, #link path
                                ...) #pass along other variables (so we can still make use of all read_excel's other functionality)
  
  ### Helper function for import functions (for Simon Bromberg's Thesis) to convert
  
  if(!is.null(col_conversionFcns))
  {
      # Configure imported as appropriate data type for R (if col_conversionFcns are supplied)
      # first create some helper functions
      # to format the individual columns in sheets
      fcn.applyFormat <- function(sheet.colname,sheet,fcnList){
        
        #grab each sheet name (given)
        apply.function <- fcnList[[sheet.colname]] #grab the corresponding function name
        formattedColumn <- apply.function(sheet[[sheet.colname]]) #apply the function to the column@sheetname
        return(formattedColumn) #return the appropriate reformated column
      }
      # to apply the above funciton to all sheets
      fcn.applyFormatFcnToSheets <- function(sheet){
        
        updatedSheet.List <- sapply(X = colnames(sheet),
                               FUN = fcn.applyFormat,
                               sheet=sheet,
                               fcnList=col_conversionFcns,
                               simplify = FALSE,
                               USE.NAMES = TRUE)
        updatedSheet <- data.frame(updatedSheet.List) #convert the returned list to a data frame
        return(updatedSheet) #return the updated sheet
      }
      
      # then apply said helper functions to sheets and columns in sheets
      importedData <- sapply(X = importedData,
                                  FUN = fcn.applyFormatFcnToSheets,
                                  simplify = FALSE,
                                  USE.NAMES = TRUE)
  }
  
  ## Open view to display data, as specified
  if(view)
  {
    View(importedData)
  }

  return(importedData) #change the name on the right side of the assignment to whatever you want

}

