### Import Generic Excel Sheet

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("readxl","tcltk")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(readxl)
library(tcltk)

importExcelSheets <- function(fileName, basePath = getwd(), baseToFilePath = "", view=FALSE, excludeSheets=NULL, col_conversionFcns=NULL, CHOOSE.DIALOGTITLE="Select file",...)
{#NOTE: the ... means any other variables we pass to it
  
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

