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

importExcelSheets <- function(fileName, basePath = getwd(), baseToFilePath = "", view=FALSE, excludeSheets=NULL, ...)
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
  
  for (sheet in sheets)
  {
    #importedData <- readxl::read_excel(path = fullFilePath, #link path
    #                                   sheet = sheet,
    #                                    ...) #pass along other variables (so we can still make use of all read_excel's other functionality)
  }

  
  ## Open view to display data, as specified
  if(view)
  {
    View(importedData)
  }

  return(importedData) #change the name on the right side of the assignment to whatever you want

}

