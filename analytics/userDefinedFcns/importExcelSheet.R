### Import Generic Excel Sheet

## Import Required Libraries
library(readxl)

importExcelSheet <- function(fileName, basePath = getwd(), baseToFilePath = "", view=FALSE, ...)
{#NOTE: the ... means any other variables we pass to it
  
  ## Specify File Location
  fullFilePath <- paste(basePath,"/",baseToFilePath,fileName,sep="")
  
  ## Perform Data Import
  
  importedData <- readxl::read_excel(fullFilePath, ...) #pass along other variables (so we can still make use of all read_excel's other functionality)
  
  ## Open view to display data, as specified
  if(view)
  {
    View(importedData)
  }

  return(importedData) #change the name on the right side of the assignment to whatever you want

}

