### Function to unnest Step Data 

## Import Required Libraries
# Install if it's not installed on this computer
pkg <- c("tidyr","dplyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyr)
library(dplyr)

#local functions

unnestStepDataFrame <- function(originalOutputDataFrame)
{
  
  #initialize
  unnestedStepData <- data.frame(StudyIdentifier = NULL,
                                 Steps = NULL,
                                 Time = NULL,
                                 Data = NULL)
  
  #for every sheet apply
  for(sheetIndex in 1:nrow(originalOutputDataFrame))
  {
    #unnest
    stepDataForSheet <- originalOutputDataFrame$StepData[[sheetIndex]]
    if( is.data.frame(stepDataForSheet) && ( "Intraday" %in% colnames(stepDataForSheet) ) )
    {
      stepDataAlone <- tidyr::unnest(data = stepDataForSheet, Intraday)
      
      #only keep the ones we want
      stepDataAlone.Trimmed <- data.frame(Steps = stepDataAlone$Steps,
                                          Time = stepDataAlone$Time,
                                          Day = stepDataAlone$Day,
                                          stringsAsFactors = FALSE)
      #tag with the appropriate study identifier
      stepDataAlone.Trimmed$StudyIdentifier <- originalOutputDataFrame$StudyIdentifier[[sheetIndex]]
      
      #addd to the rest of the stepData
      unnestedStepData <- rbind(unnestedStepData,stepDataAlone.Trimmed)
    }
  }
  
  originalOutputDataFrame$StepData <- NULL
  
  unnestedDataFrame <- merge(x = originalOutputDataFrame,
                         y = unnestedStepData, 
                         by = "StudyIdentifier", 
                         all = TRUE)
  
  return(unnestedDataFrame)
}

