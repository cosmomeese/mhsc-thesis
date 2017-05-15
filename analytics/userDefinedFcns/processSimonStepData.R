### Process Function for Fitbit Step Data (Simon Bromberg's Thesis)

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

processSimonStepData <- function(importedData)
{
  
  #for every sheet apply
  for(sheetName in names(importedData))
  {
    importedData.listItem <- importedData[[sheetName]]
    
    if(!(is.null(importedData.listItem[["Date"]]) || #Only include if "Date" & "steps" included 
         is.null(importedData.listItem[["steps"]]))) #WARN!: MUST BE LOWERCASE steps
    { #i.e. if data exists
      
      ## Reformat Data for Use
      #Make sure data frame names are compatible with R
      names(importedData.listItem) <- make.names(names(importedData.listItem),
                                                 unique=FALSE)
      
      #DateTime (and create new processedData object)
      inputFormat <- "%m/%d/%Y %H:%M:%S"
      DateTime <- as.POSIXlt(importedData.listItem[["Date"]], 
                             format = inputFormat) #access Date's column & change from 'character' (string) type to date
    
      processedData.listItem <- data.frame(DateTime) #remember df = data.frame(x) will be referred to as df$x
      rm(DateTime) #remove DateTime since we only need it to initialize data.frame
      
      #Time
      outputFormat <- "%H:%M:%S"
      processedData.listItem['Time'] <- format(processedData.listItem$DateTime,
                                      format=outputFormat) #access new DateTime column & change from POSIXlt format to time
      
      #Date  
      outputFormat <- "%Y/%m/%d"
      processedData.listItem['Date'] <- format(processedData.listItem$DateTime,
                                      format=outputFormat) #access new DateTime column & change from POSIXlt format to time
      
      #Steps
      processedData.listItem['Steps'] <- importedData.listItem$steps #make everything consistently uppercase
      
      ## Remove unneeded Data
      processedData.listItem$DateTime <- NULL
      
      # Nest all other variables (Step & Time) for each Date
      processedData.listItem <- tidyr::nest(data = dplyr::group_by(processedData.listItem,Date),
                                            .key = "Intraday")
      
      processedData.listItem["StudyIdentifier"] <- sheetName
      
      # Combine data frame with our existing list of data frames
      if(!exists("processedData")) # Create the data frame initially if it hasn't yet been created
      {
        processedData <- processedData.listItem
      }
      else #Tack on the new data frame to the existing data frame
      {
        processedData <- rbind(processedData,processedData.listItem)
      }
    }
  }
  
  #tidy it up
  processedData <- tidyr::nest(data = group_by(processedData,StudyIdentifier),
                               .key = "StepData")
  
  return(processedData)
}

