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
  system.tz = "EST" #if you actually need timezone support then you will need to us something other than as.POSIXlt and as.POSIXct since these do not support timezones well: https://stackoverflow.com/questions/6071155/how-to-extract-the-correct-timezones-from-posixct-and-posixlt-objects
  
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
                             format = inputFormat,
                             tz = system.tz) #access Date's column & change from 'character' (string) type to date
    
      processedData.listItem <- data.frame(DateTime) #remember df = data.frame(x) will be referred to as df$x
      rm(DateTime) #remove DateTime since we only need it to initialize data.frame
      
      #Timestamp
      processedData.listItem['Timestamp'] <- processedData.listItem$DateTime #save the date time, just in case...
      
      #Time
      outputFormat <- "%H:%M:%S"
      tempTimeAsChar <- format(processedData.listItem$DateTime,
                     format=outputFormat) #access new DateTime column & change from POSIXlt format to character (stripping date)
      processedData.listItem['TimeResolution'] <- "minute"
      time.lookupTable <- format(seq(
                                      as.POSIXlt(min(tempTimeAsChar),
                                                 format = outputFormat,
                                                 tz = system.tz), 
                                      as.POSIXlt(max(tempTimeAsChar),
                                                 format = outputFormat,
                                                 tz = system.tz), 
                                      by = "1 min")) #use POSIXlt to avoid time zone problem
      processedData.listItem['Time.Index'] <- match(as.POSIXlt(tempTimeAsChar,
                                                               format = outputFormat,
                                                               tz = system.tz),
                                                    time.lookupTable) - 1 #match on lookup table (-1 offset since 
      processedData.listItem['Time'] <- as.POSIXct(tempTimeAsChar, 
                                                   format = outputFormat,
                                                   tz = "GMT") #convert to POSIXct format (using arbitrary date (today's date) + time desired)
      
      #Date
      outputFormat <- "%Y/%m/%d"
      tempDateAsChar <- format(processedData.listItem$DateTime,
                                      format=outputFormat) #access new DateTime column & change from POSIXlt format to character
      processedData.listItem['BaseDay'] <- as.Date(min(tempDateAsChar))
      date.lookupTable <- format(seq(as.Date(min(tempDateAsChar)),
                                     as.Date(max(tempDateAsChar)),
                                     by = "1 day"),
                                 format = outputFormat)
      processedData.listItem['Day'] <- match(tempDateAsChar,
                                             date.lookupTable)
      
      #Steps
      processedData.listItem['Steps'] <- importedData.listItem$steps #make everything consistently uppercase
      
      ## Remove unneeded Data
      processedData.listItem$DateTime <- NULL
      
      # Nest all other variables (Step & Time) for each Date
      processedData.listItem <- tidyr::nest(data = dplyr::group_by(processedData.listItem,Day),
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

