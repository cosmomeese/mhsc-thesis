### Analyze Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

#local functions

analyzeSimonData <- function(processedData)
{
  ### Computed Data for each Participant/Sheet
  analyzedData <- sapply(processedData,function(processedData.listItem){
    
        ## Analyze Data
        
        # Average Daily Step Count
        dailyStepCount <- aggregate(formula = Steps ~ Date,
                                    data = processedData.listItem,
                                    FUN = sum)
        
        analyzedData.listItem <- data.frame(dailyStepCount)
        
        # Correlation of Daily Step Count & NYHA Class
        
        return(analyzedData.listItem)
      },
      simplify = FALSE,
      USE.NAMES = TRUE)
  #end iterative function for each participant

  ### Computed Data for Overall Set
  analyzedData['_SUMMARY_DATA_'] <- 1
  
  
  return(analyzedData)
}

