### Merge Function for Meta & actual Fitbit/CPS Data (Simon Bromberg's Thesis)

## Import Required Libraries

#local functions

mergeSimonData <- function(metaData,stepData,CPSData)
{
  #combine all the data frames together!
  mergeBy = "StudyIdentifier"
  mergedData <- merge(x = metaData,
                      y = CPSData,
                      by = mergeBy,
                      all = TRUE) #don't drop any rows you couldn't match
  mergedData <- merge(x = mergedData,
                      y = stepData,
                      by = mergeBy,
                      all = TRUE) #don't drop any rows you couldn't match
  
  return(mergedData)
  
}

