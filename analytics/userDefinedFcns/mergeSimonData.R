### Merge Function for Meta & actual Fitbit/CPS Data (Simon Bromberg's Thesis)

## Import Required Libraries

#local functions

mergeSimonData <- function(metaData,stepData,CPSData)
{
  #combine all the data frames together!
  mergeBy = "Study.Identifier"
  mergedData <- merge(x = metaData,
                      y = CPSData,
                      by = mergeBy)
  mergedData <- merge(x = mergedData,
                      y = stepData,
                      by = mergeBy)
  
  return(mergedData)
  
}

