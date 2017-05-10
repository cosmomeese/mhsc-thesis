### Process Function for Fitbit/CPS Meta Data (Simon Bromberg's Thesis)

## Import Required Libraries

#local functions

processSimonMetaData <- function(importedData)
{
  #nothing to process ? - just get first sheet
  processedData <- importedData[[1]]
  #Make sure data frame names are compatible with R
  names(processedData) <- make.names(names(processedData),
                                     unique=FALSE)
  return(processedData)
}

