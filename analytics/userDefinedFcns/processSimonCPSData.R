### Process Function for Cardiopulmonary Study Data (Simon Bromberg's Thesis)

## Import Required Libraries

#local functions

processSimonCPSData <- function(importedData)
{
  #nothing to process ? - just get first sheet
  processedData <- importedData[[1]]
  
  ##rename some fields
  #Make sure data frame names are compatible with R
  names(processedData) <- make.names(names(processedData),
                                     unique=FALSE)
  
  #rename 'Field' column to match other imports; should be 'Study.Identifier'
  #below -v is effectively: access names of processedData where names match "Field" and for these replace the name with "Study.Identifier"
  names(processedData)[names(processedData)=="Field"] <- "Study.Identifier"
  
  return(processedData)
  
}

