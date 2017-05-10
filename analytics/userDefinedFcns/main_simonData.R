### Main Function to excecute workflow on Simon's Thesis Data

#------------------------------------------------------
## Import Required Libraries
# Install if it's not installed on this computer
pkg <- c("R.utils")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(R.utils)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("importSimonMetaData",
             "importSimonStepData",
             "importSimonCPSData",
             "processSimonMetaData",
             "processSimonStepData",
             "processSimonCPSData",
             "mergeSimonData",
             "analyzeSimonData",
             "displaySimonData")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
				sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
					sourceDir=sourceDir)) #use function
rm(fcns,srcCreateFcn) #remove the extra unneeded variables

#------------------------------------------------------
## Do Science!

# Import
stepData <- importSimonStepData()
cpsData <- importSimonCPSData()
metaData <- importSimonMetaData()


# Process
p_metaData <- processSimonMetaData(metaData)
p_stepData <- processSimonStepData(stepData)
p_cpsData <- processSimonCPSData(cpsData)
  # Clean up some space in memory
rm(metaData,stepData,cpsData) 
  # Combine
p_combinedData <- mergeSimonData(p_metaData,p_stepData,p_cpsData)
  # Clean up some more space in memory
rm(p_metaData,p_stepData,p_cpsData)

# Analyze
#a_Data <- analyzeSimonData(p_combinedData)

# Display
#displaySimonData(p_combinedData,a_data)