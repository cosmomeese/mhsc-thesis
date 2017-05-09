### Main Function to excecute workflow on Simon's Cardiopulmonary Study Data


## Import Required Libraries
library(R.utils)

## Import Required Functions

sourceDir <- "userDefinedFcns"
fcns <- list("importSimonCPSData",
             "processSimonCPSData",
             "analyzeSimonCPSData",
             "displaySimonCPSData")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
				sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
					sourceDir=sourceDir)) #use function
rm(fcns,srcCreateFcn) #remove the extra unneeded variables

## Do Science!

# Import
data = importSimonCPSData()

# Process
p_Data = processSimonCPSData(data)

# Analyze
a_Data = analyzeSimonCPSData(p_Data)

# Display
displaySimonCPSData(p_data,a_data)