### Main Function to excecute workflow on Simon's Fitbit Step Data


## Import Required Libraries
library(R.utils)

## Import Required Functions

sourceDir <- "userDefinedFcns"
fcns <- list("importSimonStepData",
             "processSimonStepData",
             "analyzeSimonStepData",
             "displaySimonStepData")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",sep=""))
}
invisible(lapply(fcns,srcCreateFcn,sourceDir=sourceDir)) #use function
rm(fcns,srcCreateFcn) #remove the extra unneeded variables

## Do Science!

# Import
data = importSimonStepData()

# Process
p_Data = processSimonStepData(data)

# Analyze
a_Data = analyzeSimonStepData(p_Data)

# Display
displaySimonStepData(p_data,a_data)