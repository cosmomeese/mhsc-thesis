### Main Function to excecute workflow on Simon's Thesis Data

## Instructions (assuming ease of use and that you want to use force.ChooseDialog and not default file paths):
# create & set variable: force.ChooseDialog <- TRUE
# Run this function & select the raw data files (step data files 1 & 2, cps data and participant info)

#set these appropriate and optionally run them
simonData.Importer.CodeVersion <- "1.2"
#simonExtraMetrics.CodeVersion <-  moved to calculate function


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
fcns <- list("importExcelSheets",
             "importSimonMetaData",
             "importSimonStepData",
             "importSimonCPSData",
             "processSimonMetaData",
             "processSimonStepData",
             "processSimonCPSData",
             "mergeSimonData",
             "calculateSimonDataMetrics",
             "unnestStepDataFrame",
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
rm(srcCreateFcn) #remove the extra unneeded variables

#------------------------------------------------------
if(exists("force.ChooseDialog") && !is.null(force.ChooseDialog) && (TRUE == force.ChooseDialog))
{
  force.ChooseDialog = TRUE
} else { #why R must you force your own coding style on me. >:(
  
  validEntry <- FALSE
  overwriteWarnMsg <- paste0("Use default file? [Y], enter [N] to manually specify, [Q] to quit: \n")
  keyEntry <- ''
  while(!validEntry)
  {
    invisible(keyEntry <- readline(prompt=overwriteWarnMsg))
    if(toupper(keyEntry) == 'Y')
    {
      force.ChooseDialog = FALSE
      validEntry <- TRUE
    }
    else if(toupper(keyEntry) == 'N')
    {
      force.ChooseDialog = TRUE
      validEntry <- TRUE
    }
    else if(toupper(keyEntry) == 'Q')
    {
      validEntry <- TRUE
      rm(validEntry,overwriteWarnMsg,keyEntry)
      stop('User Requested Stop')
    }
  }
  rm(validEntry,overwriteWarnMsg,keyEntry)
  
}
  

#------------------------------------------------------
## Do Science!

# Import
stepData <- importSimonStepData(USE.CHOOSEDIALOG=force.ChooseDialog)
cpsData <- importSimonCPSData(USE.CHOOSEDIALOG=force.ChooseDialog)
metaData <- importSimonMetaData(USE.CHOOSEDIALOG=force.ChooseDialog)


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

print.me <- "Combined Data from Simon Bromberg's Thesis; CPS, Step & Participant Data. \n Data is organized into an R data frame (using the tidyr library nest() function to collapse the Step & corresponding Date & Time data to a single value in the overall data frame (i.e. [DataFrame ~> StepData ~> Data, {Intraday ~> Time, Steps}]. \n Use unnestStepDataFrame to unnest frame (warning: despite being easier to use for certain data analysis unnested frame takes up a lot more memory spare)."

# Calculate Step Metric
m_cData <- calculateSimonDataMetrics(p_combinedData)
un_Data <- unnestStepDataFrame(m_cData)

if('1.0' == simonExtraMetrics.CodeVersion)
{
  # Analyze
  a_Data <- analyzeSimonData(m_cData)
  
  # Display
  displaySimonData(m_cData,a_Data)
}


# Remove functions

rm(addByTimeUnitColumn,
   calculateBasicStatisticalMetrics,
   calculateByTimeUnitVar,
   calculateDailyActiveMinutes,
   calculateFitbitDataMetrics,
   calculateOverallSummaries,
   calculateSeriesHRMetrics,
   calculateSeriesStepMetrics,
   convertUnWornValuesToNA,
   isSeriesNotEmpty,
   nestDataFrame,
   nestStepDataFrame,
   summarizeToTimeUnit,
   unnestDataFrame)

rm(list=unlist(fcns)) #remove functions
rm(fcns)