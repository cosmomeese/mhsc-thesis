### Calculate Demographics + Statistics for Paper Summarizing Simon Bromberg's Thesis (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(glue)


## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("createMLModels")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

# START -------------------------------------------------------

# Prep ================================================

#### Add Pure NYHA Class ##############################

if(!exists("m_cData"))
{
  stop("Missing m_cData. Couldn't execute. Load .Rdata file w/ m_cData and retry. \nData File 'ConvertedRawDataForHMM-v0.3-fromSource-v1.2.RData' will do.")
  
} else if(!exists("simonExtraMetrics.CodeVersion")) {
  stop("Missing simonExtraMetrics.CodeVersion Couldn't execute. Load .Rdata file w/ simonExtraMetrics.CodeVersion and retry. \nData File 'ConvertedRawDataForHMM-v0.3-fromSource-v1.2.RData' will do.")
} else {
  
  #simonExtraMetrics.CodeVersion <- "1.1"
  isKEEP_ONLY_CPET <- TRUE
  isIMPUTEMISSING_nDROP <- FALSE
  isPRESELECT_FEATURES <- FALSE
  graphTitlePrefix <- "CPET Only"
  
  mlModelResults <- generateMLModels(m_cData,
                                     simonExtraMetrics.CodeVersion,
                                     isIMPUTEMISSING_nDROP=isIMPUTEMISSING_nDROP,
                                     isPRESELECT_FEATURES=isIMPUTEMISSING_nDROP,
                                     isKEEP_ONLY_CPET=isKEEP_ONLY_CPET
                                     )
  
  
  
  # generate confusion matrix for the ML models generated in generateMLModels. 
  # testResults should be the testResults returned from generateMLModels
  confusionMatrixForMLTestResults(mlModelResults$testResults)
  
  
  # generate ROC for the ML models generated in generateMLModels. 
  # testResults should be the testResults returned from generateMLModels
  # outcomeVar should be the name (as string) of the outcome var in the test results list
  # ignoreVar should be a vector (of strings) of names in test result ignore/for which to not create ROC for
  rocResults <- makeROCForMLTestResults(mlModelResults$testResults,
                                        outcomeVar="NYHAClass",
                                        ignoreVar=c("StudyIdentifier"),
                                        graphTitlePrefix=graphTitlePrefix,
                                        saveGraphs=TRUE)
  
  
  
  # generate histogram for the ML Models predictor
  varImportance <- makePredictorHistogram(mlModelResults$finalModels,
                                          graphTitlePrefix=graphTitlePrefix,
                                          saveGraphs=TRUE)
  
  
}

# END ------------------------------------------------------- 