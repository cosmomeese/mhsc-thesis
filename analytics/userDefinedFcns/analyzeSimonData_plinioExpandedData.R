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
  
  #### META-PARAMETERS ####
  
  GRAPH_TITLE_PREFIX <- ""
  SAVEFILE_NAME <- "H:\\MLModels\\MLModel-Metrics"
  TEST_MATRIX <- expand.grid(featuresVersion = c("1.0","1.1"),
                             keepCPETOnly = c(TRUE, FALSE),
                             isImputeMissing = c(TRUE, FALSE),
                             preselectFeatures = c(TRUE, FALSE),
                             kFolds = c(5, Inf))
  MODEL_VERSION <- "1.0"
  OUTCOME_VAR <- "NYHAClass" 
  IGNORE_VARS <- c("StudyIdentifier")
  
  for(rIdx in 1:nrow(TEST_MATRIX))
  {
    
    # Note row is passed as an (unnamed) vector so have to use indices to extract
    FEATURES_VERSION <- as.character(TEST_MATRIX[[rIdx,'featuresVersion']])
    isKEEP_ONLY_CPET <- TEST_MATRIX[[rIdx,'keepCPETOnly']]
    isIMPUTEMISSING_nDROP <- TEST_MATRIX[[rIdx,'isImputeMissing']]
    isPRESELECT_FEATURES <- TEST_MATRIX[[rIdx,'preselectFeatures']]
    kFOLDS <- TEST_MATRIX[[rIdx,'kFolds']] # defaults to length of folds if NULL

    
    #### GENERATE FULL GRAPH TITLE ####
    
    fullGraphTitlePrefix <- GRAPH_TITLE_PREFIX
    fullSaveFilePrefix <- glue("{SAVEFILE_NAME}-Mv{MODEL_VERSION}")
    if(isKEEP_ONLY_CPET)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} CPET Only")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-CPET Only")
    }
    if(isIMPUTEMISSING_nDROP)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} with Imputation")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-wImputation")
    }
    if(isPRESELECT_FEATURES)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} Top Features Preselected")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-wTopFeatures")
    }
    
    # metrics code version
    fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} v{FEATURES_VERSION}Features")
    fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-Fv{FEATURES_VERSION}")
    
    if(!is.null(kFOLDS))
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} (k={kFOLDS})")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-k{kFOLDS}")
    }
    
    #### ANALYSIS ####
    
    mlModelResults <- generateMLModels(m_cData,
                                       simonExtraMetrics.CodeVersion=FEATURES_VERSION,
                                       isIMPUTEMISSING_nDROP=isIMPUTEMISSING_nDROP,
                                       isPRESELECT_FEATURES=isIMPUTEMISSING_nDROP,
                                       isKEEP_ONLY_CPET=isKEEP_ONLY_CPET,
                                       kFolds=kFOLDS)
    
    
    # generate ROC for the ML models generated in generateMLModels. 
    # testResults should be the testResults returned from generateMLModels
    # outcomeVar should be the name (as string) of the outcome var in the test results list
    # ignoreVar should be a vector (of strings) of names in test result ignore/for which to not create ROC for
    rocResults <- makeROCForMLTestResults(mlModelResults$testResults,
                                          outcomeVar=OUTCOME_VAR,
                                          ignoreVar=IGNORE_VARS,
                                          graphTitlePrefix=fullGraphTitlePrefix,
                                          saveGraphs=TRUE)
    
    
    
    # generate histogram for the ML Models predictor
    varImportance <- makePredictorHistogram(mlModelResults$finalModels,
                                            graphTitlePrefix=fullGraphTitlePrefix,
                                            saveGraphs=TRUE)
    
    # generate confusion matrix for the ML models generated in generateMLModels. 
    # testResults should be the testResults returned from generateMLModels
    confMat <- confusionMatrixForMLTestResults(mlModelResults$testResults)
    
    # save results
      # collate the meta parameters together
    META_PARAMS <- list(MODEL_VERSION=MODEL_VERSION,
                        FEATURES_VERSION=FEATURES_VERSION,
                        isKEEP_ONLY_CPET=isKEEP_ONLY_CPET,
                        isIMPUTEMISSING_nDROP=isIMPUTEMISSING_nDROP,
                        isPRESELECT_FEATURES=isPRESELECT_FEATURES,
                        kFOLDS=kFOLDS,
                        OUTCOME_VAR=OUTCOME_VAR,
                        IGNORE_VARS=IGNORE_VARS)
      # perform actual save
    save(META_PARAMS,
         fullGraphTitlePrefix,
         fullSaveFilePrefix,
         mlModelResults,
         rocResults,
         varImportance,
         confMat,
         file = glue("{fullSaveFilePrefix}.RData"))
      # clear for next run
    rm(META_PARAMS)
    
  }
  
  print('!!!finished analysis!!!')
  
}

# END ------------------------------------------------------- 