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
  TABLEROW_CSV <- "D:\\GoogleDrive\\Business\\Work\\University\\Degree - Masters\\3a - Thesis\\Other\\Follow-up on Raghad's Study\\analyzeSimonData_raghadFollowupPaperAnalytics_TableRowDictionary.csv"
  CPETONLYKEY <- "CPETOnly"
  #TEST_MATRIX <- expand.grid(featuresVersion = c(CPETONLYKEY,"StepMetrics","CPET+Step","PaperMetrics","withSpecFeat"),
  #                           isImputeMissing = c(TRUE, FALSE),
  #                           preselectFeatures = c(TRUE, FALSE),
  #                           isForceLOOCV = c(TRUE,FALSE),
  #                           kFolds = c(Inf, 10))
  TEST_MATRIX <- expand.grid(featuresVersion = c("StepMetrics","CPET+Step"),
                             isImputeMissing = c(TRUE, FALSE),
                             preselectFeatures = c(TRUE, FALSE),
                             isForceLOOCV = c(TRUE,FALSE),
                             kFolds = c(Inf, 10))
  #MODEL_VERSION <- "1.0"
  MODEL_VERSION <- "1.1" # added isFORCE_LOOCV_TRAIN to META_PARAMS being saved. N.B. models are unchanged
  OUTCOME_VAR <- "NYHAClass" 
  IGNORE_VARS <- c("StudyIdentifier")
  
  getTableRowDict <- function()
  {
    #from analyzeSimonData_raghadFollowupPaperAnalytics.R
    # Table Row for Histogram
    # combine with getwd() + evalute the ".."'s
    dirPath <- "../../Other/Follow-up on Raghad's Study"
    dirPath <- normalizePath(file.path(getwd(),dirPath), winslash="/")
    fileNamePrefix <- "analyzeSimonData_raghadFollowupPaperAnalytics_"
    tableRowDictFileName <- "TableRowDictionary.csv"
    # assemble final full path
    tableDictFullPath <- file.path(dirPath,paste0(fileNamePrefix,
                                                  tableRowDictFileName))
    tableDict <- read_csv(tableDictFullPath, 
                          col_types = cols(OutputBase = col_skip(), 
                                           OutputPrefix = col_skip()))
    return(tableDict)
  }
  tableRowDict <- getTableRowDict()
  
  failedAtLeastOnce <- FALSE
  for(rIdx in 1:nrow(TEST_MATRIX))
  {
    # Note row is passed as an (unnamed) vector so have to use indices to extract
    FEATURES_VERSION <- as.character(TEST_MATRIX[[rIdx,'featuresVersion']])
    isKEEP_ONLY_CPET <- CPETONLYKEY == FEATURES_VERSION
    isIMPUTEMISSING_nDROP <- TEST_MATRIX[[rIdx,'isImputeMissing']]
    isPRESELECT_FEATURES <- TEST_MATRIX[[rIdx,'preselectFeatures']]
    isFORCE_LOOCV_TRAIN <- TEST_MATRIX[[rIdx,'isForceLOOCV']]
    kFOLDS <- TEST_MATRIX[[rIdx,'kFolds']] # defaults to length of folds if NULL

    
    #### GENERATE FULL GRAPH TITLE ####
    
    MODEL_NAME_PLACEHOLDER_CHAR <- "$"
    fullGraphTitlePrefix <- glue("{FEATURES_VERSION} {MODEL_NAME_PLACEHOLDER_CHAR} ")
    fullSaveFilePrefix <- glue("{SAVEFILE_NAME}-{FEATURES_VERSION}")
    #if(isKEEP_ONLY_CPET)
    #{
    #  fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} CPET Only")
    #  fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-CPET Only")
    #}
    
    if(!is.null(kFOLDS))
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} (k={kFOLDS})")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-k{kFOLDS}")
    }
    
    if(isFORCE_LOOCV_TRAIN)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} LOOCV-Trained")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-LOOCVTrained")
    }
    
    if(isIMPUTEMISSING_nDROP)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} Imputed")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-wImputation")
    }
    if(isPRESELECT_FEATURES)
    {
      fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} Only Best Features")
      fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-wTopFeatures")
    }
    
    # metrics code version
    fullGraphTitlePrefix <- glue("{fullGraphTitlePrefix} v{simonExtraMetrics.CodeVersion}Features")
    fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-Fv{simonExtraMetrics.CodeVersion}")
    
    fullSaveFilePrefix <- glue("{fullSaveFilePrefix}-Mv{MODEL_VERSION}")
    
    
    #### ANALYSIS ####
    
    mlModelResults <- generateMLModels(m_cData,
                                       extraMetricsVersion=simonExtraMetrics.CodeVersion,
                                       FEATURES=FEATURES_VERSION,
                                       isIMPUTEMISSING_nDROP=isIMPUTEMISSING_nDROP,
                                       isPRESELECT_FEATURES=isIMPUTEMISSING_nDROP,
                                       isKEEP_ONLY_CPET=isKEEP_ONLY_CPET,
                                       isFORCE_LOOCV_TRAIN=isFORCE_LOOCV_TRAIN,
                                       kFolds=kFOLDS)
    if(!is.null(mlModelResults))
    {
      # generate ROC for the ML models generated in generateMLModels. 
      # testResults should be the testResults returned from generateMLModels
      # outcomeVar should be the name (as string) of the outcome var in the test results list
      # ignoreVar should be a vector (of strings) of names in test result ignore/for which to not create ROC for
      rocResults <- makeROCForMLTestResults(mlModelResults$testResults,
                                            outcomeVar=OUTCOME_VAR,
                                            ignoreVar=IGNORE_VARS,
                                            graphTitlePrefix=fullGraphTitlePrefix,
                                            modelNamePlaceholderString=MODEL_NAME_PLACEHOLDER_CHAR,
                                            saveGraphs=TRUE)
      
      
      
      # generate histogram for the ML Models predictor
      varImportance <- makePredictorHistogram(mlModelResults$finalModels,
                                              graphTitlePrefix=fullGraphTitlePrefix,
                                              modelNamePlaceholderString=MODEL_NAME_PLACEHOLDER_CHAR,
                                              tableRowDictionary=tableRowDict,
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
                          isFORCE_LOOCV_TRAIN=isFORCE_LOOCV_TRAIN,
                          kFOLDS=kFOLDS,
                          OUTCOME_VAR=OUTCOME_VAR,
                          IGNORE_VARS=IGNORE_VARS)
      
      # perform actual save
      save(META_PARAMS,
           MODEL_NAME_PLACEHOLDER_CHAR,
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
    else
    {
      msg <- paste0('\nNull model returned at rIdx', rIdx, '\n')
      cat(msg)
      warning(msg)
      if(!failedAtLeastOnce)
      {
        failedAtLeastOnce <- TRUE
        failedIdx <- rIdx
      }
      else
      {
        failedIdx <- c(failedIdx,rIdx)
      }
      
    }
    
    
    
  }
  
  print('!!!finished analysis!!!')
  
}

collate <- function()
{
  source('userDefinedFcns/thesis_modelCollator.R')
}

collate()

# END ------------------------------------------------------- 