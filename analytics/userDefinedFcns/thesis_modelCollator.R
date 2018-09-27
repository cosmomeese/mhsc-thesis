### Collate ML Model Metrics

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
fcns <- list("analyzeData_common",
             "analyzeSimonData_common")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
#rm(srcCreateFcn) #remove the extra unneeded variables


#get models
# from https://www.r-bloggers.com/looping-through-files/

cat('\nStarting Model Collation!\n')

SAVEFILE_PATH <- "H:/MLModels/"

outFileName <-"MLModelSummary"

fileModelPattern <- "MLModel-Metrics.*RData"
file.names <- dir(SAVEFILE_PATH, pattern=fileModelPattern)
file.paths <- paste0(SAVEFILE_PATH,file.names)

firstRow <- TRUE
for(i in seq(file.paths))#i in seq_along(file.paths))
{

  load(file.paths[[i]])
  
  # in v1.0 unfortunately isFORCE_LOOCV_TRAIN was not saved to META_PARAMS.
  # Fortunately, we can infer this from the file name
  if("1.0" == META_PARAMS$MODEL_VERSION)
  {
    META_PARAMS$isFORCE_LOOCV_TRAIN <- FALSE
    if(grepl("LOOCVTrained",file.paths[[i]])) # contains missing parameter
    {
      META_PARAMS$isFORCE_LOOCV_TRAIN <- TRUE
    }
  }

  for(mtd in names(confMat))
  {
    metricsVec <- c()
    # rest of metrics
    colNames_in_which_metrics_are_stored <- c('overall','byClass')
    for(colName in colNames_in_which_metrics_are_stored)
    {
      metricsVec <- c(metricsVec,confMat[[mtd]][[colName]])
    }
    
    # confusion matrix (flattened)
    confTableFlattened <- as.numeric(confMat[[mtd]][['table']])
    names(confTableFlattened) <- c('TruePositiveCount','FalseNegativesCount','FalsePositiveCount','TrueNegativeCount')
    metricsVec <- c(metricsVec,confTableFlattened)
    
    # add roc auc
    auc <- as.numeric(rocResults[[mtd]][['auc']])
    names(auc) <- "AUC"
    metricsVec <- c(metricsVec,auc)
    
    namedMethodElement <- list(Method=mtd)
    completeMetricList <- c(namedMethodElement,META_PARAMS,metricsVec)
    
    metricDFRow <- data.frame(t(unlist(completeMetricList)),stringsAsFactors = FALSE)
    
    if(firstRow)
    {
      metricDF <- metricDFRow
      firstRow <- FALSE
    }
    else
    {
      metricDF <- rbind(metricDF,metricDFRow)
    }
  }
}

# generate bias & variance

idVars <- c("Method",
            "MODEL_VERSION",
            "FEATURES_VERSION",
            "isKEEP_ONLY_CPET",
            "isIMPUTEMISSING_nDROP",
            "isPRESELECT_FEATURES",
            #"kFOLDS", exactly not this one. Because we want to do calcs on them
            "OUTCOME_VAR",
            "IGNORE_VARS",
            "isFORCE_LOOCV_TRAIN")
kFoldMetric <- 'kFOLDS'
performanceMetric <- 'Kappa'
form <- formula(paste0(paste0(idVars,collapse=" + ")," ~ ", kFoldMetric))
wideMetricDF <- metricDF[,colnames(metricDF) %in% c(idVars,kFoldMetric,performanceMetric)] %>%
                  reshape2::dcast(form, id.var = idVars, value.var = performanceMetric)
# rename for easier referencing
wideMetricDF <- wideMetricDF %>% mutate(performanceBias_FromZero = as.numeric(`Inf`),
                                        performanceVariance = performanceBias_FromZero - as.numeric(`10`))
wideMetricDF$`10` <- NULL
wideMetricDF$`Inf` <- NULL

# now merge the new bias and variance metrics with the old one (N.B. there will be two copies per entry)
metricDF <- merge(metricDF,
                      wideMetricDF,
                      all.x=TRUE)


# and finally write out:
write.csv(metricDF,file=paste0(SAVEFILE_PATH,outFileName,".csv"))

cat('\nFinished Collating!\n')