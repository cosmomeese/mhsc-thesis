### Function to calculate Demographics + Statistics for Paper Summarizing Simon Bromberg's Thesis (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true
SAVE_CSVS <- TRUE

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue","leaps","pROC","caret","RANN","randomForest")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(glue)
library(leaps)
library(pROC)
library(caret)
library(RANN)
library(randomForest)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("analyzeData_common",
             "analyzeSimonData_common",
             "unnestStepDataFrame")

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
  
} else {
  
  preProcessHelper <- function(dataForModelSearch, ignoreColumns, isIMPUTEMISSING_nDROP)
  {
    data.preProc <- dataForModelSearch
    addtlPreProcessMethods <- NULL
    if(isIMPUTEMISSING_nDROP)
    { # use preprocess to impute missing data
      addtlPreProcessMethods <- "knnImpute"
      
    } else 
    { # drop missing cases
      # drop incomplete cases in this subsetted dataset (since regsubsets can't deal with NAs)
      data.preProc <- dataForModelSearch[complete.cases(dataForModelSearch),]
    }
    
    #### Apply pre-processor to scale, center & impute (as specified) ####
    allNotOutcomeAndIDIdx <- which(names(data.preProc) %in% ignoreColumns)
    
    # hide away the row names so we can reconstruct after applying preProcessor
    #row.names(data.preProc) <- data.preProc[[studyIDColName]]
    
    # create the pre-processor
    preProcessorMethods <- c(addtlPreProcessMethods, 
                             "center", 
                             "scale")
    
    preProcessor <- caret::preProcess(data.preProc[, -allNotOutcomeAndIDIdx],
                                      method=preProcessorMethods)
    # apply the pre-processor
    data.NoMiss <- predict(preProcessor,
                           newdata=data.preProc[, -allNotOutcomeAndIDIdx])
    
    # return studyID and response var column
    keptIndices <- which(row.names(data.preProc) %in% row.names(data.NoMiss))
    
    for(columnName in ignoreColumns)
    {
      data.NoMiss[columnName] <- data.preProc[keptIndices,columnName]
    }
    
    return(data.NoMiss)
  }
  
  fullData <- m_cData
  
  # Prep ==================================
  
  #### Add BMI ##########################################
  # BMI = weight[kg] / (height[m]^2)
  fullData <- addBMIColumn(fullData)

  
  #### Clean up factor levels ###########################
  
  fullData <- addMissingClassGroupings(fullData)
  
  #### Tag fullData as immutable
  FULL_DATA <- fullData
  rm(fullData)
  
  # Create FULL_DATA.ForViewing for easier viewing of data using: View(FULL_DATA.ForViewing)
  FULL_DATA.ForViewing <- createViewableFullData(FULL_DATA);
  
  # For debugging
  # FULL_DATA.NYHAOnly <- FULL_DATA[,c('StudyIdentifier','NYHAClassMixed','NYHAClass','PureNYHAClass','ExplicitNYHAClass')]
  
  # For distribution of participants by Sex in each class
  FULL_DATA.bySex <- FULL_DATA[,c('StudyIdentifier','Sex','ExplicitNYHAClass')]
  FULL_DATA.bySex <- FULL_DATA.bySex %>% arrange(ExplicitNYHAClass, Sex)
  
  # List of 'valid' studyID's that are not NAs
  studyIDs.NoNas <- FULL_DATA %>% 
    dplyr::filter(!is.na(NYHAClass)) %>%
    .$StudyIdentifier
  
  #### Machine Learning #################################
  
  #melt so they're all together (also use ForViewing so that we don't melt the raw step data)
  #melted <- reshape2::melt(FULL_DATA.ForViewing, id.vars=c("StudyIdentifier", 
  #                                                         "NYHAClass",
  #                                                         "PureNYHAClass",
  #                                                      "ExplicitNYHAClass"))
  
  # Meta Parameters ===============================
  K_FOLDS <- length(studyIDs.NoNas)
  N_REPEATS <- 1
  N_FEATURES_MAX <- 10
  N_BEST_FEATURES <- 10
  LEAPS_SEARCH_METHOD <- "seqrep" #N.B. do not do exhaustive w/ N_FEATURES_MAX > 5 it takes too long
  FOLD_SEED <- 123
  TRAIN_SEED <- 11111111
  RAND_INT_MAX <- 10000
  isIMPUTEMISSING_nDROP <- TRUE
  isPRESELECT_FEATURES <- FALSE
  
  # Generate & train some models
  METHODS <- c("rf", # kappa
               "nnet", # kappa around -0.04-0.633 beat by pcaNNet
               #"ada", # kappa around 0.4-0.69, most around 0.51
               #"adaboost", # kappa around 0.25-0.47
               "pcaNNet", # kappa around 0.33-0.79
               "glm", # kappa = 0.6790698
               "glmboost") # kappa = 0.738041
  FITTING_PARS <- setNames(c(
                            3, # rf
                            9, # nnet
                            #1, # ada
                            #1, # adaboost
                            9, # pcaNNet
                            1, # glm
                            1  # glmboost
                            ), 
                           METHODS)
  
  # Create Folds=========================================
  
  set.seed(FOLD_SEED)
  folds <- caret::groupKFold(studyIDs.NoNas,
                             k=K_FOLDS)
  
  
  #### Best Parameter Search ############################
  
  ### Variables =========================
  # desired response variable to test for
  responseVar <- "NYHAClass"
  
  # other variables that need to be excluded for whatever reason
  # v1.0
  excludeVars  = c("NYHAClassMixed",  # dirty outcome variable
                   "PureNYHAClass",  # alternate outcome variable
                   "ExplicitNYHAClass",  # alternate outcome variable
                   "WristbandPreference",
                   "Handedness",
                   "EjectionFraction",  # too many missing
                   "HFDiagnosisYear",  # dirty
                   "HFTreatmentsToDate",  # dirty/text
                   "RegularPhysicalActivities",  # dirty/text
                   "DeviceID",  # not relevant
                   "ID",  # not relevant
                   "CPSDate",  # not relevant?
                   "TestEnd.Reason",  # dirty/text
                   "TestEnd.Symptom",  # dirty/text
                   "RPEper20.Peak",  # too many missing
                   "PETCO2.Peak",  # too many missing
                   "OUES",  # too many missing
                   "TotalRiskScore",  # too many missing
                   "StepData.MeanDailyMeanSteps", # = constant*StepData.MeanDailyMeanSteps
                   "StepData.StdDevDailyMeanSteps", # = constant*StepData.StdDevDailyMeanSteps
                   "StepData.ModeDailyModeSteps", # all zero
                   "StepData.MeanDailyMinSteps", # all zero
                   "StepData.ModeDailyMinSteps", # all zero
                   "StepData" # this is the minute by minute stuff
                   )
  # v1.1
  if(as.numeric(simonExtraMetrics.CodeVersion) > 1.0)
  {
    excludeVars = c(excludeVars, # v1.0 excludeVars
                    "StepData.TotalMETClass.BelowMin", # = constant*StepData.MeanMETClass.BelowMin
                    "StepData.TotalMETClassIV", # = constant*StepData.MeanMETClassIV
                    "StepData.TotalMETClassIII", # = constant*StepData.MeanMETClassIII
                    "StepData.TotalMETClassII", # = constant*StepData.MeanMETClassII
                    "StepData.TotalMETClassI", # = constant*StepData.MeanMETClassI + (effectively) all zero
                    "StepData.MeanMETClassI", # (effectively) all zero
                    "StepData.StdDevMETClassI", # (effectively) all zero
                    "StepData.ModeMETClassI", # all zero
                    "StepData.MaxMETClassI", # (effectively) all zero
                    "StepData.OverallMETClassI.Percentage", # (effectively) all zero
                    "StepData.OverallMETClassI.Percentage", # (effectively) all zero
                    "StepData.ModeMETClassII" # all zero
                    #"StepData.OverallMETClassI.PercentageAll"  # (effectively) all zero (has zero variance in some folds)
                    )
  }

                  
  
  ### Common instructions that don't need to be re-calculated every loop=========================
  # subset the FULL_DATA set to only deal with response and sig variables
  studyIDColName <- "StudyIdentifier"
  dataForModelSearch <- FULL_DATA[ , -which(names(FULL_DATA) %in% excludeVars)]
  
  # convert all factors to numeric (since regsubsets can't handle strings which is what as.matrix does)
  # copied from https://stackoverflow.com/a/27528953
  indx <- sapply(dataForModelSearch, is.factor) # find index of factor
  indx <- indx | sapply(dataForModelSearch, is.logical) # find index of logicals
  indx[which(names(dataForModelSearch) %in% c(responseVar, studyIDColName))] <- FALSE # force response var & studyID to false because we want to keep as factors,
  dataForModelSearch[indx] <- lapply(dataForModelSearch[indx],
                                               function(x) as.integer(x))
  
  
  ### Handle missing data ###############################################################
  
  # apply our imputation, center/scaling method
  ignoreColumns <- c(responseVar,studyIDColName)
  dataForTest.NoMiss <- preProcessHelper(dataForModelSearch, ignoreColumns, isIMPUTEMISSING_nDROP)

  ### Start the fun stuff ###############################################################
  trainResults <- list()
  # take all response results from original data (dataForModelSearch will work)
  testResults <- dataForModelSearch[studyIDColName]
  temp <- dataForModelSearch[[responseVar]]
  # results will be factors (1,2), convert to numeric and adjust range to (0,1)
  # this will give 0 = II, 1 = III which is opposite of model, so change to
  # logical and negate giving TRUE = II, FALSE = III and convert back to numeric
  # to get 1 = II, 0 = III then save as data.frame
  temp <- as.numeric(!as.logical(as.numeric(temp)-1))
  testResults[responseVar] <- temp
  rm(temp)
  for(mtd in METHODS)
  {
    print(glue("Starting on {mtd}"))
    foldResults <- list() # model results
    testResults[mtd] <- NA_real_ # test results for method
    for(foldName in names(folds))
    {
      print(glue("    {foldName}"))
      # for debugging fix fold
      fold <- folds[[foldName]]
      
      # get the data we need for this fold
      dataForFold <- subset(dataForModelSearch, 
                            StudyIdentifier %in% studyIDs.NoNas[fold])
    
      print(glue("        preprocessing..."))
      # N.B. do this first since all functions that follow are sensitive to missing values
      dataForFold.NoMiss <- preProcessHelper(dataForFold, ignoreColumns, isIMPUTEMISSING_nDROP) 
      
      
      # get the column IDs
      responseColNum.NoMiss <- which(names(dataForFold.NoMiss) %in% c(responseVar))
      studyIDColNum.NoMiss <- which(names(dataForFold.NoMiss) %in% c(studyIDColName))
      
      ### Find the Best Features ############################################################
      if(isPRESELECT_FEATURES)
      {
        ### REVISIT THIS -> I THINK I NEED TO USE A SUBSET OF DATA FOR FEATURE SEARCH AGAIN
        modelSearch <- leaps::regsubsets(as.matrix(dataForFold.NoMiss[,-c(responseColNum.NoMiss,studyIDColNum.NoMiss)]),
                                         dataForFold.NoMiss[,responseColNum.NoMiss],
                                         nbest = N_BEST_FEATURES,
                                         nvmax = N_FEATURES_MAX,
                                         really.big = TRUE,
                                         method = LEAPS_SEARCH_METHOD)
        # which model is best?
        modelSearch.summary <- summary(modelSearch)
        bestModelIdx <- which.min(modelSearch.summary$bic) # BIC which is like AIC but penalizes complexity differently
        #bestModelIdx <- which.min(modelSearch.summary$cp) # Mallow's CP -> AIC
        bestModelCols <- modelSearch.summary$which[bestModelIdx,]
        bestModelVars <- names(bestModelCols[bestModelCols == TRUE][-1]) # get names of all true, dropping first item which is (Intercept)

        form <- as.formula(glue("{responseVar} ~ {paste(bestModelVars,collapse=' + ')}"))
      }
      else
      {
        form <- as.formula(glue("{responseVar} ~."))
      }
      
      # create trainingControl seeds (from example)
      # w/ help from: https://stackoverflow.com/a/21988897
      print(glue("        setting random seed..."))
      set.seed(TRAIN_SEED)
      trCtrlSeeds <- vector(mode = "list", 
                            length = (N_REPEATS * nrow(dataForFold.NoMiss)) + 1)
      numFittingParameters <- FITTING_PARS[[mtd]] # this needs to at least be equal to number of tunning parameters 

      for(i in 1:(length(trCtrlSeeds)-1)) trCtrlSeeds[[i]] <- sample.int(RAND_INT_MAX, numFittingParameters)
      ## For the final model (just need one single integer vs list):
      trCtrlSeeds[[length(trCtrlSeeds)]] <- sample.int(RAND_INT_MAX, 1)
      
      # set trainingControl
      trainingControl <- trainControl(method = "LOOCV", # leave one out cross validation
                                      classProbs = TRUE,  # save output probs for AUC
                                      repeats = N_REPEATS,  # repeat train/cv
                                      seeds = trCtrlSeeds)  # random seeds
      finalFoldTrainData <- dataForFold.NoMiss[,-c(studyIDColNum.NoMiss)] # make sure to drop studyIdentifier
      print(glue("        training on Fold..."))
      foldResults[[foldName]] <- train(form,
                                       method = mtd,
                                       data = finalFoldTrainData, # make sure to drop studyIdentifier,
                                       trControl = trainingControl)
      
      
      # test
      print(glue("        preparing test data..."))
      # get test data for this fold
      testDataIndxNames <- setdiff(studyIDs.NoNas, studyIDs.NoNas[fold])
      
      testDataForFold.NoMiss <- dataForTest.NoMiss[dataForTest.NoMiss[[studyIDColName]] %in% testDataIndxNames,]
      
      if(nrow(testDataForFold.NoMiss) > 0)
      {
        testResponseColNum.NoMiss <- which(names(testDataForFold.NoMiss) %in% c(responseVar))
        testStudyIDColNum.NoMiss <- which(names(testDataForFold.NoMiss) %in% c(studyIDColName))
        
        cat(glue("        testing..."))
        for(indxName in testDataIndxNames)
        {
          cat(glue(" {indxName}..."))
          finalFoldTestData <- testDataForFold.NoMiss[,-c(testStudyIDColNum.NoMiss)]
          predictProb <- predict(foldResults[[foldName]],
                                 newdata=finalFoldTestData,
                                 type="prob"
                                 )
          # exact the (first) probability for the first factor level of the response variable
          tempResult <- predictProb[1,levels(finalFoldTestData[[responseVar]])[[1]]] # get the 
          testResults[indxName == testResults[studyIDColName],mtd] <- tempResult
          rm(predictProb, tempResult)
        }
        cat("\n")  # new line
        print(glue("        done"))
      }
      else
      {
        print(glue("        skipped testing..."))
      }
      
    }

    print(glue("Ended {mtd}"))
  }
  
  #### Model Performance Evaluation #####################
  
  # Test our models
  
  # Show summary statistics
  #print(summary(glmModels[[1]]))
  
  # keep only complete cases where we can compate
  testResults.NoNAs <- testResults[complete.cases(testResults),]
  
  # threshold the probabilities
  testResults.Thresholded <- testResults.NoNAs
  studyIDColNum <- which(studyIDColName %in% names(testResults.Thresholded))
  testResults.Thresholded[,-studyIDColNum] <- round(testResults.NoNAs[, -studyIDColNum],0)
  
  testResults.Thresholded[-studyIDColNum] <- lapply(testResults.Thresholded[-studyIDColNum],
                                                    function(x) factor(x,
                                                                       labels=levels(finalFoldTestData[[responseVar]])))
  
  trueValues <- testResults.Thresholded[[responseVar]]
  
  for(mtd in METHODS)
  {
    
    cat(glue("{mtd} test results: ----------------------------
             "))
    cat("\n")
    
    # Calculate ROC
    #predictions<-predict(trainResults[[mtd]],
    #                     testData)
    #type=c("response")
    
    #g <- roc(NYHAClass ~ predictions, data = data.frame(testData,
    #                                                    predictions))
    #plot(g)    
    
    cat("\n")
    #print(auc(g))
    cat("\n")
    
    predictions <- testResults.Thresholded[[mtd]]
    
    conf <- confusionMatrix(data=predictions,
                            reference=trueValues,
                            dnn= c("AI","Physician"))
    print(conf)
    cat(glue(""))
    
  }
  
}

# END ------------------------------------------------------- 