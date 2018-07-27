### Functions to calculate Demographics + Statistics for Paper Summarizing Simon Bromberg's Thesis (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue","reshape2","leaps","pROC","caret","RANN","randomForest","ggthemes")
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
library(reshape2)
library(ggthemes)

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
#rm(srcCreateFcn) #remove the extra unneeded variables

# START -------------------------------------------------------

# Prep ================================================

#### Add Pure NYHA Class ##############################

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

# generate ML Models. 
# Use m_cData which is a data frame that contains StudyIDentifier, and the rest of the predictive variables
# simonExtraMetrics.CodeVersion is the the same named variable included with the m_CData save files
# Use isIMPUTEMISSING_nDROP = TRUE to impute missing data (instead of dropping)
# Use isPRESELECT_FEATURES = TRUE to try to preselect features instead of using all of them
# Use isKEEP_ONLY_CPET = TRUE to only keep CPET data
# Use kFolds to specify number of folds desired
generateMLModels <- function(m_cData,
                             simonExtraMetrics.CodeVersion,
                             isIMPUTEMISSING_nDROP=FALSE,
                             isPRESELECT_FEATURES=FALSE,
                             isKEEP_ONLY_CPET=TRUE,
                             kFolds=NULL
                             )
{
  # Meta Parameters ===============================
  
  RESULTS_VAR_NAME_TEST <- 'testResults'
  RESULTS_VAR_NAME_FINAL_MODEL <- 'finalModels'
  RESULTS_ATTR_NAME_METRICS_CODE_VERSION <- 'metricsCodeVersion'
  RESULTS_ATTR_NAME_IMPUTE <- 'isImpute'
  #RESULTS_VAR_NAME_PRE_PROCESS <- 'preProcess'
  RESULTS_ATTR_NAME_PRESELECT <- 'isPreselectFeatures'
  RESULTS_ATTR_NAME_CPET_ONLY <- 'isCPETOnly'
  RESULTS_ATTR_NAME_STUDYIDCOLNAME <- 'studyIDColName'

  
  N_REPEATS <- 1
  N_FEATURES_MAX <- 10
  N_BEST_FEATURES <- 10
  LEAPS_SEARCH_METHOD <- "seqrep" #N.B. do not do exhaustive w/ N_FEATURES_MAX > 5 it takes too long
  FOLD_SEED <- 123
  TRAIN_SEED <- 11111111
  RAND_INT_MAX <- 10000
  
  studyIDColName <- "StudyIdentifier"


  
  fullData <- m_cData
  
  # Prep ==================================
  
  #### Add BMI ##########################################
  # BMI = weight[kg] / (height[m]^2)
  fullData <- addBMIColumn(fullData)

  
  #### Clean up factor levels ###########################
  
  warning("Double check class groupings are as desired. Removew warning when checked.")
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
  
  kFoldsMax <- length(studyIDs.NoNas)
  if(is.null(kFolds) || !is.numeric(kFolds))
  {
    kFolds <- kFoldsMax
  }
  K_FOLDS <- max(min(kFolds, kFoldsMax), 1)
  
  #### Machine Learning #################################
  
  #melt so they're all together (also use ForViewing so that we don't melt the raw step data)
  #melted <- reshape2::melt(FULL_DATA.ForViewing, id.vars=c("StudyIdentifier", 
  #                                                         "NYHAClass",
  #                                                         "PureNYHAClass",
  #                                                      "ExplicitNYHAClass"))
  
  
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
  excludeVars <- c("NYHAClassMixed",  # dirty outcome variable
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
    excludeVars <- c(excludeVars, # v1.0 excludeVars
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
  
  # v1.2
  if(as.numeric(simonExtraMetrics.CodeVersion) > 1.1)
  {
    #excludeVars <- c(excludeVars # v1.0 excludeVars
    #)
  }
  
  if(isKEEP_ONLY_CPET)
  {
    cpetNamesToDrop <- colnames(FULL_DATA)[grepl("StepData", colnames(FULL_DATA))]
    excludeVars <- union(excludeVars, cpetNamesToDrop)
    rm(cpetNamesToDrop)
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
  result <- list()
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
    mtdResults <- list() # overall combined results for method
    foldResults <- list() # full model results
    finalModels <- list() # just the final trained models
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
      
      finalModels[[foldName]] <- foldResults[[foldName]]$finalModel
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
    
    print(glue("        saving results..."))
    trainResults[[mtd]] <- finalModels
    
    print(glue("Ended {mtd}"))
  }
  attr(testResults,'levels') <- levels(finalFoldTestData[[responseVar]])
  result[[RESULTS_VAR_NAME_TEST]] <- testResults
  result[[RESULTS_VAR_NAME_FINAL_MODEL]] <- trainResults
  
  attr(result,RESULTS_ATTR_NAME_METRICS_CODE_VERSION) <- simonExtraMetrics.CodeVersion
  attr(result,RESULTS_ATTR_NAME_IMPUTE) <- isIMPUTEMISSING_nDROP
  #result[[RESULTS_VAR_NAME_PRE_PROCESS]] <- 'preProcess'
  attr(result,RESULTS_ATTR_NAME_PRESELECT) <- isPRESELECT_FEATURES
  attr(result,RESULTS_ATTR_NAME_CPET_ONLY) <- isKEEP_ONLY_CPET
  attr(result,RESULTS_ATTR_NAME_STUDYIDCOLNAME) <- studyIDColName
  
  return(result)
}

# generate confusion matrix for the ML models generated in generateMLModels. 
# testResults should be the testResults returned from generateMLModels
# outcomeVar should be the name (as string) of the outcome var in the test results list
# ignoreVars should be a vector (of strings) of names in test result ignore/for which to not create ROC for
confusionMatrixForMLTestResults <- function(testResults,
                                            outcomeVar="NYHAClass",
                                            ignoreVars=c("StudyIdentifier"))
{
  #### Model Performance Evaluation #####################
  
  METHODS <- setdiff(names(testResults), c(outcomeVar, ignoreVars))
  
  
  # keep only complete cases where we can compare
  testResults.NoNAs <- testResults[complete.cases(testResults),]
  
  # threshold the probabilities
  testResults.Thresholded <- testResults.NoNAs
  studyIDColNum <- which(ignoreVars %in% names(testResults.Thresholded))
  testResults.Thresholded[,-studyIDColNum] <- round(testResults.NoNAs[, -studyIDColNum],0)
  
  testResults.Thresholded[-studyIDColNum] <- lapply(testResults.Thresholded[-studyIDColNum],
                                                    function(x) factor(x,
                                                                       levels=0:(length(attr(testResults,'levels'))-1),
                                                                       labels=attr(testResults,'levels')))
  
  trueValues <- testResults.Thresholded[[outcomeVar]]
  
  conf <- list()
  for(mtd in METHODS)
  {
    
    cat(glue("{mtd} test results: ----------------------------
             "))
    cat("\n")
    
    cat("\n")
    # Generate confusion matrix
    cat("\n")
    
    predictions <- testResults.Thresholded[[mtd]]
    
    conf[[mtd]] <- confusionMatrix(data=predictions,
                            reference=trueValues,
                            dnn= c("AI","Physician"))
    print(conf[[mtd]])
    cat(glue(""))
    
  }
  return(conf)
}


# generate ROC for the ML models generated in generateMLModels. 
# testResults should be the testResults returned from generateMLModels
# outcomeVar should be the name (as string) of the outcome var in the test results list
# ignoreVars should be a vector (of strings) of names in test result ignore/for which to not create ROC for
makeROCForMLTestResults <- function(testResults,
                                    outcomeVar="NYHAClass",
                                    ignoreVars=c("StudyIdentifier"),
                                    graphTitlePrefix="",
                                    saveGraphs = TRUE)
{
  #### Model Performance Evaluation #####################
  
  PLOT_TYPE <- "ROC"
  
  METHODS <- setdiff(names(testResults), c(outcomeVar, ignoreVars))
  
  # keep only complete cases where we can compare
  #testResults.NoNAs <- testResults[complete.cases(testResults),]
  
  rc <- list()
  for(mtd in METHODS)
  {
    
    cat(glue("{mtd} ROC: ----------------------------
             "))
    cat("\n")
    
    # Calculate ROC

    form <- formula(glue("{outcomeVar} ~ {mtd}"))
    rc[[mtd]] <- roc(form,
                     data=testResults,
                     direction = "<") # fix directions; default can change between models depending on control vs case results
    plot(rc[[mtd]],
         main = glue("{graphTitlePrefix} {mtd} ROC Plot, AUC:{round(auc(rc[[mtd]]),4)}"))
    cat(glue(""))
    
    if(saveGraphs)
    {
      fileName <- glue("{graphTitlePrefix}_{mtd}_{PLOT_TYPE}.png")
      savePlot(fileName,rc[[mtd]],isGGPlot=FALSE)
    }
    
    cat("\n")
  }
  
  cat(glue("All/Combined ROC: ----------------------------
             "))
  p <- pROC::ggroc(rc, size = 0.75) +
       ggtitle(glue("{graphTitlePrefix} ROC Plot - All Methods")) +
       labs(colour="Model Type") +
       theme_tufte() +
       geom_abline(colour='gray',
                   slope=1,
                   intercept=1)
  plot(p)
  if(saveGraphs)
  {
    fileName <- glue("{graphTitlePrefix}_all_{PLOT_TYPE}.png")
    savePlot(fileName,p,isGGPlot=TRUE)
  }
  
  return(rc)
}


# generate histogram for the ML Models predictor
makePredictorHistogram <- function(finalModels,
                                   graphTitlePrefix="",
                                   saveGraphs=FALSE)
{
  
  PLOT_TYPE <- "varBar"
  
  # mlModelResults$finalModels$glmboost$Fold01
  METHODS <- names(finalModels)
  
  totalVarImp <- list()
  for(mtd in METHODS)
  {
    print(glue("Starting on {mtd}"))
    
    folds <- finalModels[[mtd]]
    
    firstFold <- TRUE
    for(foldName in names(folds))
    {
     
      possibleError <- tryCatch({
        
        vI <- varImp(folds[[foldName]])
      
      }, #warning = function(w) { }, 
      error = function(err) {
        if(!startsWith(err$message,"no applicable method for 'varImp' applied to an object of class"))
        {
          stop(err)
        }
        return(err) # i.e. mask the error (but return it)
      }, finally = {

      })

      # explanation from https://stackoverflow.com/a/8094059
      if(!inherits(possibleError, "error"))
      {
        cat(glue("    Merging {foldName}..."))
        
        # Calculate Histogram
        cat("\n")
        vI$Feature <- rownames(vI)
        vI[[foldName]] <- vI$Overall
        vI$Overall <- NULL
        if(firstFold)
        {
          totalVarImp[[mtd]] <- vI
          firstFold <- FALSE
        }
        else
        {
          totalVarImp[[mtd]] <- merge(totalVarImp[[mtd]],
                                      vI,
                                      by ="Feature",
                                      all=TRUE)
        }
        cat(glue(""))
      }
      else
      {
        print(glue("  Predictor Histogram not supported for {mtd}"))
        break # was error so just skip all the folds
      }

    }
    if(!is.null(totalVarImp[[mtd]]))
    {
      # put variables as row names
      rownames(totalVarImp[[mtd]]) <- totalVarImp[[mtd]]$Variable
      # drop the seperate column for variable
      totalVarImp[[mtd]]$Variable <- NULL
      
      featureColNum <- which(names(totalVarImp[[mtd]]) == "Feature")
      totalVarImp[[mtd]][,-featureColNum] <- apply(totalVarImp[[mtd]][,-featureColNum],
                                                    MARGIN = 2, #use columns
                                                    FUN = function(X) (X - min(X))/diff(range(X)))
      
      # melt the data frame from wide to long
      plotDF <- reshape2::melt(totalVarImp[[mtd]], 
                               id.var="Feature",
                               variable.name="Fold",
                               value.name="Importance")
      # order so they'll be nicely in order of importance

      p <- plotDF %>% ggplot(aes(x = reorder(.$Feature, .$Importance, 
                                             function(x) {sum(x,na.rm=TRUE)}), # to order in descending order
                                 y = Importance,
                                 fill = Fold)) +
                      geom_bar(stat = "identity") +
                      labs(title = glue("{graphTitlePrefix} {mtd}"),
                           x = "Feature")  +
                      coord_flip() +
                      theme(legend.position = "none")
      
      plot(p)
      
      if(saveGraphs)
      {
        fileName <- glue("{graphTitlePrefix}_{mtd}_{PLOT_TYPE}.png")
        savePlot(fileName,p,isGGPlot=TRUE)
      }
      
    }

  }

  return(totalVarImp)
}

# N.B. print out plot before using savePlot
savePlot <- function(fileName,
                     plot,
                     isGGPlot=FALSE)
{
  fileName <- removeInvalidFileNameChars(fileName)
  fullFileName <- glue("plots/MLModels/{fileName}") 
  
  #create directory
  tryCatch({
    dir.create(file.path(getwd(), dirname(fullFileName)),
               recursive = TRUE)
  }, warning = function (war) {
    
    if(!endsWith(war$message,"already exists"))
    {
      warning(war)
    }
    
  }, error = function(err) {
    stop(err)
  }, finally = {
  }) # END tryCatch
  
  if(isGGPlot)
  {
    ggsave(fullFileName,
           plot=plot,
           path = getwd(),
           height = 8,
           width = 11,
           units = "in")
    cat("Printed ",fullFileName,"\n",sep="")
  }
  else
  {
    tryCatch({
      # create title
      dev.copy(png,
               fullFileName,
               width = 11,
               height = 8,
               units = "in",
               res = 300)
      cat("Printed ",fullFileName,"\n",sep="")
    },error = function(err) {
      cat("\n")
      print(glue("Failed at printing:
                         {fullFileName}"))
      stop(err)
    }, finally = {
      dev.off()
    })
  }
  

  
  

}

# END ------------------------------------------------------- 