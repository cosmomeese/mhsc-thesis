### Function to calculate Demographics + Statistics for Paper Summarizing Simon Bromberg's Thesis (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true
SAVE_CSVS <- TRUE

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue","leaps","pROC","caret")
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
  
  #### Analysis #########################################
  
  #melt so they're all together (also use ForViewing so that we don't melt the raw step data)
  #melted <- reshape2::melt(FULL_DATA.ForViewing, id.vars=c("StudyIdentifier", 
  #                                                         "NYHAClass",
  #                                                         "PureNYHAClass",
  #                                                         "ExplicitNYHAClass"))
  
  # Start here ==========================================
  
  #### Best Parameter Search ############################
  
  # explanatory variables that were found to be statistically significant during previous analysis
  sigVars  = c("SBP.Resting",
               "SBP",
               "HR",
               "HR.1minDrop",
               "Duration",
               "Watts",
               "Watts.Predicted.Percentage",
               "Weight",
               "VO2.Predicted.Relative.Percentage",
               "VCO2.Peak",
               "VEperVCO2.Peak",
               "VEperVCO2.atAT",
               "AT",
               "VO2.Peak.Predicted.Percentage",
               "RER.Peak",
               "StepData.MeanDailyTotalSteps",
               "StepData.StdDevDailyTotalSteps",
               #"StepData.MeanDailyMeanSteps",  # = StepData.MeanDailyTotalSteps / num participants
               #"StepData.StdDevDailyMeanSteps",  # = StepData.MeanDailyTotalSteps / num participants
               "StepData.MeanDailyMaxSteps",
               "StepData.ModeDailyMaxSteps",
               "StepData.MaxDailyMaxSteps")
  
  # desired response variable to test for
  responseVar <- "NYHAClass"
  
  # subset the FULL_DATA set to only deal with response and sig variables
  dataForModelSearch <- subset(FULL_DATA, select=c(responseVar,sigVars))
  # drop incomplete cases in this subsetted dataset (since regsubsets can't deal with NAs)
  dataForModelSearch <- dataForModelSearch[complete.cases(dataForModelSearch),] 
  
  responseVarColNum <- which(names(dataForModelSearch) %in% c(responseVar))
  # find best models
  modelSearch <- leaps::regsubsets(as.matrix(dataForModelSearch[,-responseVarColNum]),
                                  dataForModelSearch[,responseVarColNum],
                                  nbest = 5,
                                  nvmax = 5)
  
  # format and save them for analysis and combing through in xl
  modelSearchSummary <- summary(modelSearch)
  whichModels.Df <- as.data.frame(modelSearchSummary$which)
  whichModels.Df$Rsquared <- modelSearchSummary$rsq
  whichModels.Df$ResidualSumOfSquares <- modelSearchSummary$rss
  whichModels.Df$MallowsCp <- modelSearchSummary$cp
  whichModels.Df$BIC <- modelSearchSummary$bic
  if(SAVE_CSVS)
  {
    write.csv(whichModels.Df, file = "bestRegressionModels.csv")
  }
  
  #### Model Performance Evaluation #####################
  
  # subset the FULL_DATA set to only deal with response and sig variables
  predictVars <- c("RER.Peak","HR.1minDrop","StepData.MeanDailyMaxSteps","VEperVCO2.atAT")
  dataForPrediction <- subset(FULL_DATA, select=c(responseVar,predictVars))
  # drop incomplete cases in this subsetted dataset (since regsubsets can't deal with NAs)
  dataForPrediction <- dataForPrediction[complete.cases(dataForPrediction),] 
  
  form <- as.formula(glue("{responseVar} ~  {paste(predictVars, collapse= '+')}"))
  
  # split into train test (maybe not needed?)
  # set randomness seed for reproducability
  
  #set.seed(123)
  splitPercentage <- 0.70
  trainIndices <- sample(seq_len(nrow(dataForPrediction)), 
                         size = floor(splitPercentage * nrow(dataForPrediction)))
  
  
  trainData <- dataForPrediction[trainIndices, ]
  testData <- dataForPrediction[-trainIndices, ]

  # generate model
  #glmModels <- list()
  #glmModels[[1]] <- glm(formula = form,
  #                      data = trainData,
  #                      family=binomial(link="logit"))
  
  
  
  # https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
  # http://topepo.github.io/caret/train-models-by-tag.html#Generalized_Linear_Model
  trainingControl <- trainControl(method = "LOOCV") # leave one out cross validation
  #trainingControl <- trainControl(method = "cv",  # k-fold Cross Validation
  #                                number=3) 
  
  
  # Generate & train some models
  methods <- c("nnet", # kappa around -0.04-0.633 beat by pcaNNet
               #"ada", # kappa around 0.4-0.69, most around 0.51
               #"adaboost", # kappa around 0.25-0.47
               "pcaNNet", # kappa around 0.33-0.79
               "glm", # kappa = 0.6790698
               "glmboost") # kappa = 0.738041
  
  trainResults <- list()
  for(mtd in methods)
  {
    trainResults[[mtd]] <- train(form,
                                 method = mtd,
                                 data = trainData,
                                 trControl = trainingControl)

  }
  
  for(mtd in methods)
  {
    cat(glue("{mtd} train results:"))
    cat(glue(""))
    print(trainResults[[mtd]]$results)
  }
  
  # Test our models
  
  # Show summary statistics
  #print(summary(glmModels[[1]]))
  
  for(mtd in methods)
  {

    cat(glue("{mtd} test results: ----------------------------
             "))
    cat("\n")
    
    # Calculate ROC
    predictions<-predict(trainResults[[mtd]],
                        testData)
                        #type=c("response")

    #g <- roc(NYHAClass ~ predictions, data = data.frame(testData,
    #                                                    predictions))
    #plot(g)    
    
    cat("\n")
    #print(auc(g))
    cat("\n")
    
    conf <- confusion.glm(data=data.frame(testData,predictions),
                          model=trainResults[[mtd]])
    print(conf)
    cat(glue(""))
    
  }
  
  
}

# END ------------------------------------------------------- 