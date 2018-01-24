### Score HMM Models from Hidden Markov Model Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("dplyr","tidyr","caret")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(dplyr)
library(tidyr)
library(caret)

#library(depmixS4) #default
#library(seqHMM) #useful for TraMineR formatted sequence data
#library(mhsmm)
#library(aphid)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("hmm_common")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

# -------------------------------------------------------

# global parameters
# warning! imports some globals through hmm_common

# local functions
hmm_scoreModels <- function(modelProbabilitysForPatients.df,trueClasses.df)
{
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_DEBUG <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  if(VERBOSE_INFO){ cat("\nM: Scoring Models ",sep="") }
  
  classColName <- 'NYHAClass'
  newColName <- 'TrueClass'
  
  # Assemble Combinations
  modelCombinations <- hmm_scoreModels.Assemble(modelProbabilitysForPatients.df)
  
  # Establish Class
  trueClassFactorLevels <- levels(trueClasses.df[[classColName]])
  modelCombinations.classified <- hmm_scoreModels.Classify(modelCombinations,trueClassFactorLevels)
  
  # Merge Predicted + True Class into one data frame
  modelCombinations.classified <- modelCombinations.classified %>% merge(trueClasses.df)
  colnames(modelCombinations.classified)[colnames(modelCombinations.classified)==classColName] <- newColName # rename old class col to newColName
  
  # Evaluate Overall Performance for Model Combos
  score <- hmm_scoreModels.Evaluate(modelCombinations.classified)
  
  return(result.df)
  
}

# assemble possiblity combinations of models & return in a data frame
hmm_scoreModels.Assemble <- function(modelProbabilitysForPatients.df)
{
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_DEBUG <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  # Score Model Combinations
  if(VERBOSE_INFO){ cat("\n   - Assembling different model combinations ", sep="") }
  
  # for now we just implement 2 model combinations - check if it's more
  numModels <- length(modelProbabilitysForPatients.df$ModelID %>% as.factor() %>% levels)
  numClasses <- length(modelProbabilitysForPatients.df$ModelClass %>% as.factor() %>% levels)
  if(numModels != numClasses)
  {
    stop(paste("Current functionality only supports scoring of one model per class. Input is for ", numModels, " Models & ", numClasses, " Classes", sep=""))
  }
  
  # drop the now unnecessary ModelID column since we just have 1:1
  modelProbabilites.subset <- modelProbabilitysForPatients.df[ , !names(modelProbabilitysForPatients.df) %in% c("ModelID")]
  
  # add a label so we can identify this model combination
  modelProbabilites.subset$ModelCombination <- as.factor("default")
  
  return(modelProbabilites.subset)
  
}

hmm_scoreModels.Classify <- function(modelCombinations,trueClassFactorLevels)
{
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_DEBUG <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  predictedClassColName <- "PredictedClass"
  
  # Score Model Combinations
  if(VERBOSE_INFO){ cat("\n   - Finalizing classification ", sep="") }
  
  # reshape from long table to wide table (but make sure we keep track of the new column names)
  possiblePredictedClasses <- modelCombinations$ModelClass %>% as.factor() %>% levels()
  possiblePredictedClasses <- sort(possiblePredictedClasses,
                                   decreasing = FALSE)  # sort to make sure that II always before III which are both before IV
  classified <- modelCombinations %>% tidyr::spread(ModelClass,logLik)
  
  classified[predictedClassColName] <- NA
  
  firstCol <- TRUE
  bestSoFarColName <- "bestSoFar"
  for(classCol in possiblePredictedClasses)
  {
    if(classCol %in% names(classified))  # make sure it actually exists as a column first!
    {
      if(firstCol) # if first col, then these are all the 'best' result so far
      {
        classified[predictedClassColName] <- classCol
        classified[bestSoFarColName] <- classified[[classCol]]
        firstCol <- FALSE
      }else
      {
        whereIsBetterModel <- classified[[classCol]] >= classified[[bestSoFarColName]]
        classified[[predictedClassColName]][whereIsBetterModel] <- classCol
        classified[[bestSoFarColName]][whereIsBetterModel] <- classified[[classCol]][whereIsBetterModel]
      }
    }
  }
  # now drop temp column
  classified <- classified[ , !names(classified) %in% c(bestSoFarColName)]
  
  # and make sure predicted Class name is predicted class name w/ right number of factors
  classified[predictedClassColName] <- factor(classified[[predictedClassColName]],levels=trueClassFactorLevels)

  return(classified)
}


hmm_scoreModels.Evaluate <- function(classifiedModelsWithPredictedAndTrueClasses)
{
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_DEBUG <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  # Score Model Combinations
  if(VERBOSE_INFO){ cat("\n   - Evaluating different model combinations ", sep="") }
  
  classifiedDF <- classifiedModelsWithPredictedAndTrueClasses
  
  confusion <- confusionMatrix(data=classifiedDF$PredictedClass,
                               reference=classifiedDF$TrueClass,
                               dnn= c("AI","Physician"))
  print(confusion)
  
  return(confusion)
  
}