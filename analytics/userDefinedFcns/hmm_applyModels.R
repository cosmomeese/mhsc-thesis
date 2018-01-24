### Apply HMM Models from Hidden Markov Model Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("dplyr","tidyr","depmixS4")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(dplyr)
library(tidyr)

library(depmixS4) #default
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
hmm_applyModels <- function(potentialModelsDF,data.test)
{
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_DEBUG <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  # potentialModelsDF in format
  #potentialModels_df <- data.frame(class as character(),
  #                                 response as list(),
  #                                 family as list()
  #                                 parameters as list of numerics
  #                                 nstates as numeric()
  #                                 cut.interval as character()
  #                                 logLik as class logLik
  #                                 AIC as numeric()
  #                                 BIC as numeric()
  #                                 summary as character string)
  
  if(VERBOSE_INFO){ cat("\nM: Pre-initializing result variable ",sep="") }
  
  maxSize <- nrow(potentialModelsDF)*length(levels(data.train$StudyIdentifier))  # this is max not exact size (recall some StudyIdentifiers are dropped)
  result.df <- data.frame(ModelID=numeric(maxSize),
                                      StudyIdentifier=character(maxSize),
                                      ModelClass=character(maxSize),
                                      logLik=numeric(maxSize),
                                      stringsAsFactors=FALSE)
  result.df[!is.na(result.df)] <- NA  # and fill all with NA's before we start
  result.df$StudyIdentifier <- factor(result.df$StudyIdentifier,
                                                  levels=levels(data.test$StudyIdentifier))  # assign the factor levels (StudyIdentifier)
  result.df$ModelClass <- factor(result.df$ModelClass,
                                             levels=levels(data.test$NYHAClass))   # assign the factor levels (ModelClass)
  
  result.df.index <- 1
  if(VERBOSE_INFO){ cat("\nM: Applying Models ",sep="") }
  for(modelIndex in 1:nrow(potentialModelsDF))
  {
    if(VERBOSE_INFO){ cat("\n   - Model ", modelIndex, sep="") }
    for(pat in levels(data.test$StudyIdentifier))
    {
      if(VERBOSE_INFO){ cat("\n      - Patient ", pat, sep="") }
      # only get 1 patient's data
      data.plotSubset <- data.test %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
      
      if(nrow(data.plotSubset) > 0)
      {
        if(VERBOSE_DEBUG){ cat("\n         - Binning ", sep="") }
        # bin data
        data.plotSubsetBinned <- binData(data.plotSubset,
                                         interval=potentialModelsDF$cut.interval[[modelIndex]])
        
        if(VERBOSE_DEBUG){ cat("\n         - Computing Sequence Length ", sep="") }
        # how compute any new parameters need
        ntimes <- getSequenceLengths(data.plotSubsetBinned,'StudyIdentifier')
        
        if(VERBOSE_DEBUG){ cat("\n         - Mixing Model", sep="") }
        # create the new depmix model object based on the computed/stored model parameters
        patientDepMix <- depmix(potentialModelsDF$response[[modelIndex]],
                                data=data.plotSubsetBinned,
                                family=potentialModelsDF$family[[modelIndex]],
                                nstates=potentialModelsDF$nstates[[modelIndex]],
                                ntimes=ntimes)
        if(VERBOSE_DEBUG){ cat("\n         - Updating Model Parameters", sep="") }
        patientDepMix <- setpars(patientDepMix,  # on model 
                                 potentialModelsDF$parameters[[modelIndex]][[1]])  # get parameters (which had to be stored as a list so it's always 1st element)
        
        if(VERBOSE_DEBUG){ cat("\n         - Computing Likelihood", sep="") }
        # compute the logLik that that patient's data stream was created by our model
        logLik <- forwardbackward(patientDepMix,return.all=FALSE)$logLike
        
        #store logLik
        if(VERBOSE_DEBUG){ cat("\n         - Storing Model Likelihood", sep="") }
        result.df$ModelID[result.df.index] <- modelIndex
        result.df$StudyIdentifier[result.df.index] <- pat
        result.df$ModelClass[result.df.index] <- potentialModelsDF$class[[modelIndex]]
        result.df$logLik[result.df.index] <- logLik
        
        if(VERBOSE_DEBUG){ cat("\n         - Model Stored as index ", result.df.index, sep="") }
        
        result.df.index <- result.df.index + 1
        
      }
      else
      {
        if(VERBOSE_INFO){ cat(" (skipping; has no data)", pat, sep="") }
      }
    }
    if(VERBOSE_INFO){ cat("\nM: Finished!",sep="") }
  }
  
  # drop all incomplete cases (where ModelID, Study Identifier, Model Class, logLik, etc. if others are added later) that contain NAs
  result.df <- result.df[complete.cases(result.df), ]
  
  return(result.df)
  
}