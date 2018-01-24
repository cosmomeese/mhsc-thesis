### Generate HMM Models for Hidden Markov Model Script

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
hmm_generateModels <- function(trainingData)
{
  # set seed
  SEED <- 1
  
  # set debug
  VERBOSE_INFO <- debugGivenThreshold(DEBUGLVL.INFO)
  VERBOSE_HMMFIT <- debugGivenThreshold(DEBUGLVL.DEBUG)
  
  # help available at: http://files.meetup.com/1704326/Ingmar_Visser_depmixS4_Markov_models_22_Sept_2014.pdf

  ### return variable
  potentialModels_df <- data.frame(class=character(), # as character
                                   response=logical(), # as list therefore: replace below w/ list and see comment
                                   family=logical(), # as list therefore: replace below w/ list
                                   parameters=numeric(), # as list of numerics
                                   nstates=numeric(), # as numeric
                                   cut.interval=character(), # as character string
                                   logLik=numeric(), # as class logLik
                                   AIC=numeric(), # as numeric
                                   BIC=numeric(), # as numeric
                                   summary=character(), # as character string
                                   stringsAsFactors = FALSE)
  potentialModels_df$response <- list()  # do this seperately because no one seems to know how to make it work using the 'data.frame' format above
  potentialModels_df$family <- list()  # do this seperately because no one seems to know how to make it work using the 'data.frame' format above
  potentialModels.nextRow <- 1
  
  ### hmm parameters
  
  # for multi-variate testing
  #formulae <- list(Steps~1,HeartRate~1)
  #families <- list(gamma(),gaussian())
  formulae <- list(Steps~1)
  families <- list(gaussian())
  
  for(class in CONSTANTS$NYHA_CLASS_VEC)
  {
    
    nstates <- 3
    init.p <- c(0.95,0.0,0.05)
    #init.p <- c(1,0,0)
    init.trans <- matrix(c(0.9, 0.3, 0.33,
                           0.05, 0.5, 0.33,
                           0.05, 0.2, 0.33), nrow = length(nstates))
    
    ## initial emission probabilities
    initialStepGuess.means <- c(1,40,100) * CONSTANTS$RESCALEFACTOR.STEPS
    initialStepGuess.variances <- c(10,80,1000) * (CONSTANTS$RESCALEFACTOR.STEPS^2)
    initialStepGuess.stdevs <- sqrt(initialStepGuess.variances)
    
    # for gamma distribution - assume it's shape
    init.emisGamma <- t(matrix(c((initialStepGuess.means^2)/(initialStepGuess.variances)#,  # shape: for large k, k = (mu/sigma)^2
                                 #(initialStepGuess.variances)/(initialStepGuess.means)     # scale: for large k, theta = (sigma^2)/mu
                                ), nrow = length(nstates)))
    
    # for normal distribution: mu, sigma (for each state
    init.emisNorm <- t(matrix(c(initialStepGuess.means,
                                initialStepGuess.stdevs),
                       nrow = length(nstates)))
    
    # arrange in depmixS4 compatible format (see pg 12 of: http://ftp.cs.pu.edu.tw/network/CRAN/web/packages/depmixS4/vignettes/depmixS4.pdf)
    #init.params = c(init.p,init.trans,init.emisGamma) #to check param order fit model then run: setpars(HMM, value = 1:npar(HMM))
    init.params = c(init.p,init.trans,init.emisGamma)
                   
    if(VERBOSE_INFO){ cat("\nM: Generating models for class ",class, sep="") }
    
    # get class specific data
    classTrainingData <- trainingData %>% filter(NYHAClass == class)
    
    ## START BINNING ##
    interval <- "6 hour"
    finalTrainData <- binData(classTrainingData, interval)
    ## END BINNING ##
    
    set.seed(SEED) # to control randomness between runs (mostly for debugging)
    
    if(VERBOSE_INFO){ cat("\n   - Creating depmix (HMM) definition...") }
    
    #just try one patient
    #finalTrainData <-  finalTrainData %>% filter(StudyIdentifier == 'HF001')
    
    ntimes <- getSequenceLengths(finalTrainData,'StudyIdentifier')
    
    #We're setting the formula list as our response variables, finalTrainData dataframe we just extract, want to set 3 different regimes/states, and setting the response distributions to be the families list.
    HMM<-depmix(formulae,
                data=finalTrainData,
                nstates=nstates,
                family=families,
                ntimes=ntimes,
                instart=init.p, #these are starting probability (values)
                #trstart=init.trans,  # these are the starting transition prob
                respstart=init.emisNorm
                )
    #setpars(HMM,init.params) # turnsout you don't need this
    
    if(VERBOSE_INFO){ cat("\n   - Fitting HMM...\n") }
    wrapSuppress <- R.utils::captureOutput # this will supress output
    if(VERBOSE_INFO || VERBOSE_HMMFIT)
    { # then wrap in an empty function
      wrapSuppress <- function(expr) {eval(expr)}
    }
    wrapSuppress(HMMfit<-fit(HMM,
                        verbose = VERBOSE_HMMFIT)) #fit our model to the data set
    #output <- R.utils::captureOutput(HMMfit<-fit(HMM, 
    #                                             verbose = VERBOSE_HMMFIT,
    #                                             emcontrol=em.control(random.start=FALSE)) #use if you want to ctrl EM algorithm, e.g. no rand start
    #                                 )
    if(VERBOSE_INFO){ cat("   - Finished Fitting HMM ----") }
    
    if(VERBOSE_HMMFIT){ cat("\n\n   - Model Summary -----------\n") }
    if(VERBOSE_HMMFIT){ summary(HMMfit) } #model summary
    if(VERBOSE_HMMFIT){ print(HMMfit) } #model AIC & BIC we can compare the log Likelihood as well as the AIC and BIC values to help choose our model
    if(VERBOSE_HMMFIT){ cat("   - Model Summary END -------\n") }
    
    if(VERBOSE_INFO){ cat("\nM: Storing model") }
    potentialModels_df[[potentialModels.nextRow,"class"]] <- class
    potentialModels_df[[potentialModels.nextRow,"response"]] <- I(formulae)
    potentialModels_df[[potentialModels.nextRow,"family"]] <- I(families)
    potentialModels_df[[potentialModels.nextRow,"parameters"]] <- I(list(getpars(HMMfit)))
    potentialModels_df[[potentialModels.nextRow,"nstates"]] <- nstates
    potentialModels_df[[potentialModels.nextRow,"cut.interval"]] <- interval
    potentialModels_df[[potentialModels.nextRow,"logLik"]] <- logLik(HMMfit)
    potentialModels_df[[potentialModels.nextRow,"AIC"]] <- AIC(HMMfit)
    potentialModels_df[[potentialModels.nextRow,"BIC"]] <- BIC(HMMfit)
    potentialModels_df[[potentialModels.nextRow,"summary"]] <- R.utils::captureOutput(summary(HMMfit), collapse="\n")
    
    if(VERBOSE_INFO){ cat("\n   - Model stored with index ",potentialModels.nextRow, sep="") }
    potentialModels.nextRow <- potentialModels.nextRow + 1  # update to be ready for next row
  }
  
  if(VERBOSE_INFO){ cat("\nM: Finished generating models") }
  return(potentialModels_df)
}