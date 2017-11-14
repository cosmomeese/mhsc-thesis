### Hidden Markov Model

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","dplyr","mhsmm","dglm")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(ggplot2)
library(ggthemes)
library(dplyr)
#library(depmixS4) #default, only works for single subjects
#library(seqHMM) #useful for TraMineR formatted sequence data
library(mhsmm)


sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
source(paste(sourceDir,"/","improvedGammaDist.R",
             sep=""))
rm(sourceDir)

#######################################################################
# VERIFY ALL THE REQUIRED PIECES ARE IN PLACE FIRST (BELOW)

if(!exists("m_cData"))
{
  stop("\nE: Could not find Data frame 'm_cData'. Make sure Data is loaded")
}

if(!exists("simonData.Importer.CodeVersion"))
{
  warning("\nW: Could not verify data version number. Please make sure Data was properly loaded. \nThere should be a variable 'simonData.Importer.CodeVersion' that exists to specify the data version.")
  
}else
{
  if(suppressWarnings(is.na(as.numeric(simonData.Importer.CodeVersion)))) #suppressWarnings because otherwise it gives an annoying warning message whenever data is not a numeric
  {
    warning(paste("\nW: Could not verify data version number since version is non-numeric."))
    
  } else if(as.numeric(simonData.Importer.CodeVersion) < 1.2)
  {
    stop(paste("\nE: Script does not support loaded m_cData (version v",simonData.Importer.CodeVersion,"). Script requires at least v1.2", sep=""))
    
  }else if(as.numeric(simonData.Importer.CodeVersion) > 1.2)
  {
    warning(paste("\nW: You are using an unverified m_cData version (v",simonData.Importer.CodeVersion,"). Code only tested on v1.2", sep=""))
  }
}

mhsmm_global.env <- new.env()

#######################################################################

####1#### Parameters & Starting Values -----------------------------------------

## DEBUG LEVELS
DEBUG_LEVEL <- 4
#DEBUGLVL.ERR <- 1
#DEBUGLVL.WARN <- 2
#DEBUGLVL.INFO <- 3
DEBUGLVL.DEBUG <- 4
DEBUGLVL.ALL <- DEBUGLVL.DEBUG + 1

## SKIP SECTIONS OF SCRIPT
SKIP_DATA_FETCH <- FALSE
SKIP_HMM_COMPUTE <- FALSE

## Starting values for model (define some reasonable ones)

# set random seed
#set.seed(111111)

# rescale parameters
UNSCALEDMIN.STEPS <- 0
UNSCALEDMAX.STEPS <- 300  # assume max value is 255 per minute
RESCALEDMIN.STEPS <- 1/(UNSCALEDMAX.STEPS - UNSCALEDMIN.STEPS) #1 OR + 1/(-1 + UNSCALEDMAX.STEPS - UNSCALEDMIN.STEPS)
RESCALEDMAX.STEPS <- 1 #301
RESCALEFACTOR.STEPS <- (RESCALEDMAX.STEPS - RESCALEDMIN.STEPS) / (UNSCALEDMAX.STEPS - UNSCALEDMIN.STEPS)

# hmm parameters
MAX_FINITE_VALUE <- 1.7976931348*10^(308)
DIST_LOG_PROB <- FALSE # use log probabilities to deal with small values
STATES <- 3
MAX_ITER <- 1000
TRAINSET_PERCENTAGE <- 1  # fraction of data set to use as trainingSet
NYHA_CLASS_VEC <- c("II","III")
INIT.P <- c(1,0,0) #pi
INIT.TRANS <- matrix(c(0.9, 0.3, 0.33,
                       0.05, 0.5, 0.33,
                       0.05, 0.2, 0.33), nrow = STATES)
# !! N.B. init emission distributions should be set in Pure-Model Distribution Setup (follows) !!

####1#### Parameters & Starting Values ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####2#### Pure-Model Distribution Setup ---------------------------------------------
# more info from "Hidden Semi Markov Models for Multiple Observation Sequences: The mhsmm Package for R"
# download @ https://www.jstatsoft.org/article/view/v039i04/v39i04.pdf

# Set initial emission distribution parameters below (will vary depending on distribution used)

initialStepGuess.means <- c(1,40,140) * RESCALEFACTOR.STEPS
initialStepGuess.variances <- c(10,10,10) * RESCALEFACTOR.STEPS
initialStepGuess.stdevs <- sqrt(initialStepGuess.variances)

### NORMAL ### (pg. 2)
#INIT.EMIS <- list(mu = initialStepGuess.means, sigma = initialStepGuess.stdevs) # for normal distribution
#DDIST.HSMM = dnorm.hsmm
#MSTEP.DIST = mstep.norm
#RDIST.HSMM = rnorm.hsmm 

### POISSON ### (pg. 17)
#INIT.EMIS <- list(lambda = (initialStepGuess.means, type = "poisson")  # for poisson distribution
#DDIST.HSMM = dpois.hsmm
#MSTEP.DIST = mstep.pois
#RDIST.HSMM = rpois.hsmm

### GAMMA (custom) ### (pg. 17)
# At large k/shape/a value the gamma distribution begins to converge to the normal dist (central limit theorem) w/
# mu = k*theta & sigma^2 = k*(theta^2)
# Because the rgamma distribution function has problems with small theta/scale/s (and the MStep function generates
# small theta when a large k is happening. Since the rnorm distribution doesn't have this problem we can will use it
# past given threshold
UPPER_APPROX_THRESHOLD <- 30*10^(2)  # normal limit theorem = 30 w/ 2 orders of magnitude for error (eng. safety factor + Fermi Approx)
LOWER_APPROX_THRESHOLD <- 0.02 # one order of magnitude less than code from: https://arxiv.org/pdf/1302.1884.pdf

# recall that you can define your own functions
INIT.EMIS <- list(shape = (initialStepGuess.means^2)/(initialStepGuess.variances),  # for large k, k = (mu/sigma)^2
                  scale = (initialStepGuess.means^2)/(initialStepGuess.stdevs),  # for large k, theta = (mu^2)/sigma
                  type = "gamma")  # for poisson distribution

DDIST.HSMM <- function (x, j, model){
  
    shape <- model$parms.emission$shape[j]
    scale <- model$parms.emission$scale[j]

    # if debugging print out parameters to help (debugging)
    printDebug = FALSE
    if(DEBUG_LEVEL >= DEBUGLVL.DEBUG)
    {
      printDebug = TRUE
    }
    if(printDebug)
    {
      cat("\n\tattempted parameters (shape=", shape, " & scale=", scale,")", sep="")
    }
    
    # estimate gamma distribution (approximate with normal if shape (k) is high)
    result <- dgammaPlus(x, shape=shape, scale=scale, log=DIST_LOG_PROB)
  
    return(result)
}

# estimates gamma distribution parameters using method of moments
gammafit2 <- function(x,wt=NULL) {
  # based on hmsmm but improved to handle convergence towards shape->inf
  tol = 1e-08
  
  if(is.null(wt)) wt = rep(1,length(x))
  
  tmp = cov.wt(data.frame(x),wt=wt)
  xhat = tmp$center
  xs = sqrt(tmp$cov)
  s = log(xhat) - mean(weighted.mean(log(x),wt))    
  aold = (xhat/xs)^2
  a = Inf
  if(Inf != aold) # added to gammafit2 (if cov too close to 0, then xs -> 0 & aold -> Inf), in which case shape = Inf, scale = 0)
  {
    while(abs(a-aold)>tol) {
      a = aold - (log(aold) - digamma(aold) - s)/((1/aold) - trigamma(aold))        
      aold=a
    }
  }
  # determined parameters ->
  # A. Scale
  scale = a
  # A.2: is scale within gamma distribution limits?
  if(scale < 0)
  {
    iteration = get('currentIttr', envir=mhsmm_global.env)
    warning(paste('Shape Parameter (=',scale,') is < 0 (unsupported by Gamma distribution): using absolute value. Possibly for iteration ', iteration ,'.',sep=""))
    scale <- abs(scale)
  }
  
  # B. Shape
  shape = xhat/scale
  
  # B.2: is shape within gamma distribution limits?
  if(shape < 0)
  {
    iteration = get('currentIttr', envir=mhsmm_global.env)
    warning(paste('Scale Parameter (=',shape,') is <= 0 (unsupported by Gamma distribution): if <0 then forcing to 0. Possibly for iteration ', iteration ,'.',sep=""))
    shape <- 0
  }
  # return
  return(list(shape=shape,scale=scale))
}

MSTEP.DIST <- function (x, wt) {
    # this is our hack to get some feedback on our progress
    currentIttr <- get('currentIttr', envir=mhsmm_global.env)
    currentIttr <- currentIttr + 1
    cat("\n\t-fit iteration ", currentIttr," (above)")
    assign('currentIttr', currentIttr, envir=mhsmm_global.env)
    
    # wt is a T x K matrix (T length, K states), do each one at a time
    k <- ncol(wt)
    shape <- numeric(k)
    scale <- numeric(k)
    for (i in 1:k){
      gammaPars <- gammafit2(x,wt[,i])
      shape[i] <- gammaPars$shape
      scale[i] <- gammaPars$scale
      #if(is.infinite(scale[i]))
      #{
      #  # then we must make it finite because the mhsmm library does not handle Inf values well
      #  scale[i] = MAX_FINITE_VALUE
      #}
      #if(is.infinite(scale[i]))
      #{
      #  shape[i] = MAX_FINITE_VALUE
      #}
    }
    return(list(shape=shape,scale=scale))
}


RDIST.HSMM <- function (x, j, model){
  
  shape=model$parms.emission$shape[j]
  scale=model$parms.emission$scale[j]
  
  if(0 == shape)
  {
    browser()
  }
  
  result <- rgammaPlus(x, shape=shape, scale=scale)
  
  return(result)
}

####2#### Pure-Model Distribution Setup ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####3#### Data Setup -----------------------------------------------------------

## HELPER FUNCTIONS
createHSMMDataSet <- function(dataSubSet)  # NOTE WE REUSE THIS IN TESTING (to help assemble test set)
{
  dataSubSet.unnest <- arrange(unnestStepDataFrame(dataSubSet),  # unnest set
                               StudyIdentifier,  # sort first by ID
                               Day,  # then by day
                               Time)  # then by time in day
  
  dataSubSet.table <- table(dataSubSet.unnest$StudyIdentifier)  # get table of each patient sequence in set
  dataSubSet.N <- as.numeric(dataSubSet.table)  # get length of each patient sequence in set
  dataSubSet.N <- dataSubSet.N[dataSubSet.N != 0]  # drop zero length vectors of each patient sequence in set (i.e. patients not in set)
  dataSubSet.seqM <- max(dataSubSet.N)  # max sequence length (important for fitting sojourn gamma distribution)
  cat("\n   - Converting training set to hsmm.data data type...")
  
  dataSubSet.class <- list(x = (dataSubSet.unnest$Steps - UNSCALEDMIN.STEPS)*RESCALEFACTOR.STEPS + RESCALEDMIN.STEPS,  # rescale
                           N = dataSubSet.N,
                           table = dataSubSet.table)
  class(dataSubSet.class) <- "hsmm.data"  # convert 'train' to 'hsmm.data' class
  return(dataSubSet.class)
}
## HELPER FUNCTIONS

if(!SKIP_DATA_FETCH)
{
  # Remember script assumes m_cData is preloaded (will pump out error message if this is not the case)
  DATA_SET_RAW <- m_cData[!is.na(m_cData$NYHAClass),] #i.e. m_cData excluding patients w/o NYHAClass
  cat("\nM: Loaded Dataset...")
  dataSet <- list()
  
  for(class in NYHA_CLASS_VEC)
  {
    dataSet[[class]] <- DATA_SET_RAW %>% filter(NYHAClass == class)
    cat("\nM: Isolating class ",class," data set", sep="")
  }
  
  trainSet.id <- list()
  testSet.id <- list()
  trainData.class <- list()
  train.seqM <- list()
  
  for(class in NYHA_CLASS_VEC)
  {
    
    ## Training Data Setup
    
    cat("\nM: Creating class ",class," training/test sets", sep="")
    
    numSubjects <- nrow(dataSet[[class]])
    trainRows <- sort(sample(numSubjects,numSubjects*TRAINSET_PERCENTAGE)) #randomly select rows
    trainSet <- dataSet[[class]][trainRows,] #extract training set
    testSet <- dataSet[[class]][-trainRows,] #extract test set
    trainSet.id[[class]] <-  trainSet[["StudyIdentifier"]] #just keep the ids for memory purposes (we can just extract the trainset again if needed later)
    testSet.id[[class]] <-  testSet[["StudyIdentifier"]] #just keep the ids for memory purposes (we can just extract the trainset again if needed later)
    rm(trainRows) #some cleanup
    
    
    cat("\n   - Unnesting training set...")
    if(nrow(trainSet) <= 0)
    {
      cat("\n   ERR - no patients in training set...")
      cat("\n       - pausing with browser() command to allow debugging.")
      browser() #i.e. pseudo-quit
    }
    
    trainData.class[[class]] <- createHSMMDataSet(trainSet)
    rm(trainSet)
    
#    cat("\n   - Unnesting test set...")
#    if(nrow(testSet) <= 0)
#    {
#      cat("\n   WRN - no patients in test set, using entire training set to test...")
#      #testData.class <- NULL
#      testDataEmpty <- TRUE
#      #testSet <- TRUE
#    }
#    else
#    {
#      warning(paste("Testing with less than full dataset is deprecated and method is non-functional/not-tested. \nTRAINSET_PERCENTAGE is", TRAINSET_PERCENTAGE, "should be 1.0"))
#      #testData.class[[class]] <- createHSMMDataSet(testSet[[class]])
#      testDataEmpty <- NULL
#    }
    rm(testSet)
  
    cat("\n   Finished separating out training & test set...")
    
#COWS EXAMPLE FOR REFERENCE
#data("reproai")
#data("reprocows")
#data("reproppa")
#N <- as.numeric(table(reprocows$id)) #length of each sequence
#trainData <- list(x = reprocows$activity, N = N) #training data
#class(trainData) <- "hsmm.data" #convert 'train' to 'hsmm.data' class
#COWS EXAMPLE FOR REFERENCE
  }
  
}else
{
  cat("\nM: Skipped loading dataset...")
}

####3#### Data Setup ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####4#### Sojourn Distribution Setup ---------------------------------------------
## Choice is between gamma, poisson or non-parametric distributions (for Semi-Markov Models).
##  Gamma distribution seems to work well for 'cow' sojourn data.
##  For human step/heart data we won't use a sojourn distribution since the assumption that so we'll try this for human step data too)

## Gamma distribution seems to work well for 'cow' sojourn data
 #tmp <- gammafit(reproppa * 24) #estimate initial state parameters (scale to hours instead of days)

 #M <- train.seqM
##M <- max(N) #max time to spend in each state
##d is used for initial E-step in EM algorithm. Will calculate Gamma parameters in following M step.
##for now use a 'crude estimate': uniform distribution w/ reasonable range
 #d <- cbind(dgamma(1:M, shape = tmp$shape, scale = tmp$scale),
           # ppa sojourn  directly estimated from ppa data set
 #           dunif(1:M, 4, 30), #
           # oestrus between 4 and 30 hours
 #           dunif(1:M, 15*24, 40*24)) #num days in hours?
           # non-oestrus between 15 and 40 days

####4#### Sojourn Distribution Setup ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####5#### Specify & Fit Hidden Semi-Markov Model -------------------------------
## Specify & Fit the Hidden Semi-Markov Model

if(!SKIP_HMM_COMPUTE)
{
  hmm.activity <- list()
  for(class in NYHA_CLASS_VEC)
  {
    cat("\nM: HSMM Model for class ",class,"...")
    #for testing ---------
    #class = "II"
    #cat("   - Using only class",class,"to test...")
    #for testing ^^^^^^^^^
    
    cat("\n   - Specifying HSMM Model for class ",class,"...")
    startval <- hmmspec(init = INIT.P,
                        trans = INIT.TRANS,
                        #transition = INIT.TRANS, #for Semi-Markov Model only
                        parms.emission = INIT.EMIS, 
                        #sojourn = list(d = d, type = "gamma"), #for Semi-Markov Model only
                        dens.emission = DDIST.HSMM)
    cat("\n   - Fitting HSMM Model for class ",class,"...")
    
    assign('currentIttr', 0, envir=mhsmm_global.env)
    
    hmm.activity[[class]] <- hmmfit(x = trainData.class[[class]],
                                    start.val = startval,
                                    #model = startval, #for Semi-Markov Fit Only
                                    mstep = MSTEP.DIST, 
                                    maxit = MAX_ITER,
                                    #M=train.seqM, #for Semi-Markov Fit Only
                                    tol = 1e-6, #for Full Markov Fit Only (default 1e-08)
                                    #graphical =  TRUE, #for hsmmfit only
                                    lock.transition = FALSE #IF TRUE will not re-estimate the transition matrix
                                    )

    cat("\n   -- Finished fitting Model")
  }
}else 
{
  cat("\nM: Skipped computing HMM Models...")
}

####5#### Specify & Fit Hidden Semi-Markov Model ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####6#### Test & Verify Model --------------------------------------------------
## Test Predictive Ability

cat("\nM: Test HMM Model Predictive Abilities...")

## Helper Functions
# functions for dynamically creating column names in our hmm test results dataframe
testColPrefix <- 'class'
testColSuffix <- 'ModelProb'

# to create name given class
getModelDFTestColForClass <- function(class){
    return(paste(testColPrefix,class,testColSuffix,sep=""))
}
# to extract class given name
getClassForModelDFTestColName <- function(colName){
    patBegin <- testColPrefix
    idxBegin <- regexpr(patBegin, colName)
    patEnd <- testColSuffix
    idxEnd <- regexpr(patEnd, colName)
    result <- substring(colName, idxBegin + nchar(patBegin), idxEnd - 1)
    return(result)
}

# function that finds a model prediction for a given HMM model & patient ID
predictPatient <- function(model, id){
    singlePatientData <- DATA_SET_RAW %>% filter(StudyIdentifier == id)
    singlePatientData.hsmmtype <- createHSMMDataSet(singlePatientData)
    p <- predict(model, singlePatientData.hsmmtype)
    return(p)
}
## END Helper Functions

## START ->

# Setup results dataframe 
hmm.test <- select(DATA_SET_RAW, StudyIdentifier, NYHAClass)  # get existing from raw dataset
hmm.test['predictedClass'] = NaN  # add a column for predicted class (that we determine below)

# add columns to hmm test results dataframe
modelProbColumns <- getModelDFTestColForClass(NYHA_CLASS_VEC) # generate list of columns names for each model class
hmm.test[modelProbColumns] <- NaN # add columns for each model class (that we determine below)

# get unnested very of full raw dataset
dataSet.unnested <- arrange(unnestStepDataFrame(DATA_SET_RAW), #unnest set
                            StudyIdentifier, #sort first by ID
                            Day,             #then by day
                            Time)      #then by time in day

# determine the prediction of each model for each patient in testset
for(modelNYHAClass in NYHA_CLASS_VEC)
{
  cat("\n   - Testing HSMM Model for class ",modelNYHAClass,"...")
  colName <- getModelDFTestColForClass(modelNYHAClass)
  
  for(patientGroupClass in names(trainData.class))  # get patient set by class (because that's how our data is structured)
  {
    if(testDataEmpty)
    {
      cat("\n   WRN - testing with complete training data")
      hmm.test <- mutate(hmm.test, !!colName := predictPatient(hmm.activity[[patientGroupClass]],StudyIdentifier))
    }
    else
    {
      stop(paste("Script does not yet support testing with test set. \nSet TRAINSET_PERCENTAGE to 1.0 to test with full train set instead (is", TRAINSET_PERCENTAGE, ")", sep=""))
      #predictions = predictFun(//dataframe with subset of data//, hmm.activity[[modelNYHAClass]])
    }
    
    
  }

}

#COWS EXAMPLE FOR REFERENCE
#last.heat.hour <- cumsum(rle(yhat)$lengths)[rle(yhat)$values == 2]
#cows.validation <- reprocows[last.heat.hour, ]
#dif <- list()
#for(i in 1:nrow(reproai)){
#  for(j in reproai$days.from.calving[reproai$id == i])
#    dif[[paste(i,j)]] <- j - subset(cows.validation, id == i)$days.from.calving
#}
#dif <- unlist(dif)
#dif <- dif[abs(dif) < 15]
#plot(density(dif), xlab = "Standing heat time - AI time", main = "")
#rug(jitter(dif))
#dif
#COWS EXAMPLE FOR REFERENCE

####6#### Test & Verify Model ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####7#### Plot & Generate Outputs ----------------------------------------------
  #plot things!

#COWS EXAMPLE FOR REFERENCE
#add an index for each cow
#reprocows <- reprocows %>% group_by(id) %>% mutate(timeIndex = seq_along(id))
#
#
#qplot(y=activity,x=1:length(activity),data=reprocows,geom="smooth",na.rm=FALSE)
#
#plot <- ggplot(data = reprocows %>% filter(id==1), 
#               aes(y=activity,x=timeIndex)) +
#          theme_tufte(base_family = "serif", 
#                      ticks = FALSE) +
#          geom_line(na.rm=TRUE,aes(color=as.factor(id))) +
#          labs(title = "Cow Activity",
#               subtitle = "sTitle",
#               caption = "caption") +
#          xlab("Activity")
#print(plot)
#COWS EXAMPLE FOR REFERENCE

####7#### Plot & Generate Outputs ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
