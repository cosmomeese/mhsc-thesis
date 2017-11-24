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
library(depmixS4) #default
#library(seqHMM) #useful for TraMineR formatted sequence data
#library(mhsmm)
#library(aphid)
library(dplyr)
library(tidyr)

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

# local functions
hmm_generateModels <- function(trainingData)
{
  # help available at: http://files.meetup.com/1704326/Ingmar_Visser_depmixS4_Markov_models_22_Sept_2014.pdf
  
  ### return variable
  potentialModelList <- list() #NOTE: should probably not use list since one for each class?
  
  ### hmm parameters
  
  # for multi-variate testing
  #formulae <- list(Steps~1,HeartRate~1)
  #families <- list(gamma(),gaussian())
  formulae <- list(Steps~1)
  families <- list(Gamma())
  
  for(class in NYHA_CLASS_VEC)
  {
    
    nstates <- 3
    init.p <- c(1,0,0)
    init.trans <- matrix(c(0.9, 0.3, 0.33,
                           0.05, 0.5, 0.33,
                           0.05, 0.2, 0.33), nrow = length(nstates))
    
    ## initial emission probabilities
    initialStepGuess.means <- c(1,40,100) * RESCALEFACTOR.STEPS
    initialStepGuess.variances <- c(10,80,1000) * (RESCALEFACTOR.STEPS^2)
    initialStepGuess.stdevs <- sqrt(initialStepGuess.variances)
    
    # for gamma distribution - assume it's shape
    init.emisGamma <- (initialStepGuess.means^2)/(initialStepGuess.variances)  # shape: for large k, k = (mu/sigma)^2
                      #(initialStepGuess.variances)/(initialStepGuess.means)  #scale: for large k, theta = (sigma^2)/mu
    
    # for normal distribution: mu, sigma (for each state
    init.emisNorm <- t(matrix(c(initialStepGuess.means,
                            initialStepGuess.stdevs), nrow = length(nstates)))
    
    # arrange in depmixS4 compatible format (see pg 12 of: http://ftp.cs.pu.edu.tw/network/CRAN/web/packages/depmixS4/vignettes/depmixS4.pdf)
    init.params = c(init.p,init.trans,init.emisGamma) #to check param order fit model then run: setpars(HMM, value = 1:npar(HMM))
                   
    cat("\nM: Generating model for class ",class, sep="")
    
    # get class specific data
    classTrainingData <- trainingData %>% filter(NYHAClass == class)
    
    set.seed(1) # to control randomness between runs (mostly for debugging)
    
    cat("\n   - Creating depmix (HMM) definition...")
    
    #just try one patient
    #classTrainingData <-  classTrainingData %>% filter(StudyIdentifier == 'HF001')
    
    ntimes <- getSequenceLengths(classTrainingData,'StudyIdentifier')
    
    #We're setting the formula list as our response variables, classTrainingData dataframe we just extract, want to set 3 different regimes/states, and setting the response distributions to be the families list.
    HMM<-depmix(formulae,
                data=classTrainingData,
                nstates=3,
                family=families,
                ntimes=ntimes)
    setpars(HMM,init.params)
    
    cat("\n   - Fitting HMM...\n")
    HMMfit<-fit(HMM, verbose = TRUE, emcontrol=em.control(random.start=FALSE)) #fit our model to the data set
    cat("\n   - Finished Fitting HMM ----")
    
    cat("\n   - Model Summary -----------")
    summary(HMMfit) #model summary
    print(HMMfit) #model AIC & BIC we can compare the log Likelihood as well as the AIC and BIC values to help choose our model
    cat("\n   - Model Summary END -------")
    
  }
  

  
  cat('\nCreating depmix...')
  set.seed(1)
  HMM<-depmix(form,data=trainingData,nstates=3,family=fam,ntimes=ntimes) #We’re setting the LogReturns and ATR as our response variables, using the data frame we just built, want to set 3 different regimes, and setting the response distributions to be gaussian.
  
  cat('\nFitting HMM:')
  HMMfit<-fit(HMM, verbose = FALSE) #fit our model to the data set
  
  cat('\nPrinting HMM:\n')
  print(HMMfit) #we can compare the log Likelihood as well as the AIC and BIC values to help choose our model
  
  cat('\nSummarizing HMM:\n')
  summary(HMMfit)
  
  
  
  # states ####
  STATES <- c("Begin","Fair","Loaded")
  #STATES <- c(1,2,3) ###
  ### initial state probabilities
  INIT.P <- c(0.99,0.005,0.005) #pi
  ### initial transition probabilities ###
  #INIT.TRANS <- matrix(c(0.9, 0.3, 0.33,
  #                       0.05, 0.5, 0.33,
  #                       0.05, 0.2, 0.33), nrow = length(STATES))
  INIT.TRANS <- matrix(c(0, 0, 0,
                         0.99, 0.95, 0.1,
                         0.01, 0.5, 0.9), nrow = length(STATES))
  dimnames(INIT.TRANS) <- list(from = STATES, to = STATES)
  
  
  return(potentialModelList)
}

# for depmixS4 if you want to use multiple sequences you must specify length of each sequence. This extracts that from the dataframe using assuming they are each uniquely labelled in the column col
getSequenceLengths <- function(df,col)
{
  ## Extract the series lengths
  ntimes <- as.numeric(table(df[col]))  # get table (and length of that table) of each patient sequence in set
  ntimes <- ntimes[ntimes != 0]  # drop zero length vectors of each patient sequence in set (i.e. patients not in set)
  return(ntimes)
}


#####################################################################################################################
while(FALSE)
{
  

# hmm parameters
  # states ####
STATES <- c("Begin","Fair","Loaded")
#STATES <- c(1,2,3) ###
  ### initial state probabilities
INIT.P <- c(0.99,0.005,0.005) #pi
  ### initial transition probabilities ###
#INIT.TRANS <- matrix(c(0.9, 0.3, 0.33,
#                       0.05, 0.5, 0.33,
#                       0.05, 0.2, 0.33), nrow = length(STATES))
INIT.TRANS <- matrix(c(0, 0, 0,
                       0.99, 0.95, 0.1,
                       0.01, 0.5, 0.9), nrow = length(STATES))
dimnames(INIT.TRANS) <- list(from = STATES, to = STATES)
  # emission probabilities

# !! N.B. init emission distributions should be set in Pure-Model Distribution Setup (follows) !!

####1#### Parameters & Starting Values ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####2#### Pure-Model Distribution Setup ---------------------------------------------
# more info from "Analysis with Profile Hidden Markov Models"
# package info @ https://cran.r-project.org/web/packages/aphid/aphid.pdf
# vignette @ https://cran.r-project.org/web/packages/aphid/vignettes/aphid-vignette.html

# Set initial emission distribution parameters below (will vary depending on distribution used)

initialStepGuess.means <- c(1,40,100) * RESCALEFACTOR.STEPS
initialStepGuess.variances <- c(10,80,1000) * (RESCALEFACTOR.STEPS^2)
initialStepGuess.stdevs <- sqrt(initialStepGuess.variances)

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
                  scale = (initialStepGuess.variances)/(initialStepGuess.means),  # for large k, theta = (sigma^2)/mu
                  type = "gamma")  # for gamma distribution

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
  dataSubSet.seqM <- max(dataSubSet.N)  # max sequence length (important for fitting sojourn gamma distribution
  
  printDebug = FALSE
  if(DEBUG_LEVEL >= DEBUGLVL.DEBUG)
  {
    printDebug = TRUE
  }
  if(printDebug)
  {
    cat("\n   - Converting training set to hsmm.data data type...")
  }
  
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
    
   cat("\n   - Unnesting test set...")
   if(nrow(testSet) <= 0)
   {
     cat("\n   WRN - no patients in test set, using entire training set to test...")
     #testData.class <- NULL
     testDataEmpty <- TRUE
     #testSet <- TRUE
   }
   else
   {
     warning(paste("Testing with less than full dataset is deprecated and method is non-functional/not-tested. \nTRAINSET_PERCENTAGE is", TRAINSET_PERCENTAGE, "should be 1.0"))
     #testData.class[[class]] <- createHSMMDataSet(testSet[[class]])
     testDataEmpty <- NULL
   }
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

# function that finds a model prediction for a given HMM model & patient IDs
predictPatient <- function(model, ids){
  prob <- numeric(length(ids))
  for(i in 1:length(ids))
  {
    singlePatientData <- DATA_SET_RAW %>% filter(StudyIdentifier == ids[i])
    singlePatientData.hsmmtype <- createHSMMDataSet(singlePatientData)
    prediction <- predict(model, singlePatientData.hsmmtype)
    prob[i] <- prediction$loglik
  }
  return(prob)
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
  
  #for(patientGroupClass in names(trainData.class))  # get patient set by class (because that's how our data is structured)
  #{
    if(testDataEmpty)
    {
      cat("\n   WRN - testing with complete training data")
      model <- hmm.activity[[modelNYHAClass]]
      hmm.test <- mutate(hmm.test, !!colName := predictPatient(model,StudyIdentifier))
      
      #cat("\n   - Group Class", modelNYHAClass)
      #cat("\n Shape:",model$model$parms.emission$shape)
      #cat("\n Scale:",model$model$parms.emission$scale)
      #cat("\n II Prob:\n")
      #cat(hmm.test$classIIModelProb)
      #cat("\n III Prob:\n")
      #cat(hmm.test$classIIIModelProb)
    }
    else
    {
      stop(paste("Script does not yet support testing with test set. \nSet TRAINSET_PERCENTAGE to 1.0 to test with full train set instead (is", TRAINSET_PERCENTAGE, ")", sep=""))
      #predictions = predictFun(//dataframe with subset of data//, hmm.activity[[modelNYHAClass]])
    }
  #}

}

# get greatest class

hmm.test.subset <-hmm.test[modelProbColumns]
hmm.test.maxColumns <- max.col(hmm.test.subset,ties.method="first")
hmm.test.predictedClass <- getClassForModelDFTestColName(colnames(hmm.test.subset)[hmm.test.maxColumns])
hmm.test['predictedClass'] <- factor(hmm.test.predictedClass, levels=levels(NYHA_CLASS_VEC)) 
rm(hmm.test.subset, hmm.test.maxColumns, hmm.test.predictedClass)
#confuseMat(predictedC=hmm.test['predictedClass'],trueC=hmm.test['NYHAClass'])
confusionMatrix(data=hmm.test$predictedClass, reference=hmm.test$NYHAClass)

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
}