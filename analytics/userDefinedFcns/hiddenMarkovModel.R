### Hidden Markov Model

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","dplyr","mhsmm")
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

## Starting values for model (define some reasonable ones)

# rescale parameters
unscaledMin.steps <- 0
unscaledMax.steps <- 300  # assume max value is 255 per minute
rescaledMin.steps <- 1 # + 1/(-1 + unscaledMax.steps - unscaledMin.steps)
rescaledMax.steps <- 301
rescaleFactor.steps <- (rescaledMax.steps - rescaledMin.steps) / (unscaledMax.steps - unscaledMin.steps)

# hmm parameters
states <- 3
maxIter <- 1000
trainSetPercentage <- 0.7  # fraction of
nyhaClasses <- c("II","III")
init.P <- c(1,0,0) #pi
init.trans <- matrix(c(0.9, 0.3, 0.33,
                       0.05, 0.5, 0.33,
                       0.05, 0.2, 0.33), nrow = states)
# !! N.B. init emission distributions should be set in Pure-Model Distribution Setup (follows) !!

####1#### Parameters & Starting Values ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####2#### Pure-Model Distribution Setup ---------------------------------------------
# more info from "Hidden Semi Markov Models for Multiple Observation Sequences: The mhsmm Package for R"
# download @ https://www.jstatsoft.org/article/view/v039i04/v39i04.pdf

# Set initial emission distribution parameters below (will vary depending on distribution used)

### NORMAL ### (pg. 2)
#init.emis <- list(mu = c(0, 2.5, 0), sigma = c(1, 1, 1))  # for normal distribution
#ddist.hsmm = dnorm.hsmm
#mstep.dist = mstep.norm
#rdist.hsmm = rnorm.hsmm 

### POISSON ### (pg. 17)
#init.emis <- list(lambda = (c(10, 80, 100)/rescaleFactor.steps), type = "poisson")  # for poisson distribution
#ddist.hsmm = dpois.hsmm
#mstep.dist = mstep.pois
#rdist.hsmm = rpois.hsmm

### GAMMA (custom) ### (pg. 17)
# recall that you can define your own functions
init.emis <- list(shape = (c(10, 80, 100)), scale = c(1,1,1), type = "gamma")  # for poisson distribution
ddist.hsmm = function (x, j, model)
              {
                result = dgamma(x, shape=model$parms.emission$shape[j],scale=model$parms.emission$scale[j])
                result[is.na(result)] = 0  #if it returns an na then it's an invalid result
                cat("\n\tattempted invalid parameters (shape=",
                    model$parms.emission$shape[j],
                    " & scale=",
                    model$parms.emission$scale[j],")")
                return(result)
              }
mstep.dist = function (x, wt) 
              {
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
                  gammaPars <- gammafit(x,wt[,i])
                  shape[i] <- gammaPars$shape
                  scale[i] <- gammaPars$scale
                }
                return(list(shape=shape,scale=scale))
              }
rdist.hsmm = function (x, j, model)
              {
                rgamma(x, shape=model$parms.emission$shape[j],scale=model$parms.emission$scale[j])
              }

####2#### Pure-Model Distribution Setup ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####3#### Data Setup -----------------------------------------------------------

# Remember script assumes m_cData is preloaded (will pump out error message if this is not the case)
dataSetRAW <- m_cData[!is.na(m_cData$NYHAClass),]
cat("\nM: Loaded Dataset...")
dataSet <- list()

for(class in nyhaClasses)
{
  dataSet[[class]] <- dataSetRAW %>% filter(NYHAClass == class)
  cat("\nM: Isolating class ",class," data set", sep="")
}
rm(dataSetRAW)

trainSet <- list()
testSet <- list()
trainData.class <- list()
testData.class <- list()
train.seqM <- list()

for(class in nyhaClasses)
{
  
  ## Training Data Setup
  
  cat("\nM: Creating class ",class," training/test sets", sep="")
  
  numSubjects <- nrow(dataSet[[class]])
  trainRows <- sort(sample(numSubjects,numSubjects*trainSetPercentage)) #randomly select rows
  trainSet[[class]] <- dataSet[[class]][trainRows,] #extract training set
  testSet[[class]] <- dataSet[[class]][-trainRows,] #extract test set 
  rm(trainRows) #some cleanup
  
  cat("\n   - Unnesting training set...")
  trainSet.unnest <- arrange(unnestStepDataFrame(trainSet[[class]]), #unnest training set
                                                StudyIdentifier, #sort first by ID
                                                Day,             #then by day
                                                Time)      #then by time in day
  cat("\n   - Unnesting test set...")
  testSet.unnest <- arrange(unnestStepDataFrame(testSet[[class]]), #unnest test set
                                                StudyIdentifier, #sort first by ID
                                                Day,             #then by day
                                                Time)      #finally by time in day
  
  train.table <- table(trainSet.unnest$StudyIdentifier) #get table of each patient sequence in training set
  test.table <- table(testSet.unnest$StudyIdentifier)  #get table of each patient sequence in test set
  train.N <- as.numeric(train.table) #get length of each patient sequence in training set
  test.N <- as.numeric(test.table)  #get length of each patient sequence in test set
  train.N <- train.N[train.N != 0] #drop zero length vectors of each patient sequence in training set (i.e. patients not in set)
  test.N <- test.N[test.N != 0] #drop zero length vectors of each patient sequence in training set (i.e. patients not in set)
  train.seqM[[class]] <- max(train.N) #max sequence length (important for fitting sojourn gamma distribution)
  
  cat("\n   - Creating hsmm.data classes from unnested sets...")
  
  trainData.class[[class]] <- list(x = (trainSet.unnest$Steps - unscaledMin.steps)*rescaleFactor.steps + rescaledMin.steps,
                                   N = train.N,
                                   table = train.table)
  class(trainData.class[[class]]) <- "hsmm.data" #convert 'train' to 'hsmm.data' class
  
  testData.class[[class]] <- list(x = (testSet.unnest$Steps - unscaledMin.steps)*rescaleFactor.steps + rescaledMin.steps,
                                  N = test.N,
                                  table = test.table)
  class(testData.class[[class]]) <- "hsmm.data" #convert 'train' to 'hsmm.data' class
  
  rm(trainSet.unnest,testSet.unnest,train.N,test.N)
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

hmm.activity <- list()
for(class in nyhaClasses)
{
  cat("\nM: HSMM Model for class ",class,"...")
  #for testing ---------
  #class = "II"
  #cat("   - Using only class",class,"to test...")
  #for testing ^^^^^^^^^
  
  cat("\n   - Specifying HSMM Model for class ",class,"...")
  startval <- hmmspec(init = init.P,
                       trans = init.trans,
                       #transition = init.trans, #for Semi-Markov Model only
                       parms.emission = init.emis, 
                       #sojourn = list(d = d, type = "gamma"), #for Semi-Markov Model only
                       dens.emission = ddist.hsmm)
  cat("\n   - Fitting HSMM Model for class ",class,"...")
  
  assign('currentIttr', 0, envir=mhsmm_global.env)
  hmm.activity[[class]] <- hmmfit(x = trainData.class[[class]],
                                    start.val = startval,
                                    #model = startval, #for Semi-Markov Fit Only
                                    mstep = mstep.dist, 
                                    maxit = maxIter,
                                    #M=train.seqM, #for Semi-Markov Fit Only
                                    tol = 1e-8, #for Full Markov Fit Only (default 1e-08)
                                    lock.transition =TRUE)
  cat("\n   -- Finished fitting Model")
}


####5#### Specify & Fit Hidden Semi-Markov Model ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####6#### Test & Verify Model --------------------------------------------------
## Test Predictive Ability
yhat <- predict(hmm.activity, trainData)$s

  #cow specific
last.heat.hour <- cumsum(rle(yhat)$lengths)[rle(yhat)$values == 2]
cows.validation <- reprocows[last.heat.hour, ]
dif <- list()
for(i in 1:nrow(reproai)){
  for(j in reproai$days.from.calving[reproai$id == i])
    dif[[paste(i,j)]] <- j - subset(cows.validation, id == i)$days.from.calving
}
dif <- unlist(dif)
dif <- dif[abs(dif) < 15]
plot(density(dif), xlab = "Standing heat time - AI time", main = "")
rug(jitter(dif))
dif

####6#### Test & Verify Model ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

####7#### Plot & Generate Outputs ----------------------------------------------
  #plot things!
 
#add an index for each cow
reprocows <- reprocows %>% group_by(id) %>% mutate(timeIndex = seq_along(id))

#
qplot(y=activity,x=1:length(activity),data=reprocows,geom="smooth",na.rm=FALSE)

plot <- ggplot(data = reprocows %>% filter(id==1), 
               aes(y=activity,x=timeIndex)) +
          theme_tufte(base_family = "serif", 
                      ticks = FALSE) +
          geom_line(na.rm=TRUE,aes(color=as.factor(id))) +
          labs(title = "Cow Activity",
               subtitle = "sTitle",
               caption = "caption") +
          xlab("Activity")
print(plot)

####7#### Plot & Generate Outputs ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
