#depmixS4 Tutorial from https://inovancetech.com/hmm-tutorial-1.html

if(!exists('EURUSD1d'))
{
  stop("Load up dataset from Tutorial (URL in Script) before running")
}


#install.packages("depmixS4")
library(depmixS4) #the HMM library we’ll use

#install.packages("quantmod")
library(quantmod) #a great library for technical analysis and working with time series

library(ggplot2)
library(tidyr)
library(dplyr)

#Date<-as.character(EURUSD1d[,1])
Date <- EURUSD1d$`Open Timestamp` # modified from above because obviously we imported differently (I used dataframe)
DateTS<- as.POSIXlt(Date, format = "%Y.%m.%d %H:%M:%S") #create date and time objects

addNoise <- function(element)
{
  if(is.numeric(element))
  {
    element <- element + rnorm(length(element), mean = 0, sd = 0.1)
  }
  return(element)
}

cat('\nAssembling Training Dataset...')
ModelData.big <- data.frame(LogReturns=numeric(),ATR=numeric())
ntimes <- c()
for(i in 1:27)
{
  EURUSD1d.error <- data.frame(lapply(EURUSD1d,FUN=addNoise)) # noisy
  #EURUSD1d.error <- EURUSD1d # clean
  
  TSData.error<-data.frame(EURUSD1d.error[,2:5],row.names=DateTS)
  TSData.error<-as.xts(TSData.error) #build our time series data set
  
  ATRindicator<-ATR(TSData.error[,2:4],n=14) #calculate the indicator
  ATR<-ATRindicator[,2] #grab just the ATR
  
  LogReturns <- log(EURUSD1d.error$Close) - log(EURUSD1d.error$Open) #calculate the logarithmic returns (N.B. corrected typo!)
  
  ModelData<-data.frame(LogReturns,ATR) #create the data frame for our HMM model
  
  ModelData<-ModelData[-c(1:14),] #remove the data where the indicators are being calculated
  
  colnames(ModelData)<-c("LogReturns","ATR") #name our columns
  
  ntimes <- c(ntimes,nrow(ModelData))
  ModelData.big <- bind_rows(ModelData.big,ModelData)
}

cat('\nAssembling Test Dataset...')

TSData <-data.frame(EURUSD1d[,2:5],row.names=DateTS)
TSData<-as.xts(TSData) #build our time series data set

ATRindicator<-ATR(TSData[,2:4],n=14) #calculate the indicator
ATR<-ATRindicator[,2] #grab just the ATR

LogReturns <- log(EURUSD1d$Close) - log(EURUSD1d$Open) #calculate the logarithmic returns (N.B. corrected typo!)

ModelData.test<-data.frame(LogReturns,ATR) #create the data frame for our HMM model

ModelData.test<-ModelData.test[-c(1:14),] #remove the data where the indicators are being calculated

colnames(ModelData.test)<-c("LogReturns","ATR") #name our columns

# Now Build the HMM

# for multi-variate testing
form <- list(LogReturns~1,ATR~1)
fam <- list(gaussian(),gaussian())

# for uni-variate testing
#form <- formula(LogReturns~1)
#form <- formula(ATR~1)
#fam <- gaussian()

cat('\nCreating depmix...')
set.seed(1)
HMM<-depmix(form,data=ModelData.big,nstates=3,family=fam,ntimes=ntimes) #We’re setting the LogReturns and ATR as our response variables, using the data frame we just built, want to set 3 different regimes, and setting the response distributions to be gaussian.

cat('\nFitting HMM:')
HMMfit<-fit(HMM, verbose = FALSE) #fit our model to the data set

cat('\nPrinting HMM:\n')
print(HMMfit) #we can compare the log Likelihood as well as the AIC and BIC values to help choose our model

cat('\nSummarizing HMM:\n')
summary(HMMfit)

cat('\nCreating test depmix:')
set.seed(1)
HMM.test <- depmix(form,data=ModelData.test,nstates=3,family=fam) #identical to the above

cat('\nFitting HMM:')
HMM.test <- setpars(HMM.test,getpars(HMMfit))
HMMfit.test<-fit(HMM.test, verbose = FALSE) #fit our model to the data set

cat('\nPrinting HMM:\n')
print(HMMfit.test) #we can compare the log Likelihood as well as the AIC and BIC values to help choose our model

cat('\nSummarizing HMM:\n')
summary(HMMfit.test)

cat('\nCalculating Posterior Probabilities\n')

HMMpost<-posterior(HMMfit.test) #find the posterior odds for each state over our true data set

head(HMMpost) #we can see that we now have the probability for each state for everyday as well as the highest probability class.

HMMpost$timestamp <- data.frame(DateTS)[-c(1:14),]
HMMpost.gathered <- HMMpost %>% gather(stateKey,probability,S1:S3)

cat('\nPlotting HMM Probabilities\n')
plt <- ggplot(data=HMMpost.gathered,
              aes(x=timestamp, y=probability)) +
        facet_grid(stateKey~.) +
        geom_line(alpha=1)
print(plt)
