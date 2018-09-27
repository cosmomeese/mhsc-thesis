### Determine Data Distribution Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("fitdistrplus","logspline","tidyverse")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(fitdistrplus)
library(logspline)
library(tidyverse)

## NOTE!!! MAKE SURE TO LOAD DISTRIBUTION BEFORE RUNNING SCRIPT (MODIFY BELOW):


simonDataSetID <- 'S'
dataSetColname <- 'Dataset'
nyha2 <- "II"
nyha3 <- "III"


rescaleNoNa <- function(x,t=c(MIN_FINITE_VALUE,1),f=range(x, na.rm = TRUE,finite = TRUE))
{
  MIN_FINITE_VALUE <- .Machine$double.xmin #https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html
  
  x <- scales::rescale(x, to=t, from=f)
  x <- x[!is.na(x)]
  
  return(x)
}

# Subset data

subData <- combinedData %>% filter((!!rlang::sym(dataSetColname)) == simonDataSetID)

dataVectorRange <- subData %>% .$Steps %>% range(na.rm=TRUE, finite=TRUE)


# Extract Data
dataVector2 <-  subData %>% filter(RoundDownNYHAClass == nyha2) %>%
                        .$Steps %>% rescaleNoNa(f=dataVectorRange)

dataVector3 <- subData %>% filter(RoundDownNYHAClass == nyha3) %>% 
                        .$Steps %>% rescaleNoNa(f=dataVectorRange)

# Cullen & Frey graph
n2Dist <- descdist(dataVector2, discrete = FALSE, boot = 500, obs.col='orange', boot.col='blue')

n3Dist <- descdist(dataVector3, discrete = FALSE, boot = 500, obs.col='purple', boot.col='blue')

# Fit Distribution

dist <- 'beta' # get from cullen & frey graph above

n2DistFit <- fitdistrplus::fitdist(data=dataVector2,distr=dist,method="mme")

n3DistFit <- fitdistrplus::fitdist(data=dataVector3,distr=dist,method="mme")


# Tests

nDist <- n3DistFit
shape1 <- nDist$estimate[["shape1"]]
shape2 <- nDist$estimate[["shape2"]]

testStepCount <- rescaleNoNa(c(112.9,125.7),f=dataVectorRange)
pbeta(testStepCount,shape1,shape2)