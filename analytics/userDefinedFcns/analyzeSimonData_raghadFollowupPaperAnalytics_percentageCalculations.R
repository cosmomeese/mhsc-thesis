
#########################################################################################

# import H:\simonData\Simon_pCombinedData-v1.2-wMetrics-v1.4.RData
load("H:/simonData/Simon_pCombinedData-v1.2-wMetrics-v1.4.RData")

#########################################################################################

### Function to calculate Percentages for Follow up to Raghad's Paper using Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("plyr","tidyverse","reshape2","glue", "ggthemes","Hmisc")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(glue)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("analyzeData_common",
             "analyzeSimonData_common",
             "unnestStepDataFrame",
             "removeInvalidFileNameChars",
             "savePlot")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

fullData <- m_cData

# Prep ==================================

#### Add BMI ##########################################
# BMI = weight[kg] / (height[m]^2)
#fullData <- addBMIColumn(fullData)

#### Clean up factor levels ###########################

fullData <- addMissingClassGroupings(fullData)
fullData$NYHAClass <- fullData$RoundDownNYHAClass
fullData$RoundDownNYHAClass <- NULL

#fullData <- cleanHandednessColumnForAnalysis(fullData)

#fullData <- cleanWristbandColumnForAnalysis(fullData)

#fullData <- cleanSexColumnForAnalysis(fullData)

#### Tag fullData as immutable
FULL_DATA <- fullData
#rm(fullData)

# Create FULL_DATA.ForViewing for easier viewing of data using: View(FULL_DATA.ForViewing)
FULL_DATA.ForViewing <- createViewableFullData(FULL_DATA);

# Drop HF018 since it's not used
FULL_DATA.ForViewingNO18 <- FULL_DATA.ForViewing[-18,]

unnestedDF <- FULL_DATA %>% unnestStepDataFrame()
zeroCount <- unnestedDF[!is.na(unnestedDF$NYHAClass),] %>% 
              aggregate(Steps ~ StudyIdentifier, . , 
                        function(x) {c(zeroes = sum(x == 0),
                                       total = length(x),
                                       percent = 100*sum(x == 0)/length(x)
                                      )}
                        )

# Reorder by percent
zeroCountReorder <- zeroCount[order(zeroCount$Steps[,'percent']),]
zeroCountReorder

meanZeroes <- mean(zeroCount$Steps[,'zeroes'])
meanZeroes # 20170

meanTotal <- mean(zeroCount$Steps[,'total'])
meanTotal # 22982

meanPercent <- mean(zeroCount$Steps[,'percent'])
meanPercent # 87.76%

meanZeroes/meanTotal # 87.32%

# so since percentages not equal, base on 
