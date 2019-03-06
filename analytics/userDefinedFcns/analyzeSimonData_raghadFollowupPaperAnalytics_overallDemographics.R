
#########################################################################################

# import H:\simonData\Simon_pCombinedData-v1.2-wMetrics-v1.4.RData
load("H:/simonData/Simon_pCombinedData-v1.2-wMetrics-v1.4.RData")

#########################################################################################

### Function to calculate Demographics + Statistics for Follow up to Raghad's Paper using Simon Bromberg's Thesis Data (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true
SAVE_CSVS <- TRUE
SAVE_STAT_TEST <- FALSE
SAVE_JMIR_TABLE <- TRUE

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
library(ggthemes)  # for tufte themes
library(plyr)
library(dplyr)
library(reshape2)
library(glue)
library(Hmisc)

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
fullData <- addBMIColumn(fullData)

#### Clean up factor levels ###########################

fullData <- addMissingClassGroupings(fullData)
fullData$NYHAClass <- fullData$RoundDownNYHAClass
fullData$RoundDownNYHAClass <- NULL

fullData <- cleanHandednessColumnForAnalysis(fullData)

fullData <- cleanWristbandColumnForAnalysis(fullData)

fullData <- cleanSexColumnForAnalysis(fullData)

#### Tag fullData as immutable
FULL_DATA <- fullData
#rm(fullData)

# Create FULL_DATA.ForViewing for easier viewing of data using: View(FULL_DATA.ForViewing)
FULL_DATA.ForViewing <- createViewableFullData(FULL_DATA);

# Drop HF018 since it's not used
FULL_DATA.ForViewingNO18 <- FULL_DATA.ForViewing[-18,]

# Get Age, Height, Weight & BMI
vars <- c("Age","Height","Weight","BMI")
cat("\n")
for(var in vars)
{
  cat(var, ":", fivenum(FULL_DATA.ForViewingNO18[[var]]),"\n")
}
