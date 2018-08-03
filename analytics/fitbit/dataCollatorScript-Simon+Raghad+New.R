# Script to generate combined data for Cedric's Lab

#### TODO: #####
# 1. 
require(readr)
require(reshape2)
require(tidyverse)
require(readr)

warning("This function can take a long time to execute")

# TO UPDATE SOURCE FILES, UPDATE DATA IN STUDYDATA_FILE AND ONLY THEN PROCEED TO USE THIS SCRIPT

STUDYDATA_FILE <- "H:/allStudiesData/AllStudiesDataRaw.RData"
STUDYSPLIT_FILE <- "H:/allStudiesData/AllStudiesDataSplit.RData"
STUDYCOMBINED_FILE <- "H:/allStudiesData/AllStudiesDataCombined.RData"
ID_MATRIX_FILE <- "H:/allStudiesData/MedlyFitbitSimonRaghadUniqueMatrix_NoPHI.csv"

load(STUDYDATA_FILE)

source('D:/GoogleDrive/Business/Work/University/Degree - Masters/3a - Thesis/code/analytics/userDefinedFcns/analyzeSimonData_common.R')

addUniversalIDCol <- function(df,matrix.file=ID_MATRIX_FILE)
{
  require(readr)
  require(reshape2)
  require(tidyverse)
  
  library(readr)
  idMatrix <- read_csv(ID_MATRIX_FILE, 
                       col_types = cols(`Medly ID` = col_character(), 
                                        `Numbered ID for Raghad` = col_character(), 
                                        `Raghad ID` = col_character(),
                                        `Simon ID` = col_character(), 
                                        `Universal ID` = col_character(), 
                                        `Yaz Study ID` = col_character(),
                                        `Lubdub ID` = col_character())
                                        )
  
  cat("Adding universal IDs. This may take a while...")
  #melt the ID matrix so that we get a 1:1 table of 'old ID' to universal ID
  idMatrix.melt <- melt(idMatrix, 'Universal ID', na.rm = TRUE)

  idLookupTable <- idMatrix.melt$`Universal ID`
  names(idLookupTable) <- idMatrix.melt$value
  
  df <- df %>% mutate(UniversalStudyID = unname(idLookupTable[as.character(StudyIdentifier)]))
  df$UniversalStudyID <- factor(df$UniversalStudyID)
  cat("Finally finished adding universal IDs! :)")
  
  return(df)
}

## Start

raghadDataSetID <- 'R'
dataSetColname <- 'Dataset'
# Raghad Data
raghadDataSplit <- splitDateTimeToDayAndTime(raghadParticipantData.df,
                                             dropOldColumns = FALSE)
raghadDataSplit[dataSetColname] <- raghadDataSetID

# 'My' data (i.e. current data format)
jfbDataSetID <- 'J'
jfbDataSplit <- splitDateTimeToDayAndTime(fitbitData.df, 
                                          dropOldColumns = FALSE)
jfbDataSplit[dataSetColname] <- jfbDataSetID
jfbDataSplit <- jfbDataSplit %>% mutate(!!dataSetColname := ifelse(StudyIdentifier == "Canary",
                                                                   NA, 
                                                                   !!rlang::sym(dataSetColname)))

# Simon Data
simonDataSetID <- 'S'
simonData.StepsAndBaseDay <- addBaseDayFromTime(simonData.StepsOnly)
simonData.StepsAndBaseDay$Day <- simonData.StepsAndBaseDay$Day - 1
simonDataSplit <- mergeDayAndTimeToDateTime(simonData.StepsAndBaseDay,
                                            dropOldColumns = FALSE)
simonDataSplit$NYHAClass <- simonDataSplit$NYHAClassMixed
simonDataSplit$NYHAClassMixed <- NULL
simonDataSplit[dataSetColname] <- simonDataSetID

saveVariables <- c("jfbDataSplit",
                   "raghadDataSplit",
                   "simonDataSplit")
save(list = saveVariables,
     file = STUDYSPLIT_FILE,
     precheck = TRUE)

# Combine together
combinedData <- rbind(jfbDataSplit,raghadDataSplit,simonDataSplit)
combinedData$Dataset <- as.factor(combinedData$Dataset)
# create NYHAClassMixed column (since this is what the
# addMissingClassGroupings function uses as source)
combinedData$NYHAClassMixed <- combinedData$NYHAClass
# create NYHAClass column: we need this for current implementation of 
# addMissingClassGroupings function - this might be a bug, but for now
# this is faster than fixing
combinedData <- addMissingClassGroupings(combinedData,
                                         removeHF018=FALSE)
combinedData$NYHAClass <- NULL # remove this as we now have 3 different NYHA class columns to chose from (so this is likely misleading)

# create a class matrix for easier comparison
classesByStudyID <- getClassesByStudyID(combinedData)

cat("\nN.B. there will be no person in Raghad's data with ID 2\n")

# add universal ID col
combinedData <- addUniversalIDCol(combinedData)

# move last column (Universal ID) to first
lastColName <- names(combinedData)[[ncol(combinedData)]]
combinedData <- combinedData %>% select(lastColName, everything())

# to save:
cat("\nSaving Data\n")
# save Rdata
saveVariablesCombined <- c("classesByStudyID",
                           "combinedData")
save(list = saveVariablesCombined,
     file = STUDYCOMBINED_FILE,
     precheck = TRUE)
# save CSV

# JFB
currentStudyOnly <- combinedData %>% 
                    filter((!!rlang::sym(dataSetColname)) == jfbDataSetID)
write.csv(currentStudyOnly, file="H:/allStudiesData/AllStudiesDataForCedricsTeam.csv")

# Simon
#currentStudyOnly <- combinedData %>% 
#  filter((!!rlang::sym(dataSetColname)) == simonDataSetID)
#write.csv(currentStudyOnly, file="H:/allStudiesData/SimonFitbitDataForCedricsTeam.csv")

# Raghad
#currentStudyOnly <- combinedData %>% 
#  filter((!!rlang::sym(dataSetColname)) == raghadDataSetID)
#write.csv(currentStudyOnly, file="H:/allStudiesData/RaghadFitbitDataForCedricsTeam.csv")
