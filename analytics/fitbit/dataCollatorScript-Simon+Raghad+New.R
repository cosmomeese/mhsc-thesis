# Script to generate combined data for Cedric's Lab

#### TODO: #####
# 1. 

warning("This function can take a long time to execute")

STUDYDATA_FILE <- "H:/allStudiesData/AllStudiesData.RData"
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
                                        `Yaz Study ID` = col_character()))
  
  #melt the ID matrix so that we get a 1:1 table of 'old ID' to universal ID
  idMatrix.melt <- melt(idMatrix, 'Universal ID', na.rm = TRUE)

  idLookupTable <- idMatrix.melt$`Universal ID`
  names(idLookupTable) <- idMatrix.melt$value
  
  df <- df %>% mutate(UniversalStudyID = unname(idLookupTable[as.character(StudyIdentifier)]))
  df$UniversalStudyID <- factor(df$UniversalStudyID)
  
  return(df)
}

## Start

dataSetColname <- 'Dataset'
# Raghad Data
raghadDataSplit <- splitDateTimeToDayAndTime(raghadParticipantData.df,
                                             dropOldColumns = FALSE)
raghadDataSplit['Dataset'] <- "R"

# 'My' data (i.e. current data format)
jfbDataSplit <- splitDateTimeToDayAndTime(fitbitData.df, 
                                          dropOldColumns = FALSE)
jfbDataSplit['Dataset'] <- "J"

# Simon Data
simonData.StepsAndBaseDay <- addBaseDayFromTime(simonData.StepsOnly)
simonData.StepsAndBaseDay$Day <- simonData.StepsAndBaseDay$Day - 1
simonDataSplit <- mergeDayAndTimeToDateTime(simonData.StepsAndBaseDay,
                                            dropOldColumns = FALSE)
simonDataSplit$NYHAClass <- simonDataSplit$NYHAClassMixed
simonDataSplit$NYHAClassMixed <- NULL
simonDataSplit['Dataset'] <- "S"

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
write.csv(combinedData, file="H:/allStudiesData/AllStudiesDataForCedricsTeam.csv")