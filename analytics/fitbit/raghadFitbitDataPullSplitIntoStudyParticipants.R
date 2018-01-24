## Script to assign Raghad Raw Fitbit Data Streams into Appropriate Study Participant## 
## V1.0 2018-01-23 Jonathan-F. Baril

# The following script outlines the process used after 
# downloading Raghad's Fitbit Data (from the 6 emails used) to 
# split the 6 data streams into their appropriate chunks for each
# study participant
# N.B. this uses a second data csv 
# (I'm too lazy to program intelligence to use a single one)

warning("Import fitbitData.df from RaghadDataRawPull.Rdata (or use fitbitPullScriptForStudy to pull raw Fitbit data from the 6 study emails)")

reprocessfilePath <- "fitbit/RaghadData-Clean.csv"
reprocessCSV <- read_csv(reprocessfilePath)

reprocessCSV$StudyIdentifier <- as.factor(reprocessCSV$`Study Identifier`)
reprocessCSV$`Study Identifier` <- NULL # rename Study Identifier
reprocessCSV$FitbitEmail <- reprocessCSV$'Fitbit Email'
reprocessCSV$'Fitbit Email' <- NULL

# Clear extra imported attributes
attr(reprocessCSV, "spec") <- NULL

## Process

workingCopy <- fitbitData.df
workingCopy$temp <- factor(NA,levels=levels(reprocessCSV$StudyIdentifier))
#iterate below for each StudyIdentifier
for(pIndex in 1:nrow(reprocessCSV))
{
  thisKitID <- workingCopy$StudyIdentifier==reprocessCSV$Kit[[pIndex]]
  startDate <- reprocessCSV$StartDate[[pIndex]]
  endDate <- as.POSIXct(paste(reprocessCSV$EndDate[[pIndex]],"23:59:00"),
                        format = "%Y-%m-%d %H:%M:%S")
  if(!(is.na(startDate) || is.na(endDate)))
  {
    dateRange <- ((startDate<=workingCopy$DateTime) & (workingCopy$DateTime<=endDate))
    workingCopy$temp[thisKitID & dateRange] <- reprocessCSV$StudyIdentifier[[pIndex]]
    workingCopy$NYHAClass[thisKitID & dateRange] <- reprocessCSV$NYHA[[pIndex]]
  }
}
# drop rows where temp is now only an NA
workingCopy <- workingCopy[!is.na(workingCopy$temp), ]
# drop the temp column (moving to StudyIdentifier)
workingCopy$StudyIdentifier <- workingCopy$temp
workingCopy$temp <- NULL

# save as a different variable name
raghadParticipantData.df <- workingCopy

# clean up
rm(workingCopy,reprocessfilePath,reprocessCSV,pIndex,thisKitID,startDate,endDate,dateRange)
