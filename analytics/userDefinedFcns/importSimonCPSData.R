### Import Script for Fitbit CPS Data (Simon Bromberg's Thesis)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","importExcelSheets.R",
				sep=""))
rm(sourceDir)

#local functions

importSimonCPSData <- function(VIEW=FALSE,USE.CHOOSEDIALOG=FALSE) 
{
  
  ## Specify File Location
  if(USE.CHOOSEDIALOG)
  {
    fileName <- NULL
    basePath <- NULL
    baseToFilePath <- NULL
    dialogTitle <- "Select CPS Data File"
  }
  else
  {
    basePath <- getwd() #if local
    #basePath <- "" #for network drive... tbd
    baseToFilePath <- "data(gitignored)/" #again if needed
    fileName <- "structure_CPSDummyData.xlsx"
    dialogTitle <- NULL
  }
  ## Specify Important Excel Import Parameters
  initialRowsToSkip <- 3 #skip the first two (Simon left hidden data there...) +  1 (since we specify col_names)
  excludeSheets <- c("Sheet2","Minute conv","JFB-LabelInvestigation")
  #date helper function for later
  dateInputFormat = "%m/%d/%Y"
  as.DateWithSimonsFormat <- function(X,dIF=dateInputFormat,...){
    return(as.Date(X,dIF))
  }
  columnNames = c("StudyIdentifier", #Field
                  "ID", #ID
                  "CPSDate", #Date of Test
                  "Sex", #Sex
                  "Age", #Age (Years)
                  "Height", #Height (cm)
                  "Weight", #Weight (kg)
                  "SBP.Resting", #Rest SBP -> Systolic Blood Pressure
                  "DBP.Resting", #Rest DBP -> Diastolic Blood Pressure
                  "HR.Resting", #Rest HR -> Resting Heart Rate
                  "O2Sat.Resting", #Rest O2 Sat -> Resting Oxygen Saturation
                  "FEV.Resting", #Rest FEV1 -> Forced Expiration Volume 1
                  "FEV.Resting.Percentage", #Rest FEV1 % -> as above
                  "FVC.Resting", #Rest FVC -> Forced Vital Capacity
                  "FVC.Resting.Percentage", #Rest FVC % -> as above
                  "SBP", #SBP -> Systolic Blood Pressure
                  "DBP", #DBP -> Diastolic Blood Pressure
                  "HR", #Heart rate
                  "HR.1min", #1 min rec HR
                  "HR.1minDrop", #Drop in 1 min
                  "O2Sat", #O2 Sat -> Oxygen Saturation
                  "Duration", #Time (fractional minutes)
                  "Watts", #Watts
                  "Watts.Predicted.Percentage", #%predicted watts
                  "TestEnd.Reason", #Termination -> reason
                  "TestEnd.Symptom", #Symptoms Y/N -> Note: it's not Y/N
                  "VO2.perBodyWeight", #VO2 (ml/kg/min)
                  "VO2.Predicted.Relative", #Predicted rel vo2
                  "VO2.Predicted.Relative.Percentage", #% Predicted rel vo2
                  "VO2", #VO2 (L/min)
                  "VO2.Predicted.Absolute", #Predicted abs vo2
                  "VO2.Predicted.Absolute.Percentage", #% Predicted abs vo2
                  "VE.Peak", #Peak VE
                  "VCO2.Peak", #Peak VCO2 (L/min)
                  "VEperVCO2.Peak", #VE/VCO2 Peak
                  "VEperVCO2.atAT", #VE/VCO2 @ AT
                  "AT", #AT (ml/kg/min)
                  "VO2.Peak.Measured.Percentage", #% meas. Peak VO2
                  "VO2.Peak.Predicted.Percentage", #% pred. Peak VO2
                  "RER.Peak", #Peak RER -> Resting Expiratory Rate
                  "RPEper20.Peak", #Peak RPE/20 -> Perceived Exertion Rate /20 NOTE: not measured for most
                  "PETCO2.Peak", #Peak PETCO2 -> NOTE: not measured for most
                  "OUES", #OUES -> NOTE: not measured for most
                  "TotalRiskScore", #Total Risk Score -> Risk of what?
                  "empty1", #Empty Column (included because import function will complain otherwise)
                  "empty2") #Empty Column w/ 1 spurious entry  (included because import function will complain otherwise)
  
  columnTypes = c("text", #Field
                  "numeric", #ID
                  "text", #Date of Test
                  "text", #Sex
                  "numeric", #Age (Years)
                  "numeric", #Height (cm)
                  "numeric", #Weight (kg)
                  "numeric", #Rest SBP -> Systolic Blood Pressure
                  "numeric", #Rest DBP -> Diastolic Blood Pressure
                  "numeric", #Rest HR -> Resting Heart Rate
                  "numeric", #Rest O2 Sat -> Resting Oxygen Saturation
                  "numeric", #Rest FEV1 -> Forced Expiration Volume 1
                  "numeric", #Rest FEV1 % -> as above
                  "numeric", #Rest FVC -> Forced Vital Capacity
                  "numeric", #Rest FVC % -> as above
                  "numeric", #SBP -> Systolic Blood Pressure
                  "numeric", #DBP -> Diastolic Blood Pressure
                  "numeric", #Heart rate
                  "numeric", #1 min rec HR
                  "numeric", #Drop in 1 min
                  "numeric", #O2 Sat -> Oxygen Saturation
                  "numeric", #Time (fractional minutes)
                  "numeric", #Watts
                  "numeric", #%predicted watts
                  "text", #Termination -> reason
                  "text", #Symptoms Y/N -> Note: it's not Y/N
                  "numeric", #VO2 (ml/kg/min)
                  "numeric", #Predicted rel vo2
                  "numeric", #% Predicted rel vo2
                  "numeric", #VO2 (L/min)
                  "numeric", #Predicted abs vo2
                  "numeric", #% Predicted abs vo2
                  "numeric", #Peak VE
                  "numeric", #Peak VCO2 (L/min)
                  "numeric", #VE/VCO2 Peak
                  "numeric", #VE/VCO2 @ AT
                  "numeric", #AT (ml/kg/min)
                  "numeric", #% meas. Peak VO2
                  "numeric", #% pred. Peak VO2
                  "numeric", #Peak RER -> Resting Expiratory Rate
                  "numeric", #Peak RPE/20 -> Perceived Exertion Rate /20 NOTE: not measured for most
                  "numeric", #Peak PETCO2 -> NOTE: not measured for most
                  "numeric", #OUES -> NOTE: not measured for most
                  "numeric", #Total Risk Score -> Risk of what?
                  "skip", #Empty Column (included because import function will complain otherwise)
                  "skip") #Empty Column w/ 1 spurious entry  (included because import function will complain otherwise)
  
  names(columnTypes) <- columnNames #key columnType values to columnName keys (not really used but)
  
  columnTypeConversionFcns = c(as.character, #Field
                               as.numeric, #ID
                               as.DateWithSimonsFormat, #Date of Test
                               as.factor, #Sex
                               as.numeric, #Age (Years)
                               as.numeric, #Height (cm)
                               as.numeric, #Weight (kg)
                               as.numeric, #Rest SBP -> Systolic Blood Pressure
                               as.numeric, #Rest DBP -> Diastolic Blood Pressure
                               as.numeric, #Rest HR -> Resting Heart Rate
                               as.numeric, #Rest O2 Sat -> Resting Oxygen Saturation
                               as.numeric, #Rest FEV1 -> Forced Expiration Volume 1
                               as.numeric, #Rest FEV1 % -> as above
                               as.numeric, #Rest FVC -> Forced Vital Capacity
                               as.numeric, #Rest FVC % -> as above
                               as.numeric, #SBP -> Systolic Blood Pressure
                               as.numeric, #DBP -> Diastolic Blood Pressure
                               as.numeric, #Heart rate
                               as.numeric, #1 min rec HR
                               as.numeric, #Drop in 1 min
                               as.numeric, #O2 Sat -> Oxygen Saturation
                               as.numeric, #Time (fractional minutes)
                               as.numeric, #Watts
                               as.numeric, #%predicted watts
                               as.factor, #Termination -> reason
                               as.factor, #Symptoms Y/N -> Note: it's not Y/N
                               as.numeric, #VO2 (ml/kg/min)
                               as.numeric, #Predicted rel vo2
                               as.numeric, #% Predicted rel vo2
                               as.numeric, #VO2 (L/min)
                               as.numeric, #Predicted abs vo2
                               as.numeric, #% Predicted abs vo2
                               as.numeric, #Peak VE
                               as.numeric, #Peak VCO2 (L/min)
                               as.numeric, #VE/VCO2 Peak
                               as.numeric, #VE/VCO2 @ AT
                               as.numeric, #AT (ml/kg/min)
                               as.numeric, #% meas. Peak VO2
                               as.numeric, #% pred. Peak VO2
                               as.numeric, #Peak RER -> Resting Expiratory Rate
                               as.numeric, #Peak RPE/20 -> Perceived Exertion Rate /20 NOTE: not measured for most
                               as.numeric, #Peak PETCO2 -> NOTE: not measured for most
                               as.numeric, #OUES -> NOTE: not measured for most
                               as.numeric, #Total Risk Score -> Risk of what?
                               as.character, #Empty Column (included because import function will complain otherwise)
                               as.character) #Empty Column w/ 1 spurious entry  (included because import function will complain otherwise)
  names(columnTypeConversionFcns) <- columnNames #key columnType values to columnName keys
  ## END SPECIFY IMPORTANT EXCEL IMPORT PARAMETERS
  
  ## Perform Data Import
  
  importedData.sheets <- importExcelSheets(fileName,basePath,baseToFilePath,
                                           skip = initialRowsToSkip,
                                           excludeSheets = excludeSheets,
                                           col_types = columnTypes,
                                           col_names = columnNames,
                                           col_conversionFcns = columnTypeConversionFcns,
                                           CHOOSE.DIALOGTITLE = dialogTitle)
    
  importedData <- importedData.sheets
  
  return(importedData)
}
