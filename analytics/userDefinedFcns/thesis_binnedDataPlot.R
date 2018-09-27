### Plot 6 hour binned data

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(glue)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("hmm_common",
             "removeInvalidFileNameChars",
             "savePlot",
             "exploratoryAnalysis/plotGeneric")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
#rm(srcCreateFcn) #remove the extra unneeded variables

#### START

DAYS_IN_TWO_WEEKS <- 14

#### Helper Fcn #####
plotDayByDayData <- function(df,
                             cls="")
{
  plotData <- df
  
  # Day by Day by Patient Plot ####
  # prep
  id <- "DayByDayByPatient"
  title <- "Step Time Series"
  # plot
  graphTitle <- paste(id,":",title,sep="")
  TITLE_SEP = " | "
  
  for(pat in unique(plotData$StudyIdentifier))
  {
    data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
    
    tzOffset <- - 4
    hoursInDay <- 24
    data.plotSubset$XAxis <- ((hour(data.plotSubset$Time) - tzOffset) / hoursInDay) %% 1
    data.plotSubset$XAxis <- data.plotSubset$Day + data.plotSubset$XAxis
    if(nrow(data.plotSubset) > 0)
    {
      patientGraphTitle <- paste(graphTitle," | Pt",pat,sep="")
      fullTitle <- paste(patientGraphTitle,TITLE_SEP,"FC",cls,sep="")
      plt <- ggplot(data=data.plotSubset) +
                    ggtitle(fullTitle) +
                    theme_tufte() +
                    xlab('Study Day (relative to onboarding)') +
                    ylab('Step Count [Steps/6 hours]') +
                    theme(legend.position = "none",
                          text = element_text(size=14),
                          axis.text = element_text(size=14))
      #plt <- plt + geom_line(aes(x=XAxis,
      #                           y=Steps))
      plt <- plt + geom_col(aes(x=XAxis,
                                y=Steps,
                                color=StudyIdentifier,
                                fill=StudyIdentifier))

      breakSeq <- seq(0,DAYS_IN_TWO_WEEKS,1)
      plt <- plt + scale_x_continuous(breaks=breakSeq,
                                       labels=breakSeq+1
                                       #limits
                                       )
      
      fName <- glue("{id}-{title} - Pt{pat} - FC{cls}.png")
      fPrefix=glue("plotsSimon/6Hour_PatientDayByDay/NYHA{cls}/")
      # show the plot
      print(plt)
      # save plot
      savePlot(fileName=fName,
               filePrefix=fPrefix,
               plot=plt,
               isGGPlot=FALSE)
    }
    else
    {
      cat('\nNo data for patient ',pat,'. Skipped printing & plotting.',sep="")
    }
  }
}

#### MAIN ####

#load up combined data file (with combinedData)
dataSetColname <- 'Dataset'
simonDataSetID <- 'S'

fitbitDf <- combinedData %>% 
  filter((!!rlang::sym(dataSetColname)) == simonDataSetID) %>% filter(StudyIdentifier != 'Canary')

daysInTwoWeeks <- 14
plotData <- fitbitDf %>% dplyr::filter(Day <= daysInTwoWeeks)

mergeCols <- c('StudyIdentifier','DateTime')
commonColsKeep <- c(mergeCols,'Steps','HeartRate')
trainingData <- plotData[,names(plotData) %in% c(commonColsKeep,'RoundDownNYHAClass')]
trainingData$NYHAClass <- trainingData$RoundDownNYHAClass
trainingData$RoundDownNYHAClass <- NULL

trainingDataDayTime <- plotData[,names(plotData) %in% c(mergeCols,'Day','Time')]


for(class in unique(trainingData$NYHAClass))
{
  if(!is.na(class))
  {
    # get class specific data
    classTrainingData <- trainingData %>% filter(NYHAClass == class)
    
    ## START BINNING ##
    interval <- "6 hour"
    finalTrainData <- binData(classTrainingData, interval)
    finalTrainData$DateTime <- as.POSIXct(finalTrainData$DateTime)
    nyhaClassVector <- trainingData$NYHAClass
    finalTrainData$NYHAClass <- factor(x=finalTrainData$NYHAClass,
                                       levels=seq_along(levels(nyhaClassVector)),
                                       labels=levels(nyhaClassVector))
    finalTrainDataWithDayAndTime <- merge(finalTrainData,
                                          trainingDataDayTime,
                                          by=mergeCols,
                                          all.x=TRUE)
    ## END BINNING ##
    
    ## IMPORTANT ## REDEFINE THE FORMATTING CONSTANTS! THIS IS BAD PRACTICE, but quick and dirty
    
    plotDayByDayData(df=finalTrainDataWithDayAndTime,
                     cls=class)
  }
  
}








