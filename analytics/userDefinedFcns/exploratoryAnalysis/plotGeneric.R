### Plot Gamma Distribution (should auto plot state distributions of HMMs stored in hmm.activity)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","dplyr","tidyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)


library(ggplot2)
library(ggthemes)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)


sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","improvedGammaDist.R", #N.B. this is always relative to R's cwd (not the script)
             sep=""))
rm(sourceDir)

#######################################################################


plotX <- function(data.clean,CONSTANTS)
{
  # Constants ####
 
  LABEL <- list(DATETIME=list(), STEPS=list())
  FORMAT <- list(DATETIME=list(), STEPS=list())
  
  LABEL$STEPS$perMinute <- "Step Count [Steps/minute]"
  VISUAL_SCALE_COMP <- 125
  FORMAT$STEPS$perMinute.limits <- c(CONSTANTS$UNSCALEDMIN.STEPS,CONSTANTS$UNSCALEDMAX.STEPS-VISUAL_SCALE_COMP)
  
  LABEL$DATETIME$full <- "Study Day + Time [Day Hour]"
  FORMAT$DATETIME$full <- "%j %R" # day of year (000-366) & decimal hour (0-23) & decimal minute (00-59)
  
  LABEL$DATETIME$hour <- "Study Time [Hour]"
  FORMAT$DATETIME$hour <- "%H" # decimal hour (0-23)
  
  LABEL$DATETIME$day <- "(start of) Normalized Study Day [Day]"
  FORMAT$DATETIME$day <- "%j" # day of year (000-366)
  
  DAYS_TO_KEEP <- 16
  data.orig <- data.clean

  
  ######################################## PLOTS

  # Day by Day by Patient Plot ####
  # prep
  id <- "DayByDayByPatient"
  title <- "Step Time Series"
  plotData <- data.orig %>% filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  # extract time only
  timeColLabel <- 'Time'  # !!! N.B. fix below in aes if this changes
  newTimeCol <- plotData$DateTime
  year(newTimeCol) <- year(CONSTANTS$NORMALIZED_STUDY_DAY_START)
  yday(newTimeCol) <- yday(CONSTANTS$NORMALIZED_STUDY_DAY_START)
  plotData[timeColLabel] <- newTimeCol
  # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    fullTitle <- paste(id,":",title," | FC",cls,sep="")
    data.plotSubset <- plotData %>% filter(NYHAClass %in% c(cls)) # keep only this NYHA class for plot
    plt <- ggplot(data=data.plotSubset,
                  aes(x=Time, y=Steps/CONSTANTS$RESCALEFACTOR.STEPS, colour=StudyIdentifier)) +
      geom_line(alpha=0.8) +
      ggtitle(fullTitle) +
      scale_y_continuous(LABEL$STEPS$perMinute, 
                         limits=FORMAT$STEPS$perMinute.limits) +
      scale_x_datetime(LABEL$DATETIME$hour,
                       date_labels=FORMAT$DATETIME$hour,
                       date_breaks="6 hour") +
      facet_wrap(~yday(DateTime)) +
      theme_tufte() +
      theme(legend.position = "none")
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt,fullTitle)
  }
  
  
  # Day by Day Plot ####
  # prep
  id <- "DayByDay"
  title <- "Step Time Series"
  plotData <- data.orig %>% filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  # extract time only
  timeColLabel <- 'Time'  # !!! N.B. fix below in aes if this changes
  newTimeCol <- plotData$DateTime
  year(newTimeCol) <- year(CONSTANTS$NORMALIZED_STUDY_DAY_START)
  yday(newTimeCol) <- yday(CONSTANTS$NORMALIZED_STUDY_DAY_START)
  plotData[timeColLabel] <- newTimeCol
  # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    fullTitle <- paste(id,":",title," | FC",cls,sep="")
    data.plotSubset <- plotData %>% filter(NYHAClass %in% c(cls)) # keep only this NYHA class for plot
    plt <- ggplot(data=data.plotSubset,
                  aes(x=Time, y=Steps/CONSTANTS$RESCALEFACTOR.STEPS, colour=StudyIdentifier)) +
      geom_line(alpha=0.8) +
      ggtitle(fullTitle) +
      scale_y_continuous(LABEL$STEPS$perMinute, 
                         limits=FORMAT$STEPS$perMinute.limits) +
      scale_x_datetime(LABEL$DATETIME$hour,
                       date_labels=FORMAT$DATETIME$hour,
                       date_breaks="6 hour") +
      facet_wrap(~yday(DateTime)) +
      theme_tufte() +
      theme(legend.position = "none")
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt,fullTitle)
  }
  
    
  # Hours Plot ####
    # prep
  id <- "Hours"
  title <- "Step Time Series"
  plotData <- data.orig %>% filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
    # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    fullTitle <- paste(id,":",title," | FC",cls,sep="")
    data.plotSubset <- plotData %>% filter(NYHAClass %in% c(cls)) # keep only this NYHA class for plot
    # generate basic plot
    plt <- generateBasicTimeStepPlot(plotData=plotData,
                                     id=id,
                                     title=title,
                                     LABEL_CONSTS=LABEL,
                                     FORMAT_CONSTS=FORMAT,
                                     CONSTANT_CONSTS=CONSTANTS,
                                     nyhaClass=cls)
    # modify it to scale time axis by hours
    plt <- plt +  scale_x_datetime(LABEL$DATETIME$hour,
                                date_labels=FORMAT$DATETIME$hour,
                                date_breaks="6 hour")
    
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt,fullTitle)
  }
  
  # Days Plot ####
    # prep
  id <- "AllDays"
  title <- "Step Time Series"
  plotData <- data.orig
    # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    # generate basic plot
    plt <- generateBasicTimeStepPlot(plotData=plotData,
                                     id=id,
                                     title=title,
                                     LABEL_CONSTS=LABEL,
                                     FORMAT_CONSTS=FORMAT,
                                     CONSTANT_CONSTS=CONSTANTS,
                                     nyhaClass=cls)
    # modify it to scale time axis by days
    plt <- plt +  scale_x_datetime(LABEL_CONSTS$DATETIME$day,
                                   date_labels=FORMAT_CONSTS$DATETIME$day,
                                   date_breaks="1 day")

    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt)
  }
}

# plots basic time step plot. if no nyhaClass is specified then will plot all classes
generateBasicTimeStepPlot <- function(plotData, 
                                      id=Sys.time(),
                                      title="Plot", 
                                      LABEL_CONSTS, 
                                      FORMAT_CONSTS, 
                                      CONSTANT_CONSTS,
                                      nyhaClass)
{
  # since cls is considered optional, 
  if(!missing(nyhaClass))
  {
    plotData <- plotData %>% filter(NYHAClass %in% c(nyhaClass))  # keep only this NYHA class for plot
  }
  else
  {
    nyhaClass = "-all"
  }
  fullTitle <- paste(id,":",title," | FC",nyhaClass,sep="")
  lineTransparency = 0.8
  plt <- ggplot(data=plotData,
                aes(x=DateTime,
                    y=Steps/CONSTANT_CONSTS$RESCALEFACTOR.STEPS,
                    colour=StudyIdentifier)) +
    geom_line(alpha=lineTransparency) +
    ggtitle(fullTitle) +
    scale_y_continuous(LABEL_CONSTS$STEPS$perMinute, 
                       limits=FORMAT_CONSTS$STEPS$perMinute.limits) +
    theme_tufte() +
    theme(legend.position = "none")
  return(plt)
}

# saves plot in directory specified in saveDir (relative to cwd) if possible (otherwise to cwd)
savePNGPlot <- function(plt,width=1920,height=1080,saveDir="plots/")
{
  ext <- ".png"
  cleanPlotTitle <- gsub('[^a-zA-Z\\(\\)\\,\\-\\_\\+\\=[:space:]]', '-', plt$labels$title)
  success <- createDirectoryIfNotExists(saveDir)
  if(!success)
  {
    saveDir="" # save to cwd instead
    warn("Couldn't save to designated saveDir, saving to cwd instead (see w/ getwd())")
  }
  png(file=paste(saveDir,cleanPlotTitle,ext,sep=""),
      width=width,
      height=height)  # setup print device
  print(plt)  # this prints to device
  dev.off() # turn off print device
  cat("\nprinted PNG plot with name \"", cleanPlotTitle,"\"",sep="")
}

# creates directory in cwd if it doesn't already exist
createDirectoryIfNotExists <- function(subDir)
{
  mainDir <- getwd()
  fp <- file.path(mainDir, subDir)
  if(!dir.exists(fp))
  {
    dir.create(fp)
  }
  return(dir.exists(fp))
  
}


# # takes dataframes embedded in list and combines into a single dataframe (with a new column indicating keys)
# # N.B. this combining strips any potential attributes in the listed dataframes. Function will warn user, warning
# # can be suppressed by setting ack_StripsDFAttribute to TRUE
# listToDataFrame <- function(L,colLabel,ack_StripsDFAttribute = FALSE)
# {
#   if(!missing(colLabel))
#   {
#     for(i in 1:length(names(L)))
#     {
#       # mutate data frame to add a column labelling dataframe as belonging to this item in list
#       # the column generated should have the named with the value of colName (this is what !! xxx := is for) 
#       L[[i]] <- L[[i]] %>% dplyr::mutate(!!colLabel := names(L)[i])
#     }
#   }
#    
#   df <- rbind.fill(L)
#   
#   if(!ack_StripsDFAttribute)
#   {
#     warning('N.B. any \'attributes\' set for listed (sub)-dataframes were stripped away during merge process (i.e. returned dataframe contains no \'attributes\')')
#   }
#   
#   return(df) 
# }