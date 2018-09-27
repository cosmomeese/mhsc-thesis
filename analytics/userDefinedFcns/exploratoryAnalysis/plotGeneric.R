### Plot Gamma Distribution (should auto plot state distributions of HMMs stored in hmm.activity)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","glue","stringr","lubridate","plyr","dplyr","tidyr")
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
library(glue)
library(stringr)


## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("improvedGammaDist",
             "hmm_common",
             "analyzeData_common",
             "removeInvalidFileNameChars")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

#######################################################################
#### INSTRUCTIONS ####

#to use, first pass m_cData through hmm_convertRawData(m_cData, fitbitDownload=FALSE), use output as data.clean
#then load hmm_common for constants

#### WARN THIS REALLY NEEDS REFACTORING ####

LABEL <- list(DATETIME=list(), HEARTRATE=list(), STEPS=list())
FORMAT <- list(DATETIME=list(), HEARTRATE=list(), STEPS=list())

# N.B. these constants should be 
# (the upper case version) of the keywords used in generateXYZPlot functions
LABEL$STEPS$perMinute <- "Step Count [Steps/minute]"
VISUAL_SCALE_COMP <- 125
FORMAT$STEPS$perMinute.limits <- c(CONSTANTS$UNSCALEDMIN.STEPS,CONSTANTS$UNSCALEDMAX.STEPS-VISUAL_SCALE_COMP)

LABEL$HEARTRATE$perMinute <- "Heart Rate [bpm]"
HRVISUAL_SCALE_COMP <- 0
FORMAT$HEARTRATE$perMinute.limits <- c(CONSTANTS$UNSCALEDMIN.HEARTRATE,CONSTANTS$UNSCALEDMAX.HEARTRATE-HRVISUAL_SCALE_COMP)

LABEL$DATETIME$full <- "Study Day + Time [Day Hour]"
FORMAT$DATETIME$full <- "%j %R" # day of year (000-366) & decimal hour (0-23) & decimal minute (00-59)

LABEL$DATETIME$hour <- "Study Time [Hour]"
FORMAT$DATETIME$hour <- "%H" # decimal hour (0-23)

LABEL$DATETIME$day <- "(start of) Normalized Study Day [Day]"
FORMAT$DATETIME$day <- "%j" # day of year (000-366)

TIME_COL_LABEL <- 'Time'
DAYS_TO_KEEP <- 16


plotX <- function(data.clean,CONSTS=CONSTANTS,withHeartRate=FALSE,showPlot=FALSE)
{
  bypassForThesisFlag <- TRUE
  
  
  freqSpecPlot <- FALSE # NOT DONE YET
  stackedDayByDay <- !bypassForThesisFlag
  patientDayByDay <- TRUE #only to keep for thesis
  normalDayByDay <- !bypassForThesisFlag
  byHoursPlot <- !bypassForThesisFlag
  byDaysPlot <- !bypassForThesisFlag
  
  # Constants ####
  
  data.orig <- data.clean
  
  if(withHeartRate)
  {
    title <- "Step + HR Time Series"
  }
  else
  {
    title <- "Step Time Series"
  }
  
  ######################################## PLOTS
  
  if(freqSpecPlot)
  {
    # Frequency Spectrum Day by Day by Patient Plot ####
    # prep
    id <- "Freq-DayByDayByPatient"
    #title <- "Step Time Series"
    plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTS$NORMALIZED_STUDY_DAY_START))
    
    # plot
    for(pat in levels(plotData$StudyIdentifier))
    {
      data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
      if(nrow(data.plotSubset) > 0)
      {
        cls <- getSingleFactorFromVector(data.plotSubset$NYHAClass, asCharacter=TRUE)
        title.patient <- paste(title," | ",pat," | Class ",cls,sep="")
        
        del <- 0.1
        x.spec <- spectrum(data.plotSubset$Steps,log="no",span=10,plot=FALSE)
        spx <- x.spec$freq/del
        spy <- 2*x.spec$spec
        graphics::plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
        graphics::title(main=title.patient,
                        xlab="Frequency",
                        ylabel="Spectral Density")
        #### WARN HERE!!! THIS PLOT IS NOT YET WORKING ####
      }
      else
      {
        cat('\nNo data for patient ',pat,'. Skipped printing & plotting.',sep="")
      }
    }
  }
  else
  {
    warning("Not finished programming spectral plots")
  }

  if(stackedDayByDay)
  {
    # Stacked Day by Day by Patient Plot ####
    # prep
    id <- "DayByDayByPatient(Stacked)"
    #title <- "Step Time Series"
    plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTS$NORMALIZED_STUDY_DAY_START))
    
    # plot
    for(pat in levels(plotData$StudyIdentifier))
    {
      data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
      if(nrow(data.plotSubset) > 0)
      {
        cls <- getSingleFactorFromVector(data.plotSubset$NYHAClass, asCharacter=TRUE)
        title.patient <- paste(title," | Pt",pat,sep="")
        
        mappingList <- list()
        
        mappingList[['STEPS']] = aes(x=TIME_PLACEHOLDER,
                                     y=Steps/CONSTANT_CONSTS$RESCALEFACTOR.STEPS,
                                     fill=as.factor(yday(DateTime)),
                                     colour=as.factor(yday(DateTime)))
        
        if(withHeartRate)
        {
          mappingList[['HEARTRATE']] = aes(x=TIME_PLACEHOLDER,
                                           y=HeartRate/CONSTANT_CONSTS$RESCALEFACTOR.HEARTRATE,
                                           #fill=as.factor(yday(DateTime)),
                                           colour=as.factor(yday(DateTime)))
        }
        
        plt <- generateTimeHeartStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                         plotData=data.plotSubset,
                                         id=id,
                                         mappingList=mappingList,
                                         title=title.patient,
                                         LABEL_CONSTS=LABEL,
                                         FORMAT_CONSTS=FORMAT,
                                         CONSTANT_CONSTS=CONSTS,
                                         nyhaClass=cls)
        
        
        # modify it to scale time axis by hours
        plt <- plt + 
          scale_x_datetime(LABEL$DATETIME$hour,
                           date_labels=FORMAT$DATETIME$hour,
                           date_breaks="6 hour")
        
        # show the plot
        if(showPlot)
        {
          print(plt) # can get away with this since we're using ggplot, if using regular plot need to print first
        }
        
        # save plot
        savePNGPlot(plt)
      }
      else
      {
        cat('\nNo data for patient ',pat,'. Skipped printing & plotting.',sep="")
      }
    }
  }
  
  if(patientDayByDay)
  {
    #### RESET mappingList ####
    mappingList <- list(STEPS=NA)
    if(withHeartRate)
    {
      mappingList[['HEARTRATE']] <- NA
    }
    
    # Day by Day by Patient Plot ####
    # prep
    id <- "DayByDayByPatient"
    #title <- "Step Time Series"
    plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTS$NORMALIZED_STUDY_DAY_START))
    
    # plot
    for(pat in levels(plotData$StudyIdentifier))
    {
      data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
      if(nrow(data.plotSubset) > 0)
      {
        cls <- getSingleFactorFromVector(data.plotSubset$NYHAClass, asCharacter=TRUE)
        title.patient <- paste(title," | Pt",pat,sep="")
        
        data.plotSubsetWithDayLabel <- data.plotSubset
        data.plotSubsetWithDayLabel$DayLabel <- factor(yday(data.plotSubset$DateTime),
                                                       labels=paste0("Day ",unique(yday(data.plotSubset$DateTime))))
          
        
        plt <- generateTimeHeartStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                         plotData=data.plotSubsetWithDayLabel,
                                         id=id,
                                         mappingList=mappingList,
                                         title=title.patient,
                                         LABEL_CONSTS=LABEL,
                                         FORMAT_CONSTS=FORMAT,
                                         CONSTANT_CONSTS=CONSTS,
                                         nyhaClass=cls)
        
        # modify it to scale time axis by hours
        plt <- plt + 
          scale_x_datetime(LABEL$DATETIME$hour,
                           date_labels=FORMAT$DATETIME$hour,
                           date_breaks="6 hour")
        
        # make sure there are more than 1 day (for facet to work)
        if(length(unique(yday(data.plotSubset$DateTime))) > 1)
        {
          plt <- plt + facet_wrap(~DayLabel)
        }
        
        # show the plot
        print(plt)
        # save plot
        savePNGPlot(plt)
      }
      else
      {
        cat('\nNo data for patient ',pat,'. Skipped printing & plotting.',sep="")
      }
    }
  }
  
  if(normalDayByDay)
  {
    #### RESET mappingList ####
    mappingList <- list(STEPS=NA)
    if(withHeartRate)
    {
      mappingList[['HEARTRATE']] <- NA
    }
    
    # Day by Day Plot ####
    # prep
    id <- "DayByDay"
    #title <- "Step Time Series"
    plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTS$NORMALIZED_STUDY_DAY_START))
    
    # plot
    for(cls in CONSTS$NYHA_CLASS_VEC)
    {
      plt <- generateTimeHeartStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                       plotData=plotData,
                                       id=id,
                                       mappingList=mappingList,
                                       title=title,
                                       LABEL_CONSTS=LABEL,
                                       FORMAT_CONSTS=FORMAT,
                                       CONSTANT_CONSTS=CONSTS,
                                       nyhaClass=cls)
      
      # modify it to scale time axis by hours
      plt <- plt + 
        scale_x_datetime(LABEL$DATETIME$hour,
                         date_labels=FORMAT$DATETIME$hour,
                         date_breaks="6 hour")
      
      # make sure there are more than 1 day (for facet to work)
      if(length(unique(yday(plotData$DateTime))) > 1)
      {
        plt <- plt + facet_wrap(~yday(DateTime))
        
      }
      
      if(nrow(plt$data) > 0)
      {
        # show the plot
        print(plt)
        # save plot
        savePNGPlot(plt)
      }
      else
      {
        cat("\nSkipped printing ", id, " plot for class: ", cls, " (no data for class)\n")
      }
      
    }
  }
  
  
  if(byHoursPlot)
  {
    #### RESET mappingList ####
    mappingList <- list(STEPS=NA)
    if(withHeartRate)
    {
      mappingList[['HEARTRATE']] <- NA
    }
    
    # Hours Plot ####
    # prep
    id <- "Hours"
    #title <- "Step Time Series"
    plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTS$NORMALIZED_STUDY_DAY_START))
    # plot
    for(cls in CONSTS$NYHA_CLASS_VEC)
    {
      # generate basic plot
      plt <- generateDateTimeHeartStepPlot(plotData=plotData,
                                           id=id,
                                           mappingList=mappingList,
                                           title=title,
                                           LABEL_CONSTS=LABEL,
                                           FORMAT_CONSTS=FORMAT,
                                           CONSTANT_CONSTS=CONSTS,
                                           nyhaClass=cls)
      
      # modify it to scale time axis by hours
      plt <- plt + scale_x_datetime(LABEL$DATETIME$hour,
                                    date_labels=FORMAT$DATETIME$hour,
                                    date_breaks="6 hour")
      
      if(nrow(plt$data) > 0)
      {
        # show the plot
        print(plt)
        # save plot
        savePNGPlot(plt)
      }
      else
      {
        cat("\nSkipped printing ", id, " plot for class: ", cls, " (no data for class)\n")
      }
    }
  }
  
  if(byDaysPlot)
  {
    #### RESET mappingList ####
    mappingList <- list(STEPS=NA)
    if(withHeartRate)
    {
      mappingList[['HEARTRATE']] <- NA
    }
    
    # Days Plot ####
    # prep
    id <- "AllDays"
    #title <- "Step Time Series"
    plotData <- data.orig
    # plot
    for(cls in CONSTS$NYHA_CLASS_VEC)
    {
      # generate basic plot
      plt <- generateDateTimeHeartStepPlot(plotData=plotData,
                                           id=id,
                                           mappingList=mappingList,
                                           title=title,
                                           LABEL_CONSTS=LABEL,
                                           FORMAT_CONSTS=FORMAT,
                                           CONSTANT_CONSTS=CONSTS,
                                           nyhaClass=cls)
      
      # modify it to scale time axis by days
      plt <- plt +  scale_x_datetime(LABEL$DATETIME$day,
                                     date_labels=FORMAT$DATETIME$day,
                                     date_breaks="1 day")
      
      if(nrow(plt$data) > 0)
      {
        # show the plot
        print(plt)
        # save plot
        savePNGPlot(plt)
      }
      else
      {
        cat("\nSkipped printing ", id, " plot for class: ", cls, " (no data for class)\n")
      }
    }
  }
  
  
}

# adds a column with label TIME_COL_LABEL to the DATA that contains only the time component of the column with sourceColumnlabel.
# the time column will have a date corresponding to the NORMALIZED_STUDY_DAY_START in CONSTANTS_CONSTS
addTimeColumn <- function(data,sourceColumnLabel,TIME_COL_LABEL,CONSTANT_CONSTS)
{
  # extract time only
  newTimeCol <- data[[sourceColumnLabel]]
  year(newTimeCol) <- lubridate::year(CONSTANT_CONSTS$NORMALIZED_STUDY_DAY_START)
  yday(newTimeCol) <- lubridate::yday(CONSTANT_CONSTS$NORMALIZED_STUDY_DAY_START)
  data[TIME_COL_LABEL] <- newTimeCol
  return(data)
}

# plots basic datetime heart rate + step plot using mapping.
# if no mapping is specified it will simply plot DateTime vs Steps according colored by StudyID
# if no nyhaClass (NULL or missing or '') is specified then will plot all classes
#     - if no nyhaClass = '' then will also not add a label to plot regarding NYHA class
#
# similar to generateTimeHeartStepPlot, see inputs of that too
generateDateTimeHeartStepPlot <- function(plotData,
                                          mappingList=list(),
                                          id=Sys.time(),
                                          title="Plot", 
                                          LABEL_CONSTS, 
                                          FORMAT_CONSTS, 
                                          CONSTANT_CONSTS,
                                          nyhaClass=NULL)
{
  STEP_MAP_NAME <- "STEPS"
  HEART_MAP_NAME <- "HEARTRATE"
  
  # since cls is considered optional,
  fullTitle <- paste(id,":",title,sep="")
  TITLE_SEP = " | "
  if(!((missing(nyhaClass) || is.null(nyhaClass)) || (nyhaClass %in% c('')) ))
  {
    plotData <- plotData %>% dplyr::filter(NYHAClass %in% c(nyhaClass))  # keep only this NYHA class for plot
    fullTitle <- paste(fullTitle,TITLE_SEP,"FC",nyhaClass,sep="")
  }
  else if(!is.null(nyhaClass) && nyhaClass == '')
  {
    # do nothing
  }
  else
  {
    fullTitle <- paste(fullTitle,"FC-all",sep=TITLE_SEP)
  }
  
  mappingList <- mappingHelperFcn(mappingList)
  
  if(any(is.na(mappingList)))
  {
    stop('invalid mapping list provided')
  }
  
  lineTransparency <- 0.6
  plt <- ggplot(data=plotData) +
         ggtitle(fullTitle) +
         theme_tufte() +
         theme(legend.position = "none",
               text = element_text(size=18),
               axis.text = element_text(size=18))
  
  if(length(mappingList) > 0)
  {
    mapNames <- names(mappingList)
    priMapName <- mapNames[[1]]
    axesName <- LABEL_CONSTS[[toupper(priMapName)]]$perMinute
    axesLimits <- FORMAT_CONSTS[[toupper(priMapName)]]$perMinute.limits
    
    if(length(mappingList) > 1)
    {
      secMapName <- mapNames[[2]]
      
      # get transform constant
      rescalePrefix <- "RESCALEFACTOR."
      transformConst <- CONSTANT_CONSTS[[paste0(rescalePrefix,toupper(secMapName))]] / CONSTANT_CONSTS[[paste0(rescalePrefix,toupper(priMapName))]]
      #transformConst <- 1
      
      # transform secondary mapping
      
      map <- mappingList[[secMapName]]
      map$y <- parse(text=paste0(deparse(map$y),
                                 "/",transformConst))[[1]]
      mappingList[[secMapName]] <- map
      
      # create secondary axis
      secAxesLabel <- LABEL_CONSTS[[toupper(secMapName)]]$perMinute
      # add axis & secondary
      plt <- plt + scale_y_continuous(axesName, 
                                      limits=axesLimits,
                                      sec.axis = sec_axis(~. * transformConst,
                                                          name = secAxesLabel)
                                     )
    }
    else
    {
      plt <- plt + scale_y_continuous(axesName, 
                                      limits=axesLimits
                                     )
    }
  }
  
  axesCount <- 0
  if(exists(STEP_MAP_NAME,mappingList))
  {
    axesCount <- axesCount + 1
    plt <- plt + geom_area(mapping=mappingList[[STEP_MAP_NAME]])
  }
  
  if(exists(HEART_MAP_NAME,mappingList))
  {
    axesCount <- axesCount + 1
    plt <- plt + geom_line(mapping=mappingList[[HEART_MAP_NAME]],
                           alpha=lineTransparency,
                           color="black")
  }
  
  return(plt)
}


# plots basic time heart rate step plot using mapping (i.e. as with generateDateTimeStepPlot but replaces datetime w/ time variable).
# N.B. will replace instances of TIME_PLACEHOLDER in mapping (i.e. specify as TIME_PLACEHOLDER with quotation marks) with the TIME_COL_LABEL specified
# i.e. if you specify in mapping that x=TIME_PLACEHOLDER it will plot time as the x value (so you can still use datetime if desired).
# N.B. generateDateTimeStepPlot performs faster if you don't need to plot actual time
# if no mapping is specified it will simply plot DateTime vs Steps according colored by StudyID
# if no nyhaClass is specified it will plot the no nyhaClass setting of generateDateTimeStepPlot
# mappingList should be a list of mappings desired ('heartrate','steps') 
# in the form list(STEPS=aes(...), HEARTRATE=aes(...))
# if you just want steps do list(step=aes(...)) or list(step=NA) if you don't have a mapping to specify
generateTimeHeartStepPlot <- function(TIME_COL_LABEL,
                                      plotData,
                                      mappingList=list(STEPS=NA,HEARTRATE=NA),
                                      id=Sys.time(),
                                      title="Plot", 
                                      LABEL_CONSTS, 
                                      FORMAT_CONSTS, 
                                      CONSTANT_CONSTS,
                                      nyhaClass=NULL)
{
  #KEY_WORD = "TIME_PLACEHOLDER"
  plotData.withTimeCol <- addTimeColumn(data = plotData,
                                        sourceColumnLabel = "DateTime",
                                        TIME_COL_LABEL = TIME_COL_LABEL,
                                        CONSTANT_CONSTS = CONSTANT_CONSTS)
  
  mappingList <- mappingHelperFcn(mappingList,
                                  TIME_COL_LABEL)
  
  plt <- generateDateTimeHeartStepPlot(plotData=plotData.withTimeCol,
                                       mappingList=mappingList,
                                       id=id,
                                       title=title, 
                                       LABEL_CONSTS=LABEL_CONSTS, 
                                       FORMAT_CONSTS=FORMAT_CONSTS, 
                                       CONSTANT_CONSTS=CONSTANT_CONSTS,
                                       nyhaClass=nyhaClass)
  return(plt)
}



# fill out missing ggplot aes mappings (if specified; i.e. 'step' & 'heart' not NA) for Step & Heart Rate
# i.e
# if no mapping list: error
# if list(step=..., ...) -> uses step
# if list(heartrate=..., ...) -> uses heart rate
# if list(step=NA,...) -> generates default step mapping
# if list(step=aes(...),...) -> uses specified step mapping
# if list(heartrate=NA,...) -> generates default heart rate mapping
# if list(heartrate=aes(...),...) -> uses specified heart rate mapping
# if list(step=...,heartrate=...) -> (order insensitive) uses both step & heart rate mapping
# if list() or any list not containing step or heart -> returns NA
#
# if TIME_COL_LABEL is supplied 
#    then function for x-axis will try to override the keyword TIME_PLACEHOLDER in mapping list with the provided TIME_COL_LABEL
# otherwise will use DateTime as x-axis
mappingHelperFcn <- function(mappingList,TIME_COL_LABEL)
{
  # CHECK INPUT VARIABLES
  if(missing(mappingList))
  {
    stop("Missing mapping list")
  }
  
  # CONSTANTS
  KEY_WORD = "TIME_PLACEHOLDER"
  STEP_MAP_NAME <- "STEPS" # mappingList name (key)
  HEART_MAP_NAME <- "HEARTRATE" # mappingList name (key)
  # assemble the list & apply names
  VALID_MAP_NAMES <- list("Steps","HeartRate") # values: dataframe column names & suffix (x) for CONSTANT_CONSTS$RESCALEFACTOR.x (must match)
  VALID_MAP_NAMES <- setNames(VALID_MAP_NAMES,c(STEP_MAP_NAME, HEART_MAP_NAME))
  atLeastOne <- FALSE
  
  # CHECK IF TIME_COL_LABEL is defined
  x <- quote(DateTime)
  if(!missing(TIME_COL_LABEL))
  {
    x <- parse(text=KEY_WORD)[[1]]
  }
  
  for(mapName in names(VALID_MAP_NAMES))
  {
    # if missing step mapping, using default
    if(exists(mapName,mappingList))
    {
      atLeastOne <- TRUE
      if(any(is.na(mappingList[[mapName]]))) #any should be redundant but...
      {
        y <- parse(text=paste0(VALID_MAP_NAMES[[mapName]],
                               "/CONSTANT_CONSTS$RESCALEFACTOR.",
                               mapName))[[1]]
        mappingList[[mapName]] <- aes_(x=x,
                                       y=y,
                                       colour=quote(StudyIdentifier))
        
        
        if(!(mapName %in% c(HEART_MAP_NAME))) # list of map names to not add fills for
        { # i.e. if mapName name is not the exclusion list, then add a fill
          mappingList[[mapName]]$fill <- quote(StudyIdentifier)
        }
        
        
      }
      
      if(!missing(TIME_COL_LABEL))
      {
        for(i in 1:length(mappingList[[mapName]]))
        {
          mappingList[[mapName]][[i]] <- recurseChangingKeywords(mappingList[[mapName]][[i]],
                                                                 REPLACE_WORD = KEY_WORD,
                                                                 NEW_WORD = TIME_COL_LABEL)
        }
      }
    }
  }
  
  if(!atLeastOne)
  {
    mappingList <- NA
  }
  
  return(mappingList)
}


# recursively change the keywords in mappedObject ('name' or 'call' object) changing the REPLACE_WORDs to NEW_WORDs
recurseChangingKeywords <- function(mappedObject,REPLACE_WORD,NEW_WORD)
{
  mappingClass = class(mappedObject)
  if("name" == mappingClass) # termination condition
  {
    mappedObject <- as.name(gsub(REPLACE_WORD, NEW_WORD, mappedObject))
  }
  else if("call" == mappingClass)
  {
    callList <- as.list(mappedObject)
    for(call_index in 1:length(callList))
    {
      callList[[call_index]] <- recurseChangingKeywords(mappedObject= callList[[call_index]],
                                                        REPLACE_WORD = REPLACE_WORD,
                                                        NEW_WORD = NEW_WORD)
    }
    mappedObject <- as.call(callList)
  }
  return(mappedObject)
}



# saves plot in directory specified in saveDir (relative to cwd) if possible (otherwise to cwd)
# N.B. print out plot before using savePNGPlot if using regular plot, no need if only using ggplot
savePNGPlot <- function(plt,width=1920,height=1080,saveDir="plots/",dpi=100,fast=TRUE)
{
  height_in <- height / dpi
  width_in <- width / dpi
  ext <- ".png"
  #cleanPlotTitle <- gsub('[^a-zA-Z0-9\\(\\)\\,\\-\\_\\+\\=[:space:]]', '-', plt$labels$title)
  cleanPlotTitle <- removeInvalidFileNameChars(plt$labels$title)
  success <- createDirectoryIfNotExists(saveDir)
  if(!success)
  {
    saveDir="" # save to cwd instead
    warn("Couldn't save to designated saveDir, saving to cwd instead (see w/ getwd())")
  }
  fullFileName <- paste(saveDir,cleanPlotTitle,ext,sep="")

  if(fast)
  {
    # (much) Faster version
    png(file=fullFileName,
        width=width,
        height=height)  # setup print device
    print(plt)  # this prints to device
    dev.off() # turn off print device
    cat("\nprinted PNG plot with name \"", cleanPlotTitle,"\"",sep="")
    
  }
  else
  {
    # Tested, but this is the slow version
    continue <- TRUE
    attempt <- 1
    while(TRUE == continue && attempt <= 3)
    {
      if(is.ggplot(plt))
      {
        if(3 == attempt)
        {
          graphics.off() # reset all graphic devices to clear memory
        }
        continue <- tryCatch({
          ggsave(fullFileName,
                 plot=plt,
                 path = getwd(),
                 height = height_in,
                 width = width_in,
                 dpi=dpi,
                 units = "in")
          cat("Printed ggplot (attempt ",attempt, ") ", fullFileName, "\n",sep="")
          FALSE #returns this
        },error = function(err) {
          cat("\n")
          print(paste("Unsuccessful printing (attempt ", attempt, ") of ", fullFileName))
          #stop(err)
          TRUE #returns this
        })
        cat(paste("success:",!continue,"\n\n"))
      }
      else
      {
        continue <- tryCatch({
          # create title
          dev.copy(png,
                   fullFileName,
                   width = width_in,
                   height = height_in,
                   units = "in",
                   res = dpi)
          cat("Printed plot (attempt ",attempt, ") ", fullFileName, "\n",sep="")
          FALSE #returns this
        },error = function(err) {
          cat("\n")
          print(paste("Unsuccessful printing (attempt ", attempt, ") of ", fullFileName))
          #stop(err)
          TRUE #returns this
        })
        dev.off()
        cat(paste("success:",!continue,"\n\n"))
      }
      attempt <- attempt + 1
    }
    if(TRUE == continue)
    {
      cat("\n")
      print(paste("Failed at printing:", fullFileName))
    }
  }
}

# creates directory in cwd if it doesn't already exist
# fileNamePath = TRUE if subDirPath is actually a fileNamePath (e.g. "plots/test.png")
# fileNamePath = FALSE if subDirPath is a sub dir (e.g. "plots/")
createDirectoryIfNotExists <- function(subDirPath,fileNamePath=FALSE)
{
  mainDir <- getwd()
  fp <- file.path(mainDir, subDirPath)
  secDir <- if(fileNamePath) { fp } else { dirname(fp)}
  if(!dir.exists(secDir))
  {
    #create directory
    tryCatch({
      dir.create(file.path(mainDir, secDir),
                 recursive = TRUE)
    }, warning = function (war) {
      
      if(!endsWith(war$message,"already exists"))
      {
        warning(war)
      }
      
    }, error = function(err) {
      stop(err)
    }, finally = {
    }) # END tryCatch
  }
  return(dir.exists(secDir))
  
}

# for a vector containing a single factor level returns the value of that factor
# for a vector containing more than a single factor levels returns NULL value
# for a vector containing < 0 factor levels returns empty string ('') & a warning
getSingleFactorFromVector <- function(vec,asCharacter=FALSE)
{
  cls <- NULL
  numClassesInVector <- dplyr::n_distinct(vec)
  if(1 == numClassesInVector)
  {
    cls <- dplyr::first(vec)
    if(asCharacter)
    {
      cls <- as.character(cls)
    }
  }
  else if(0 >= numClassesInVector)
  {
    cls <- ''
    warning('detected 0 or less distinct factors in provided vector')
  }
  return(cls)
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