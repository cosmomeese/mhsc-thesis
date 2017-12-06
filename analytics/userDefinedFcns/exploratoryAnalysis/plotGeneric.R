### Plot Gamma Distribution (should auto plot state distributions of HMMs stored in hmm.activity)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","lubridate","plyr","dplyr","tidyr")
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
  
  TIME_COL_LABEL <- 'Time'
  DAYS_TO_KEEP <- 16
  data.orig <- data.clean
  
  
  ######################################## PLOTS
  
  # Frequency Spectrum Day by Day by Patient Plot ####
  # prep
  id <- "Freq-DayByDayByPatient"
  title <- "Step Time Series"
  plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  
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
  
  
  # Stacked Day by Day by Patient Plot ####
  # prep
  id <- "DayByDayByPatient(Stacked)"
  title <- "Step Time Series"
  plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  
  # plot
  for(pat in levels(plotData$StudyIdentifier))
  {
    data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
    if(nrow(data.plotSubset) > 0)
    {
      cls <- getSingleFactorFromVector(data.plotSubset$NYHAClass, asCharacter=TRUE)
      title.patient <- paste(title," | Pt",pat,sep="")
      
      mapping = aes(x=TIME_PLACEHOLDER,
                    y=Steps/CONSTANT_CONSTS$RESCALEFACTOR.STEPS,
                    colour=as.factor(yday(DateTime)))
      
      plt <- generateTimeStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                  plotData=data.plotSubset,
                                  id=id,
                                  mapping=mapping,
                                  title=title.patient,
                                  LABEL_CONSTS=LABEL,
                                  FORMAT_CONSTS=FORMAT,
                                  CONSTANT_CONSTS=CONSTANTS,
                                  nyhaClass=cls)
      
      # modify it to scale time axis by hours
      plt <- plt + 
        scale_x_datetime(LABEL$DATETIME$hour,
                         date_labels=FORMAT$DATETIME$hour,
                         date_breaks="6 hour")
      
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
  
  
  
  
  # Day by Day by Patient Plot ####
  # prep
  id <- "DayByDayByPatient"
  title <- "Step Time Series"
  plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  
  # plot
  for(pat in levels(plotData$StudyIdentifier))
  {
    data.plotSubset <- plotData %>% dplyr::filter(StudyIdentifier %in% c(pat)) # keep only this patient for plot
    if(nrow(data.plotSubset) > 0)
    {
      cls <- getSingleFactorFromVector(data.plotSubset$NYHAClass, asCharacter=TRUE)
      title.patient <- paste(title," | Pt",pat,sep="")
      plt <- generateTimeStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                  plotData=data.plotSubset,
                                  id=id,
                                  title=title.patient,
                                  LABEL_CONSTS=LABEL,
                                  FORMAT_CONSTS=FORMAT,
                                  CONSTANT_CONSTS=CONSTANTS,
                                  nyhaClass=cls)
      
      # modify it to scale time axis by hours
      plt <- plt + 
        scale_x_datetime(LABEL$DATETIME$hour,
                         date_labels=FORMAT$DATETIME$hour,
                         date_breaks="6 hour") +
        facet_wrap(~yday(DateTime))
      
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
  
  
  # Day by Day Plot ####
  # prep
  id <- "DayByDay"
  title <- "Step Time Series"
  plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  
  # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    plt <- generateTimeStepPlot(TIME_COL_LABEL=TIME_COL_LABEL,
                                plotData=plotData,
                                id=id,
                                title=title,
                                LABEL_CONSTS=LABEL,
                                FORMAT_CONSTS=FORMAT,
                                CONSTANT_CONSTS=CONSTANTS,
                                nyhaClass=cls)
    # modify it to scale time axis by hours
    plt <- plt + 
      scale_x_datetime(LABEL$DATETIME$hour,
                       date_labels=FORMAT$DATETIME$hour,
                       date_breaks="6 hour") +
      facet_wrap(~yday(DateTime))
    
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt)
  }
  
  
  # Hours Plot ####
  # prep
  id <- "Hours"
  title <- "Step Time Series"
  plotData <- data.orig %>% dplyr::filter(DateTime < (days(DAYS_TO_KEEP) + CONSTANTS$NORMALIZED_STUDY_DAY_START))
  # plot
  for(cls in CONSTANTS$NYHA_CLASS_VEC)
  {
    # generate basic plot
    plt <- generateDateTimeStepPlot(plotData=plotData,
                                    id=id,
                                    title=title,
                                    LABEL_CONSTS=LABEL,
                                    FORMAT_CONSTS=FORMAT,
                                    CONSTANT_CONSTS=CONSTANTS,
                                    nyhaClass=cls)
    # modify it to scale time axis by hours
    plt <- plt + scale_x_datetime(LABEL$DATETIME$hour,
                                  date_labels=FORMAT$DATETIME$hour,
                                  date_breaks="6 hour")
    
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt)
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
    plt <- generateDateTimeStepPlot(plotData=plotData,
                                    id=id,
                                    title=title,
                                    LABEL_CONSTS=LABEL,
                                    FORMAT_CONSTS=LABEL,
                                    CONSTANT_CONSTS=CONSTANTS,
                                    nyhaClass=cls)
    # modify it to scale time axis by days
    plt <- plt +  scale_x_datetime(LABEL$DATETIME$day,
                                   date_labels=FORMAT$DATETIME$day,
                                   date_breaks="1 day")
    
    # show the plot
    print(plt)
    # save plot
    savePNGPlot(plt)
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

# plots basic time step plot using mapping (i.e. as with generateDateTimeStepPlot but replaces datetime w/ time variable).
# N.B. will replace instances of TIME_PLACEHOLDER in mapping (i.e. specify as TIME_PLACEHOLDER with quotation marks) with the TIME_COL_LABEL specified
# i.e. if you specify in mapping that x=TIME_PLACEHOLDER it will plot time as the x value (so you can still use datetime if desired).
# N.B. generateDateTimeStepPlot performs faster if you don't need to plot actual time
# if no mapping is specified it will simply plot DateTime vs Steps according colored by StudyID
# if no nyhaClass is specified it will plot the no nyhaClass setting of generateDateTimeStepPlot
generateTimeStepPlot <- function(TIME_COL_LABEL,
                                 plotData,
                                 mapping,
                                 id=Sys.time(),
                                 title="Plot", 
                                 LABEL_CONSTS, 
                                 FORMAT_CONSTS, 
                                 CONSTANT_CONSTS,
                                 nyhaClass=NULL)
{
  KEY_WORD = "TIME_PLACEHOLDER"
  plotData.withTimeCol <- addTimeColumn(data = plotData,
                                        sourceColumnLabel = "DateTime",
                                        TIME_COL_LABEL = TIME_COL_LABEL,
                                        CONSTANT_CONSTS = CONSTANT_CONSTS)
  
  if(missing(mapping))
  {
    mapping = aes(x=TIME_PLACEHOLDER,
                  y=Steps/CONSTANT_CONSTS$RESCALEFACTOR.STEPS,
                  colour=StudyIdentifier)
  }
  for(i in 1:length(mapping))
  {
    mapping[[i]] <- recurseChangingKeywords(mapping[[i]],
                                            REPLACE_WORD = KEY_WORD,
                                            NEW_WORD = TIME_COL_LABEL)
  }
  
  plt <- generateDateTimeStepPlot(plotData=plotData.withTimeCol,
                                  mapping=mapping,
                                  id=id,
                                  title=title, 
                                  LABEL_CONSTS=LABEL_CONSTS, 
                                  FORMAT_CONSTS=FORMAT_CONSTS, 
                                  CONSTANT_CONSTS=CONSTANT_CONSTS,
                                  nyhaClass=nyhaClass)
  return(plt)
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

# plots basic datetime step plot using mapping.
# if no mapping is specified it will simply plot DateTime vs Steps according colored by StudyID
# if no nyhaClass (NULL or missing or '') is specified then will plot all classes
#     - if no nyhaClass = '' then will also not add a label to plot regarding NYHA class
generateDateTimeStepPlot <- function(plotData,
                                     mapping,
                                     id=Sys.time(),
                                     title="Plot", 
                                     LABEL_CONSTS, 
                                     FORMAT_CONSTS, 
                                     CONSTANT_CONSTS,
                                     nyhaClass=NULL)
{
  # since cls is considered optional,
  fullTitle <- paste(id,":",title,sep="")
  TITLE_SEP = " | "
  if(!((missing(nyhaClass) || is.null(nyhaClass)) || nyhaClass == ''))
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
  # if missing mapping, using default
  if(missing(mapping))
  {
    mapping = aes(x=DateTime,
                  y=Steps/CONSTANT_CONSTS$RESCALEFACTOR.STEPS,
                  colour=StudyIdentifier)
  }
  
  lineTransparency = 0.8
  plt <- ggplot(data=plotData,
                mapping) +
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
  cleanPlotTitle <- gsub('[^a-zA-Z0-9\\(\\)\\,\\-\\_\\+\\=[:space:]]', '-', plt$labels$title)
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