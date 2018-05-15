### Display Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","ggforce","car","rgl","hms","bigmemory","glue","tidyverse","seewave")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(glue)
library(ggplot2)
library(ggthemes)
library(ggforce)
library(car)
library(rgl)
library(hms)
library(bigmemory)
library(seewave)


sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
rm(sourceDir)

displaySimonData <- function(processedData,analyzedData,
                             printHeatMap=FALSE,
                             printMinByMin=FALSE,
                             printSpectral=FALSE)
{
  if(!printHeatMap)
  {
    warning("Reminder that function is set to not print patient activity heat maps. Set printHeatMap = TRUE to print these.")
  }
  if(!printMinByMin)
  {
    warning("Reminder that function is set to not print patient activity heat maps. Set printMinByMin = TRUE to print these.")
  }
  if(!printSpectral)
  {
    warning("Reminder that function is set to not print patient spectral analysis graphs. Set printSpectral = TRUE to print these.")
  }
  #http://motioninsocial.com/tufte/#minimal-boxplot
  
  #Mean Daily Total Steps + Five Number Summary #--------------------------------------------------------------------------------------------------------------
  pD_removedNYHA.NAs <- processedData[!is.na(processedData$NYHAClass), ]
  fiveNumSum <- round(aggregate(pD_removedNYHA.NAs$StepData.MeanDailyTotalSteps, 
                         by = list(pD_removedNYHA.NAs$NYHAClass), 
                         FUN=fivenum)[[2]],0)
  dataPoints <- nrow(pD_removedNYHA.NAs) #count number of non-NA data points
  numInClass <- table(m_cData$NYHAClass)
  numInSex <- table(m_cData$Sex)
  numInSecondaryVar <- table(m_cData$WristbandPreference)
  sTitle <- sprintf("n=%s (II->%s, III->%s) (F->%s, M->%s) (L->%s, R->%s)",
                   dataPoints,
                   numInClass["II"],
                   numInClass["III"],
                   numInSex["F"],
                   numInSex["M"],
                   numInSecondaryVar[1],
                   numInSecondaryVar[3])
  plot <- ggplot(data = pD_removedNYHA.NAs, 
                 aes(NYHAClass,StepData.MeanDailyTotalSteps)) +
              theme_tufte(base_family = "serif", 
                          ticks = FALSE) +
              geom_jitter(alpha = 0.60, 
                   aes(shape = factor(Sex, levels=rev(levels(Sex))), 
                       color = HR.1minDrop),
                   size = 2,
                   width = 0.1) +
              geom_tufteboxplot(outlier.colour="transparent", #boxplot
                                na.rm=TRUE,
                                median.type = "point") +
              stat_summary(fun.y = "median", size = 1, geom = "point") +
              coord_cartesian(xlim = NULL, 
                              ylim = NULL, 
                              expand = TRUE) +
              scale_y_continuous(name = "Daily Average Step Count (Five Num Summary)", 
                                 breaks = fiveNumSum[1,],
                                 sec.axis = sec_axis(trans=~., #i.e. 1:1 transformation
                                                     breaks=fiveNumSum[2,])) +
              labs(title = "Daily Average Steps per NYHA Class",
                   subtitle = sTitle,
                   caption = "Simon Bromberg's Thesis Data") +
              xlab("NYHA Class") +
              scale_colour_gradient2(low="blue", mid="white", midpoint= 15, high="red")
  print(plot)
  
  #Mean Daily Total Steps + Standard Deviation #--------------------------------------------------------------------------------------------------------------
  
  MinMeanSEMMax <- function(x) {
    v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
    names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
    v
  }
  
  pD_removedNYHA.NAs <- processedData[!is.na(processedData$NYHAClass), ]
  plot <- ggplot(data = pD_removedNYHA.NAs, 
                 aes(NYHAClass,StepData.MeanDailyTotalSteps,color=NYHAClass)) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    stat_summary(fun.data=MinMeanSEMMax, geom="boxplot") +
    geom_jitter() + 
    labs(title = "Daily Average Steps per NYHA Class (Stat Summary)",
         caption = "Simon Bromberg's Thesis Data") +
    xlab("NYHA Class")
  print(plot)
  
  #--------------------------------------------------------------------------------------------------------------
  
  pD_removedNYHA.NAs <- processedData[!is.na(processedData$NYHAClass), ]
  plot <- ggplot(data = pD_removedNYHA.NAs, 
                 aes(NYHAClass,StepData.MeanDailyMaxSteps,color=NYHAClass)) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    stat_summary(fun.data=MinMeanSEMMax, geom="boxplot") +
    geom_jitter() +
    labs(title = "Daily Max Steps per NYHA Class (Stat Summary)",
         caption = "Simon Bromberg's Thesis Data") +
    xlab("NYHA Class")
  print(plot)
  
  #Plot of Steps.DailyAverageStepCount, HR.1minDrop, Weight (Note: Opens in Seperate Window) #---------------------------------------------------------------
  open3d()
  mat <- matrix(1:4, 2, 2)
  layout3d(mat, height = rep(c(5, 1), 1), sharedMouse = TRUE)
  next3d()
  scatter3d(StepData.MeanDailyTotalSteps ~ Weight + HR.1minDrop | NYHAClass, data = pD_removedNYHA.NAs, surface = FALSE)
  next3d()
  text3d(0,0,0,"Mean Daily Total Steps")
  next3d()
  scatter3d(StepData.MeanDailyMaxSteps ~ Weight + HR.1minDrop | NYHAClass, data = pD_removedNYHA.NAs, surface = FALSE)
  next3d()
  text3d(0,0,0,"Mean Daily Max Steps")
  
  
  unnestedDF <- unnestStepDataFrame(processedData)
  #Plot of Minute by Minute Step Count Densities
  summar_stepDistr <- unnestedDF %>% 
                        filter(!is.na(NYHAClass)) %>%
                        group_by(StudyIdentifier,NYHAClass)
  classCounts <- count(processedData, NYHAClass) %>% mutate(n = 1/n) %>% rename(stackedDensityWeighting = n)
  summar_stepDistr <- inner_join(summar_stepDistr,classCounts,by = "NYHAClass")
    
    # Baseplot
    baseTitle <- "Density Plot of Subjects Minute by Minute Step Count, excluding step counts < "
    basePlot <- ggplot(data = summar_stepDistr,
                       aes(x = Steps, 
                           group=StudyIdentifier, 
                           color=StudyIdentifier)) +
      theme_tufte(base_family = "serif", 
                  ticks = FALSE) +
      facet_wrap(~NYHAClass, 
                 labeller=labeller(NYHAClass = 
                                     c(II="NYHA Class II", 
                                       III ="NYHA Class III"))) +
      xlab("minute by minute step count")
    
    # Complete plot
    for(threshold in c(0,1,50,100,125))
    {
      #Raw density plot
      plot <- basePlot +
        geom_density(data=subset(summar_stepDistr,
                                 Steps >= threshold), 
                     position = "identity", 
                     alpha = 0.4) +
        ggtitle(paste(baseTitle,threshold))
      print(plot)
    }
    
    # Complete stacked plots
    displaySimonData <- list()
    baseTitle <- "Stacked Weighted Density Plot of Subjects Min. by Min. Step Count, excluding step counts < "
    for(threshold in c(0,1,50,100,125))
    {
    
      #Stacked density plot (can 'elucidate average/common peaks')
      plot <- basePlot +
        geom_density(data=subset(summar_stepDistr,
                                 Steps >= threshold), 
                     position = "stack", 
                     alpha = 0.4,
                     inherit.aes = TRUE,
                     aes(fill=StudyIdentifier,
                         weight=stackedDensityWeighting)) +
        ggtitle(paste(baseTitle,threshold))
      
      displaySimonData[threshold %>% toString] <- ggplot_build(plot)
      print(plot)
    }
    
    #Clean up
    rm(basePlot,baseTitle)
    
  #Plot of NYHA Class vs Activity Time #--------------------------------------------------------------------------------------------------------------------
  summarized <- unnestedDF %>%
    group_by(StudyIdentifier,NYHAClass,Day) %>%
    summarize(n_Minutes = n(),
              n_gt100 = sum(Steps > 1),
              p_gt100 = n_gt100 / n_Minutes) %>%
    na.omit
  plot <- ggplot(data = summarized, 
                 aes(x = NYHAClass, color=StudyIdentifier)) +
          theme_tufte(base_family = "serif", 
                      ticks = FALSE) +
          geom_density(aes(n_gt100))
  print(plot)
  
  #Plot of Step Distribution #------------------------------------------------------------------------------------------------------------------------------
  
  #individual step distributions
  baseTitle <- "Individual Patient Density Plots, Minute by Minute Step Count, excluding step counts < "
    #so that facets are grouped by NYHAClass (vs Study Identifier#) we need to the factor levels for ggplot 
  oldorder <- unnestedDF$StudyIdentifier %>% levels
  neworder <- list()
  
  for(class in levels(unnestedDF$NYHAClass))
  {
    #for each NYHAClass remove all data points that don't have a matching class, extract the remaining StudyIdentifiers (apply factor function again to drop all unused StudyIdentifiers) and then extract the levels
    subsetBelongingToClass <- unnestedDF %>% filter(NYHAClass == class) %$% StudyIdentifier %>% factor %>% levels
    #append this subsetted group for this particular NYHAClass to the rest of the list
    neworder <- append(neworder,subsetBelongingToClass)
  }
  #now add any factors that were dropped out because they didn't have a NYHAClass
  neworder <- append(neworder,
                     unnestedDF %>% filter(is.na(NYHAClass)) %$% StudyIdentifier %>% factor %>% levels)
  
  summar_stepDistr <- unnestedDF %>% 
                        filter(!is.na(NYHAClass)) %>%
                        group_by(StudyIdentifier,NYHAClass) %>% 
                        arrange %>% transform(StudyIdentifier=factor(StudyIdentifier,levels=neworder),StudyIdentifier)
  
  for(threshold in c(0,1,50,100,125))
  {
    plot <- ggplot(data = subset(summar_stepDistr,
                                 Steps >= threshold)) +
      theme_tufte(base_family = "serif", 
                  ticks = FALSE) +
      geom_density(aes(x = Steps, fill=NYHAClass, color=NYHAClass), position = "identity", alpha = 0.4) +
      facet_wrap(~StudyIdentifier) +
      ggtitle(paste(baseTitle,threshold))
    print(plot)
  }
  rm(summar_stepDistr,oldorder,neworder)
  
  #Plot of Activity Heat Map #------------------------------------------------------------------------------------------------------------------------------
    
  if(printHeatMap)
  {
  
    #Heat Map of Activity vs Day for each Patient
    heatMapData <- mutate(unnestedDF,Time=as.hms(Time)) %>%
      group_by(StudyIdentifier,NYHAClass,Day)
    
    maxSteps <- max(heatMapData$Steps, na.rm = TRUE)
    maxDays <- max(heatMapData$Day, na.rm = TRUE)
    
    for(class in c("II","III"))
    {
      currentPage <- 0
      hasNextPage <- TRUE
      numPages <- 0
      while(hasNextPage)
      {
        
        plot <- ggplot(data = subset(heatMapData,class==NYHAClass),aes(Time,Day)) +
          theme_tufte(base_family = "serif",
                      ticks = FALSE) +
          geom_raster(aes(fill = Steps)) +
          #facet_wrap(~StudyIdentifier) +
          expand_limits(y=c(0,maxDays)) +
          facet_wrap_paginate(~StudyIdentifier, ncol = 5, nrow = 3, page = currentPage) +
          #scale_fill_gradientn(colors=c("white","#e5f5e0","#a1d99b","#31a354"),values=c(0,1/maxSteps,1/2*maxSteps,1)) #from Greens color scheme from http://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
          #scale_fill_gradientn(colors=c("white","#fee6ce","#fdae6b","#e6550d"),values=c(0,1/maxSteps,1/2*maxSteps,1)) #from Orange color scheme from http://colorbrewer2.org/#type=sequential&scheme=Oranges&n=3
          scale_fill_gradientn(colors=c("#92c5de","#f7f7f7","#f4a582","#ca0020"),values=c(0,1/maxSteps,1/2*maxSteps,1)) #modified from diverging red & blue color color scheme from http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=5
        #scale_fill_distiller()
        
        if(0 >= currentPage)
        {
          numPages <- n_pages(plot)
          currentPage <- 0 #make sure it's 0 so that it gets incremented to 1 for next time
          cat("Print path: ",getwd(), sep="")
        }
        else
        {
          #save plot (verbosely)
          fileName <- paste('heatmapNYHAClass',class,'_page',currentPage,'.png',sep="") 
          ggsave(fileName, path = getwd())
          cat("Printed ",fileName,"\n",sep="")
          
          #determine if there's a next page
          if(currentPage >= numPages)
          {
            hasNextPage <- FALSE
          }
        }
        currentPage <- currentPage + 1
      }
      cat("\n")
    }
     
  }
  
  #Plot of Minute by Minute Step Count #---------------------------------------------------------------------------------------------------------------------
  
  if(printMinByMin)
  {
    
    #Minute by Minute Activity for each Patient
    #unnestedDF <- unnestStepDataFrame(processedData)
    #unnestedDF

    maxSteps <- max(unnestedDF$Steps, na.rm = TRUE)
    maxDays <- max(unnestedDF$Day, na.rm = TRUE)
    
    #for(class in c("II","III"))
    for(studyID in unique(unnestedDF %>% 
                          dplyr::filter(!is.na(NYHAClass)) 
                          %>% .$StudyIdentifier))
    {
      ssForStudyID <- subset(unnestedDF,
                             StudyIdentifier==studyID)
      class <- processedData$NYHAClass[processedData$StudyIdentifier == studyID]
      color <- if("III"==class) "PURPLE" else "ORANGE"
      for(day in unique(ssForStudyID$Day))
      {
        
        plot <- ggplot(data = subset(ssForStudyID,
                                     Day==day),
                              aes(Time,Steps)) +
          theme_tufte(base_family = "serif",
                      ticks = TRUE) +
          geom_line(colour = color, size=0.05) +
          scale_y_continuous(limits = c(0, maxSteps)) +
          ggtitle(glue("{studyID} (Class {class}) - Day {day}")) +
          xlab("Time") + ylab("Steps Count [number of steps]") +
          scale_x_datetime(date_breaks = "2 hour", 
                           date_minor_breaks = "1 hour",
                           date_labels = "%H")
          #scale_fill_gradientn(colors=c("#92c5de","#f7f7f7","#f4a582","#ca0020"),values=c(0,1/maxSteps,1/2*maxSteps,1)) #modified from diverging red & blue color color scheme from http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=5
        #scale_fill_distiller()
        #print(plot)
        
        #save plot (verbosely)
        fileName <- glue("plots/MinByMin/Class{class}/{studyID}/minByMinPlot{studyID}_day{day}.png") 
        
        #create directory
        tryCatch({
          dir.create(file.path(getwd(), dirname(fileName)),
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
        
        ggsave(fileName,
               path = getwd(),
               height = 8,
               width = 11,
               units = "in")
        cat("Printed ",fileName,"\n",sep="")
      }
    }
    
  }
  #Plot of Spectrogram #-------------------------------------------------------------------------------------------------------------------------------------
  
  if(printSpectral)
  {
    #Minute by Minute Activity for each Patient
    #unnestedDF <- unnestStepDataFrame(processedData)
    #unnestedDF
    
    maxSteps <- max(unnestedDF$Steps, na.rm = TRUE)
    maxDays <- max(unnestedDF$Day, na.rm = TRUE)
    
    for(studyID in unique(unnestedDF %>% 
                          dplyr::filter(!is.na(NYHAClass)) 
                          %>% .$StudyIdentifier))
    {
      studyID <- "HF020"
      ssForStudyID <- subset(unnestedDF,
                             StudyIdentifier==studyID)
      class <- processedData$NYHAClass[processedData$StudyIdentifier == studyID]
      
      for(day in unique(ssForStudyID$Day))
      {
        day <- 2
        ssForStudyIDDay <- subset(ssForStudyID,
                                  Day==day)
                
        #window_n = 15*Fs*60 w/ zPadFactor = 10
        
        # Spectrogram
        Fs <- 1/60 # i.e. 1 per minute
        F_max <- Fs/2 # if fx = 1/60 then fs/x = per 2 minute
        window_n <- trunc(12*60*Fs*60)          # 30 min data window
        fftn <- 2^ceiling(log2(abs(window_n))) # next highest power of 2
        zPadFactor <- 5
        winType <- "flattop"
        #overlap = ceiling(length(window_n)/8)
        
        dynspectro()
        seewave::spectro(wave=ssForStudyID$Steps, 
                         f = Fs,
                         wl = fftn, 
                         wn = winType, 
                         zp = fftn*zPadFactor,
                         ovlp = 0.90, 
                         complex = FALSE, 
                         norm = TRUE, 
                         correction="none",
                         fftw = FALSE,
                         dB = "max0",
                         dBref = NULL,
                         plot = TRUE, flog = FALSE, grid = TRUE,
                         osc = TRUE, scale = TRUE, cont = FALSE,
                         collevels = NULL,
                         palette = spectro.colors,
                         contlevels = NULL, colcont = "black",
                         colbg = "white", colgrid = "black",
                         colaxis = "black", collab="black",
                         cexlab = 1, cexaxis = 1,
                         tlab = "Time (s)",
                         flab = "Frequency (kHz)",
                         alab = "Amplitude",
                         scalelab = "Amplitude\n(dB)",
                         main = glue("{studyID} (Class {class}) - Day {day}"),
                         scalefontlab = 1, scalecexlab =0.75,
                         axisX = TRUE, axisY = TRUE, tlim = NULL, trel = TRUE,
                         flim = NULL, flimd = NULL,
                         widths = c(6,1), heights = c(3,1),
                         oma = rep(0,4),
                         listen=FALSE)
      } # END for day
    } # END for StudyID
      

      # # Spectrogram
      # Fs <- 1/60 # i.e. 1 per minute
      # F_max <- Fs/2 # if fx = 1/60 then fs/x = per 2 minute
      # step <- trunc(30*Fs*60)             # one spectral slice every 15 min
      # window_n <- trunc(4*60*Fs*60)          # 30 min data window
      # fftn <- 2^ceiling(log2(abs(window_n))) # next highest power of 2
      # 
      # win <- signal::flattopwin(window_n) # sym = c('symmetric', 'periodic'))
      # #overlap = ceiling(length(window_n)/8)
      # spg <- signal::specgram(x=ssforStudyID$Steps,
      #                         n=fftn,
      #                         Fs=Fs,
      #                         window=win,
      #                         overlap= window_n - step
      #                         )
      # 
      # S <- abs(spg$S[2:(fftn*F_max*(1/Fs)),])   # magnitude in range 0<f<=4000 Hz.
      # S <- S/max(S)         # normalize magnitude so that max is 0 dB.
      # S[S < 10^(-80/10)] <- 10^(-80/10)    # clip below -40 dB.
      # #S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
      # par(mfrow=c(2,1))
      # #1
      # image(t(20*log10(S)), axes = TRUE, col = gray(0:255 /255))  #, col = gray(0:255 / 255))
      # #2
      # plot(ssforStudyID$Steps)
      # 
      # plot <- ggplot(data = subset(ssforStudyID),
      #                aes(Time,Steps)) +
      #   theme_tufte(base_family = "serif",
      #               ticks = TRUE) +
      #   geom_line( size=0.05) +
      #   scale_y_continuous(limits = c(0, maxSteps)) +
      #   #ggtitle(glue("{studyID} (Class {class}) - Day {day}")) +
      #   xlab("Time") + ylab("Steps Count [number of steps]") +
      #   scale_x_datetime(date_breaks = "2 hour", 
      #                    date_minor_breaks = "1 hour",
      #                    date_labels = "%H")
                             
        
  } # END if(printSpectral)
  
  
  
  
  #Plot of Signal Match Image #------------------------------------------------------------------------------------------------------------------------------
  

  
  
  #Plot of ... #-------------------------------------------------------------------------------------------------------------------------------------------
    
    
  
  
  #Clean up
  rm(pD_removedNYHA.NAs)

} # END displaySimonData