### Display Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","ggforce","car","rgl","hms","bigmemory")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(ggplot2)
library(ggthemes)
library(ggforce)
library(car)
library(rgl)
library(hms)
library(bigmemory)

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
rm(sourceDir)

displaySimonData <- function(processedData,analyzedData)
{
  
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
      plot <- basePlot +
        geom_density(data=subset(summar_stepDistr,
                                 Steps >= threshold), 
                     position = "identity", 
                     alpha = 0.4) +
        ggtitle(paste(baseTitle,threshold))
      print(plot)
    }
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
  summar_stepDistr <- unnestedDF %>% 
    filter(Steps >= 100,!is.na(NYHAClass)) %>%
    group_by(StudyIdentifier,NYHAClass)
  plot <- ggplot(data = summar_stepDistr) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    geom_density(aes(x = Steps, fill=NYHAClass, color=NYHAClass), position = "identity", alpha = 0.4) +
    facet_wrap(~StudyIdentifier)
  print(plot)  
    
  
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
      }
      else
      {
        #save plot (verbosely)
        fileName <- paste('heatmapNYHAClass',class,'_page',currentPage,'.png',sep="") 
        ggsave(fileName, path = getwd())
        cat("\nPrinted ",fileName,"\n",sep="")
        
        #determine if there's a next page
        if(currentPage >= numPages)
        {
          hasNextPage <- FALSE
        }
      }
      currentPage <- currentPage + 1
    }
  }
  
  #Plot of Signal Match Image #------------------------------------------------------------------------------------------------------------------------------
  

  
  
  #Plot of ... #-------------------------------------------------------------------------------------------------------------------------------------------
    
    
  
  
  #Clean up
  rm(pD_removedNYHA.NAs)
  
}

