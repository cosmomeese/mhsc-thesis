### Display Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","ggthemes","car","rgl")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(ggplot2)
library(ggthemes)
library(car)
library(rgl)

displaySimonData <- function(processedData,analyzedData)
{
  
  #http://motioninsocial.com/tufte/#minimal-boxplot
  
  #Mean Daily Total Steps
  pD_removedNYHA.NAs <- processedData[!is.na(processedData$NYHAClass), ]
  fiveNumSum <- round(aggregate(pD_removedNYHA.NAs$StepData.MeanDailyTotalSteps, 
                         by = list(pD_removedNYHA.NAs$NYHAClass), 
                         FUN=fivenum)[[2]],0)
  dataPoints <- nrow(pD_removedNYHA.NAs) #count number of non-NA data points
  numInClass <- table(m_cData$NYHAClass)
  numInSex <- table(m_cData$Sex)
  numInSecondaryVar <- table(m_cData$WristbandPreference)
  sTitle <- sprintf("n=%s (II->%s, III->%s) (F->%s, M->%s) (A->%s, B->%s)",
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
          geom_point(alpha = 0.60, 
               aes(shape = factor(Sex, levels=rev(levels(Sex))), 
                   color = Age),
               size = 2) +
          geom_tufteboxplot(outlier.colour="transparent", #boxplot
                            na.rm=TRUE,
                            median.type = "point") +
          stat_summary(fun.y = "median", size = 1, geom = "point") +
          coord_cartesian(xlim = NULL, 
                          ylim = NULL, 
                          expand = TRUE) +
          scale_y_continuous(name = "Daily Average Step Count", 
                             breaks = fiveNumSum[1,],
                             sec.axis = sec_axis(trans=~., #i.e. 1:1 transformation
                                                 breaks=fiveNumSum[2,])) +
          labs(title = "Daily Average Steps per NYHA Class",
               subtitle = sTitle,
               caption = "Simon Bromberg's Thesis Data") +
          xlab("NYHA Class")
  print(plot)
  rm(pD_removedNYHA.NAs)
  
  #Plot of Steps.DailyAverageStepCount, HR.1minDrop, Weight (Note: Opens in Seperate Window)
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
  
  #
  
  
}

