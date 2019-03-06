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
source(paste(sourceDir,"/","savePlot.R",
             sep=""))
rm(sourceDir)

#########################################################

#n.b. relative to gwd()
plotSaveLocation <- "plotsFiveNumBoxPlots/"

vars <- c("StepData.MeanDailyTotalSteps",
         "StepData.MeanDailyMaximumSteps",
         "StepData.MaximumDailyMaximumSteps")

varTitles <- list("Mean Daily Total Steps [steps]",
                 "Mean Daily Maximum Steps [steps/minute]",
                 "Maximum Daily Maximum Steps [steps/minute]")
names(varTitles) <- vars

FiveNumberFcn <- function(x) {
  v <- fivenum(x)
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}


## PREPROCESSING
nClass <- "NYHAClass"

# remove NA
pD_removedNYHA.NAs <- processedData[!is.na(processedData[[nClass]]), ]

# revise factor labels to II* and III*
nClassVec <- pD_removedNYHA.NAs[[nClass]]
nClassVec <- plyr::mapvalues(nClassVec,
                             from = levels(nClassVec),
                             to = paste(levels(nClassVec),"*",sep=""))
pD_removedNYHA.NAs[nClass] <- nClassVec

## START PRINT

for(var in vars)
{
  fiveNumSum <- round(aggregate(pD_removedNYHA.NAs[[var]], 
                                by = list(pD_removedNYHA.NAs[[nClass]]), 
                                FUN=fivenum)[[2]],0)
  dataPoints <- nrow(pD_removedNYHA.NAs) #count number of non-NA data points
  #numInClass <- table(m_cData[[nClass]])
  #numInSex <- table(m_cData$Sex)
  #numInSecondaryVar <- table(m_cData$WristbandPreference)
  #sTitle <- sprintf("n=%s (II->%s, III->%s) (F->%s, M->%s) (L->%s, R->%s)",
  #                  dataPoints,
  #                  numInClass["II"],
  #                  numInClass["III"],
  #                  numInSex["F"],
  #                  numInSex["M"],
  #                  numInSecondaryVar[1],
  #                  numInSecondaryVar[3])
  plot <- ggplot(data = pD_removedNYHA.NAs,
                 aes_string(nClass,var,color=nClass)) +
                 #aes(NYHAClass,StepData.MeanDailyTotalSteps,color=NYHAClass)) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    stat_summary(fun.data=FiveNumberFcn, geom="boxplot") +
    geom_jitter(alpha = 0.60, 
                #aes(shape = factor(Sex, levels=rev(levels(Sex))), 
                #    color = HR.1minDrop),
                size = 2,
                width = 0.1) +
    coord_cartesian(xlim = NULL, 
                    ylim = NULL, 
                    expand = TRUE) +
    scale_y_continuous(name = varTitles[[var]], 
                       breaks = fiveNumSum[1,],
                       sec.axis = sec_axis(trans=~., #i.e. 1:1 transformation
                                           breaks=fiveNumSum[2,])) +
    #scale_colour_gradient2(low="blue", mid="white", midpoint= 15, high="red") +
    #labs(title = varTitles[[var]],
    #     #subtitle = sTitle,
    #     caption = "Simon Bromberg's Thesis Data") +
    xlab("NYHA Class")
  print(plot)
  
  dpi <- 175
  plotTitle <- paste("FigX:",varTitles[[var]],"(FiveNumSummary).png")
  savePlot(fileName=plotTitle,
           filePrefix=plotSaveLocation,
           plot=plot,
           width = 800/dpi,
           height = 1200/dpi,
           units = "in",
           res.dpi = dpi,
           isGGPlot=TRUE)
}

