### Display Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries
#local functions

displaySimonData <- function(processedData,analyzedData)
{
  
  #http://motioninsocial.com/tufte/#minimal-boxplot
  
  #Mean Daily Total Steps
  plot.new()
  frame()
  x <- processedData$NYHAClass
  y <- processedData$StepData.MeanDailyTotalSteps
  y_labels <- round(sort(c(aggregate(y, by=list(x), FUN=mean)[[2]],
                     min(y,na.rm=TRUE),
                     max(y,na.rm=TRUE)
                     )
                   ),0) #means + min & max (rounded)
  boxplot(y ~ x, 
          main= NULL,
          axes = FALSE, 
          xlab="NYHA Class", 
          ylab="Mean Daily Total Steps",
          pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                      medcex = 0.7,  outcex = 0, staplelty = "blank"))
  #axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
  #axis(2, las=2, tick=F, family="serif")
  axis(1, at=1:length(levels(x)), label=sort(levels(x)), tick=F, family="serif")
  axis(2, las=2, at=c(850,3750,5750,10250), labels=y_labels, tick=F, family="serif") #at locations are faked to get the top label to display
  text(1.4,max(y,na.rm=TRUE)/1.1, pos = 4, family="serif",
       sprintf("Daily Average Steps per NYHA Class (n=%s)",nrow(processedData)))
  
}

