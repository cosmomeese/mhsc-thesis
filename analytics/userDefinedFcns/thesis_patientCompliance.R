# Test patient compliance

pkg <- c("tidyverse","PairedData","aplpack","ggplot2")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

library(tidyverse)
#library(PairedData)
library(aplpack)
library(ggplot2)

#load up combined data file (with combinedData)
dataSetColname <- 'Dataset'
jfbDataSetID <- 'J'

fitbitDf <- combinedData %>% 
  filter((!!rlang::sym(dataSetColname)) == jfbDataSetID) %>% filter(StudyIdentifier != 'Canary')

# N.B. uses system time zone
dateThresholds <- c(May=as.POSIXct("2018-04-10 00:00:00", tz='America/Toronto'),
                    Now=as.POSIXct(Sys.time(), tz='America/Toronto'))

percentFac <- 100
#activeThresholds <- c(perfect=0.95, excellent=0.90, good=0.68, sporadic=0.50, barely=1/7, not=-0.1)*percentFac
activeThresholds <- c(high=0.95,mid=0.80,low=0.50,not=-.01)*percentFac
activeThresholds <- sort(activeThresholds)

firstRun <- TRUE
for(dateThresIdx in seq_along(dateThresholds))
{
  dateThres <- dateThresholds[dateThresIdx]
  dateThresName <- names(dateThresholds)[dateThresIdx]
  fitbitBeforeDate <- fitbitDf %>% filter(DateTime < dateThres)
  
  dfAll <- fitbitBeforeDate %>% dplyr::group_by(StudyIdentifier) %>%
            dplyr::summarize(allDays = n_distinct(Day))
  
  dfStepsGTE0 <- fitbitBeforeDate %>% filter(Steps > 0 | !is.na(HeartRate)) %>% 
                  dplyr::group_by(StudyIdentifier) %>%
                  dplyr::summarize(activeDays = n_distinct(Day))
  
  dfTogether <- merge(dfAll,
                      dfStepsGTE0,
                      by="StudyIdentifier",
                      all.x=TRUE)
  
  #replace days with NA (due to dfStepsGTE0 dropping the StudyID since no days were active) with 0
  dfTogether$activeDays[is.na(dfTogether$activeDays)] <- 0
  dfTogether <- dfTogether %>% mutate(percentActiveDays = percentFac*activeDays/allDays)
  dfTogether <- dfTogether %>% mutate(activeGroup = findInterval(percentActiveDays,
                                                                 activeThresholds,
                                                                 left.open=TRUE))
  
  dfSummarized <- dfTogether %>% dplyr::group_by(activeGroup) %>% dplyr::summarize(count=n())
  dfSummarized <- dfSummarized %>% mutate(countPercentage = count / sum(count[!is.na(activeGroup)]))
  dfSummarized[is.na(dfSummarized$activeGroup),'countPercentage'] <- NA
  naActiveVector <- !is.na(dfSummarized$activeGroup)
  reverseCount <- rev(dfSummarized$count[naActiveVector])
  dfSummarized$cumSum <- 0
  dfSummarized$cumSum[naActiveVector] <- rev(cumsum(reverseCount))
  dfSummarized$cdf <- dfSummarized$cumSum / sum(reverseCount[naActiveVector])
  
  if(firstRun)
  {
    dfCombined <- dfTogether
    dfSummarizedCombined <- dfSummarized
    firstRun <- FALSE
  }
  else
  {
    dfCombined <- merge(dfCombined,
                        dfTogether,
                        by="StudyIdentifier",
                        all.x=TRUE,
                        all.y=TRUE,
                        suffixes=(c("",dateThresName)))
    dfSummarizedCombined <- merge(dfSummarizedCombined,
                                  dfSummarized,
                                  by="activeGroup",
                                  all.x=TRUE,
                                  all.y=TRUE,
                                  suffixes=c("",dateThresName))
  }
  
  cat("As (single) stem plot ",dateThresName,"\n")
  stem(dfTogether$percentActiveDays, scale=2)
  cat("\n")
  
  qplot(dfTogether$allDays,
        dfTogether$activeDays,
        main=dateThresName)
  
  qplot(sort(dfTogether$percentActiveDays),
        seq_along(dfTogether$percentActiveDays),
        main=dateThresName)
}

cat("As stem plot ", names(dateThresholds)[1], " | ", names(dateThresholds)[2], "\n")
aplpack::stem.leaf.backback(dfCombined$percentActiveDays,
                            dfCombined$percentActiveDaysNow,
                            depths=FALSE)
cat("\n")

# Labels
dfSummarizedCombined <- dfSummarizedCombined[order(-dfSummarizedCombined$activeGroup),]
dfSummarizedCombined$activeGroup <- factor(dfSummarizedCombined$activeGroup,
                                           levels=seq_along(activeThresholds),
                                           labels=names(activeThresholds))


wilcox.test(dfSummarizedCombined$countPercentage,dfSummarizedCombined$countPercentageNow,
            paired=TRUE,
            alternative = "two.sided")

dfSummarizedCombined

ccDfComb <- dfCombined[complete.cases(dfCombined),]
before<-ccDfComb$percentActiveDays
after<-ccDfComb$percentActiveDaysNow
pd <- PairedData::paired(before,after)
plt <- PairedData::plot(pd,type="profile")
plot(plt)