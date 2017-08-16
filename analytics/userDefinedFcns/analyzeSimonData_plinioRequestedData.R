### Analyze Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
rm(sourceDir)

#local functions

analyzeSimonData_plinioRequestedData <- function(processedData)
{
  #processedData <- m_cData
  #processedData <- pD_removedNYHA.NAs
   
  ##### START #####
  
  models <- list()
  
  unnested <- unnestStepDataFrame(m_cData)
  
  unnested <- unnested %>% group_by(StudyIdentifier,NYHAClass,Day,HR.1minDrop,Weight)
  
  summarized <- unnested %>% 
    summarize(dailyMax = max(Steps),
              n_gt0 = sum(Steps >0),
              p_gt0 = n_gt0 / n(), 
              n_gt1 = sum(Steps >1), 
              p_gt1 = n_gt1 / n(),
              n_gt50 = sum(Steps >50),
              p_gt50 = n_gt50 / n(),
              n_gt100 = sum(Steps > 100),
              p_gt100 = n_gt100 / n())
  
  finalSummarize <- summarized %>% 
    group_by(StudyIdentifier,NYHAClass,HR.1minDrop,Weight) %>% 
    summarize(meanDailyMax = mean(dailyMax), 
              nn_gt0 = sum(n_gt0), 
              pp_gt0 = sum(p_gt0) / n(), 
              nn_gt1 = sum(n_gt1), 
              pp_gt1 = sum(p_gt1) / n(),
              nn_gt50 = sum(n_gt50), 
              pp_gt50 = sum(p_gt50) / n(),
              nn_gt100 = sum(n_gt100),
              pp_gt100 = sum(p_gt100) / n())
  
  
  ##### meanDailyMax (steps) + proportion of all steps greater than 1
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  models[[1]] <- glm(formula = NYHAClass ~ meanDailyMax + pp_gt1,
               data = finalSummarize,
               family=binomial(link="logit"))
  
  # Show summary statistics
  print(summary(models[[1]]))
  
  ##### meanDailyMax (steps) + proportion of steps greater than 50 + heart rate drop in 1 minute
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  models[[2]] <- glm(formula = NYHAClass ~ meanDailyMax + pp_gt50 + HR.1minDrop,
               data = finalSummarize,
               family=binomial(link="logit"))
  
  # Show summary statistics
  print(summary(models[[2]]))
  
  
  ######
  x <- finalSummarize[,which(names(finalSummarize) %in% 
                               c("Weight","HR.1minDrop","meanDailyMax","pp_gt100"))]
  
  models[[3]] <- cor(x[sapply(x,is.numeric)], use="complete.obs", method="pearson")
  print(models[[3]])
  
  
  return(models)
}

