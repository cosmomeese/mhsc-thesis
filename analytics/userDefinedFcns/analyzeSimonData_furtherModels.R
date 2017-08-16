### Analyze Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
rm(sourceDir)

#local functions

analyzeSimonData_furtherModels <- function(processedData)
{
  #processedData <- m_cData
  #processedData <- pD_removedNYHA.NAs
   
  ##### START #####
  
  models <- list()
  
  unnested <- unnestStepDataFrame(m_cData)
  
  unnested <- unnested %>% group_by(StudyIdentifier,NYHAClass,Day,HR.1minDrop,Weight,Height)
  
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
    group_by(StudyIdentifier,NYHAClass,HR.1minDrop,Weight,Height) %>% 
    summarize(meanDailyMax = mean(dailyMax), 
              nn_gt0 = sum(n_gt0), 
              pp_gt0 = sum(p_gt0) / n(), #NOTE: we can only do this since they all use the same base
              nn_gt1 = sum(n_gt1), 
              pp_gt1 = sum(p_gt1) / n(),
              nn_gt50 = sum(n_gt50), 
              pp_gt50 = sum(p_gt50) / n(),
              nn_gt100 = sum(n_gt100),
              pp_gt100 = sum(p_gt100) / n())
  
  finalSummarize <- mutate(finalSummarize, BMI = Weight / (Height / 100)^2)
  
  ##### meanDailyMax (steps) + HR Drop in 1 Min
  #Create a linear regression model
  models[[1]] <- glm(formula = NYHAClass ~ meanDailyMax + HR.1minDrop,
               data = finalSummarize,
               family=binomial(link="logit"))
  
  # Show summary statistics
  print(summary(models[[1]]))
  
  ##### meanDailyMax (steps) + HR Drop in 1 Min + Weight
  #Create a linear regression model
  models[[2]] <- glm(formula = NYHAClass ~ meanDailyMax + HR.1minDrop + Weight,
               data = finalSummarize,
               family=binomial(link="logit"))
  
  # Show summary statistics
  print(summary(models[[2]]))
  
  ##### meanDailyMax (steps) + HR Drop in 1 Min + BMI
  #Create a linear regression model
  models[[3]] <- glm(formula = NYHAClass ~ meanDailyMax + HR.1minDrop + BMI,
                     data = finalSummarize,
                     family=binomial(link="logit"))
  
  # Show summary statistics
  print(summary(models[[3]]))
  
  
  ######
  x <- finalSummarize[,which(names(finalSummarize) %in% 
                               c("BMI","Height","Weight","HR.1minDrop","meanDailyMax","pp_gt100"))]
  
  models[[4]] <- cor(x[sapply(x,is.numeric)], use="complete.obs", method="pearson")
  print(models[[4]])
  
  
  return(models)
}

