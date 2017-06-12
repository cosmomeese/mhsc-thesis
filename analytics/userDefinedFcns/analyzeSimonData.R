### Analyze Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Import Required Libraries

#local functions

analyzeSimonData <- function(processedData)
{
  #processedData <- m_cData
  #processedData <- pD_removedNYHA.NAs
   
  ##### START #####
  
  models <- list()
  
  #### GROUPED
  
  ## Everything (too large)
  # Create a linear regression model: DV = NYHAClass, IV = everything that seems even slightly predictive
  #nyha.everything <- glm(formula = NYHAClass ~ Age + Weight + Height + Sex + Exercises + SBP.Resting + DBP.Resting + HR.Resting + O2Sat.Resting + FEV.Resting + FEV.Resting.Percentage + HR.1minDrop + O2Sat + Duration + Watts.Predicted.Percentage + VO2.Predicted.Relative.Percentage + VO2.Peak.Measured.Percentage + StepData.MeanDailyTotalSteps + StepData.MeanDailyMeanSteps + StepData.MeanDailyMaxSteps,
  #                    data = processedData,
  #                    family=binomial(link="logit"))
  
  ## Non-Choice Based Parameters
  # Create a linear regression model: DV = NYHAClass, IV = Age, Weight, Height, Sex
  nyha.age_weight_height_sex <- glm(formula = NYHAClass ~ Age + Weight + Height + Sex,
                    data = processedData,
                    family=binomial(link="logit"))
  
  summary(nyha.age_weight_height_sex) # Show summary statistics
  models[["nyha.age_weight_height_sex"]] <- nyha.age_weight_height_sex
  
  ## Heart parameters
  # Create a linear regression model: DV = NYHAClass, IV = heart parameters
  nyha.heartpar <- glm(formula = NYHAClass ~ SBP.Resting + DBP.Resting + HR.Resting + HR.1minDrop + VO2.Peak.Measured.Percentage + VO2.Peak.Predicted.Percentage,
                    data = processedData,
                    family=binomial(link="logit"))
  
  summary(nyha.heartpar) # Show summary statistics
  models[["nyha.heartpar"]] <- nyha.heartpar
  
  ## Step Parameters
  # Create a linear regression model: DV = NYHAClass, IV = step parameters
  nyha.steppar <- glm(formula = NYHAClass ~ StepData.MeanDailyTotalSteps + StepData.StdDevDailyTotalSteps + StepData.MeanDailyMeanSteps + StepData.StdDevDailyMeanSteps + StepData.MeanDailyMaxSteps + StepData.MaxDailyMaxSteps,
                    data = processedData,
                    family=binomial(link="logit"))
  
  summary(nyha.steppar) # Show summary statistics
  models[["nyha.steppar"]] <- nyha.steppar
  
  #### Singles
  
  ## Mean Daily MAX Steps
  # Create a linear regression model: DV = NYHAClass, IV = Mean Daily Max Steps
  nyha.meanDailyMaxSteps <- glm(formula = NYHAClass ~ StepData.MeanDailyMaxSteps,
                    data = processedData,
                    family=binomial(link="logit"))
  
  summary(nyha.meanDailyMaxSteps) # Show summary statistics
  models[["nyha.meanDailyMaxSteps"]] <- nyha.meanDailyMaxSteps
  
  ## Mean Daily Total Steps
  #Create a linear regression model: DV = NYHAClass, IV = Daily Average Total Steps
  nyha.meanDailyTotalSteps <- glm(formula = NYHAClass ~ StepData.MeanDailyTotalSteps,
                    data = processedData,
                    family=binomial(link="logit"))
  
  summary(nyha.meanDailyTotalSteps) # Show summary statistics
  models[["nyha.meanDailyTotalSteps"]] <- nyha.meanDailyTotalSteps
  
  ##### Heart Rate Drop in 1 Minute
  #Create a linear regression model: DV = NYHAClass, IV = HR.1minDrop
  nyha.hr1minDrop <- glm(formula = NYHAClass ~ HR.1minDrop,
                         data = processedData,
                         family=binomial(link="logit"))
  
  summary(nyha.hr1minDrop) # Show summary statistics
  models[["nyha.hr1minDrop"]] <- nyha.hr1minDrop
  
  ##### Weight Only
  #Create a linear regression model: DV = NYHAClass, IV = Weight
  nyha.weight <- glm(formula = NYHAClass ~ Weight,
                     data = processedData,
                     family=binomial(link="logit"))
  
  summary(nyha.weight) # Show summary statistics
  models[["nyha.weight"]] <- nyha.weight
  
  #### Possible Models
  
  ## Both Daily Step Measures + HR Drop in 1 min
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Max Steps
  nyha.bothSteps_hr_w <- glm(formula = NYHAClass ~ StepData.MeanDailyMaxSteps + StepData.MeanDailyTotalSteps + HR.1minDrop,
                                                  data = processedData,
                                                  family=binomial(link="logit"))
  
  
  summary(nyha.bothSteps_hr_w) # Show summary statistics
  models[["nyha.weight_hr1minDrop_meanDailyMaxSteps_meanDailyTotalSteps"]] <- nyha.bothSteps_hr_w
  
  ##### Daily Total Steps + HR Drop in 1 min + Weight
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Max Steps
  nyha.maxSteps_hr1minDrop_weight <- glm(formula = NYHAClass ~ Weight + StepData.MeanDailyMaxSteps + HR.1minDrop,
                    data = processedData,
                    family=binomial(link="logit"))
  
  
  # Show summary statistics
  summary(nyha.maxSteps_hr1minDrop_weight)
  models[["nyha.maxSteps_hr1minDrop_weight"]] <- nyha.maxSteps_hr1minDrop_weight
  
  ##### Total Steps + HR Drop in 1 min
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  nyha.totalSteps_hr1minDrop <- glm(formula = NYHAClass ~ StepData.MeanDailyTotalSteps + HR.1minDrop,
                    data = processedData,
                    family=binomial(link="logit"))
  
  # Show summary statistics
  summary(nyha.totalSteps_hr1minDrop)
  models[["nyha.totalSteps_hr1minDrop"]] <- nyha.totalSteps_hr1minDrop
  
  ##### Weight + Daily Steps
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  nyha.weight_meanDailyTotalSteps <- glm(formula = NYHAClass ~ Weight + StepData.MeanDailyTotalSteps,
                    data = processedData,
                    family=binomial(link="logit"))
  
  # Show summary statistics
  summary(nyha.weight_meanDailyTotalSteps)
  models[["nyha.weight_meanDailyTotalSteps"]] <- nyha.weight_meanDailyTotalSteps
  
  ##### Weight + HR Drop in 1 min + Daily Steps
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  nyha.weight_hr1minDrop_meanDailyTotalSteps <- glm(formula = NYHAClass ~ Weight + StepData.MeanDailyTotalSteps + HR.1minDrop,
                                                    data = processedData,
                                                    family=binomial(link="logit"))
  
  # Show summary statistics
  summary(nyha.weight_hr1minDrop_meanDailyTotalSteps)
  models[["nyha.weight_hr1minDrop_meanDailyTotalSteps"]] <- nyha.weight_hr1minDrop_meanDailyTotalSteps
  
  ##### HR Drop in 1 min + Mean Max Steps
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  nyha.meanDailyMaxSteps_hr1minDrop <- glm(formula = NYHAClass ~ StepData.MeanDailyMaxSteps + HR.1minDrop,
                                           data = processedData,
                                           family=binomial(link="logit"))
  
  # Show summary statistics
  summary(nyha.weight_meanDailyTotalSteps)
  models[["nyha.weight_meanDailyTotalSteps"]] <- nyha.weight_meanDailyTotalSteps
  
  ##### Weight + Mean Max Steps
  #Create a linear regression model: DV = NYHAClass, IV = Weight, Daily Average Total Steps
  nyha.weight_meanDailyMaxSteps <- glm(formula = NYHAClass ~ Weight + StepData.MeanDailyMaxSteps,
                    data = processedData,
                    family=binomial(link="logit"))
  
  # Show summary statistics
  summary(nyha.weight_meanDailyMaxSteps)
  models[["nyha.weight_meanDailyMaxSteps"]] <- nyha.weight_meanDailyMaxSteps
  
  
  
  return(models)
}

