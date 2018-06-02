### Data Analysis Common Definitions & Functions

# SOME BASIC DEFINITIONS

## DEBUG LEVELS

# compress & save important variables in list of constants used

################################################################################
# Common Helper Functions


addBMIColumn <- function(fullData) {
  
  # BMI = weight[kg] / (height[m]^2)
  fullData <- fullData %>% mutate(BMI = Weight / ((Height/100)^2))
  return(fullData)
  
}

addMissingClassGroupings <- function(fullData) {
  #### Clean up factor levels ###########################
  
  ## Factor Levels
  # All factors with dirty factor names
  FACTOR_LEVELS <- list()
  FACTOR_LEVELS$ALL <- levels(fullData$NYHAClassMixed)
  # Factors where the given class occurs in any of the dirty factor level names, including as a mixed class
  FACTOR_LEVELS$ANY1 <- grep("(\\b(1|I)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY2 <- grep("(\\b(2|II)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY3 <- grep("(\\b(3|III)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY4 <- grep("(\\b(4|IV)\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  # Factors where the given class occurs in any of the dirty factor level names, but not as a mixed class
  FACTOR_LEVELS$PURE1 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY1,FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4)
  FACTOR_LEVELS$PURE2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1)
  FACTOR_LEVELS$PURE3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2)
  FACTOR_LEVELS$PURE4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3)
  # Factor levels used in original Raghad analysis where e.g. I/II or II are considered class II, II/III or III => III, III/IV or IV => IV
  FACTOR_LEVELS$ROUNDDOWN2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1)
  FACTOR_LEVELS$ROUNDDOWN3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$ROUNDDOWN4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2),FACTOR_LEVELS$PURE3)
  # Factor levels where class is mixed I/II, II/III, III/IV
  FACTOR_LEVELS$MIXED12 <- setdiff(FACTOR_LEVELS$ROUNDDOWN2,FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$MIXED23 <- setdiff(FACTOR_LEVELS$ROUNDDOWN3,FACTOR_LEVELS$PURE3)
  FACTOR_LEVELS$MIXED34 <- setdiff(FACTOR_LEVELS$ROUNDDOWN4,FACTOR_LEVELS$PURE4)
  
  
  #### Add MixedNYHAClass (already in Simon Data) #######
  
  # Add NYHAClass, i.e. those using rounddown technique
  # already done (manually) in Simon's data set (in fact we're used it above as the 'cleaned up' factor names
  
  factorLevels2Index <- function(levels)
  {
    temp <- !is.na(factor(fullData$NYHAClassMixed, levels=levels))
    return(temp)
  }
  
  #### Add PureNYHAClass ################################
  
  # Add PureNYHAClass, i.e. just those with true class II and true class III, etc.
  pureNYHAClassVector <- vector(mode="character",nrow(fullData))
  pureFactorIndicies <- factorLevels2Index(c(FACTOR_LEVELS$PURE1,
                                             FACTOR_LEVELS$PURE2,
                                             FACTOR_LEVELS$PURE3,
                                             FACTOR_LEVELS$PURE4))  # get indices for NYHA Classes that are pure 1, 2, 3 or 4 only
  pureNYHAClassVector[pureFactorIndicies] <- as.character(fullData$NYHAClass[pureFactorIndicies])  # select/copy the 'cleaned up' factor names
  fullData$PureNYHAClass <- factor(pureNYHAClassVector, levels=levels(fullData$NYHAClass))  # convert to factor
  rm(pureFactorIndicies,pureNYHAClassVector)
  
  #### Add ExplicitNYHAClass ############################
  
  # Add ExplicitNYHAClass, i.e. those with class I, I/II, II, II/III, III, III/IV, IV
  explicitNYHAClassVector <- vector(mode="character",nrow(fullData))
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE1))] <- "I"  # populate pure I
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED12))] <- "I/II"  # populate mixed I/II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE2))] <- "II"  # populate pure II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED23))] <- "II/III"  # populate mixed II/III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE3))] <- "III"  # populate pure III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED34))] <- "III/IV"  # populate mixed III/IV
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE4))] <- "IV"  # populate pure IV
  fullData$ExplicitNYHAClass <- factor(explicitNYHAClassVector,levels=c("I","I/II","II","II/III","III","III/IV","IV"))
  rm(explicitNYHAClassVector)
  
  # Remove participants where we have missing important (see step) data
  warning("Forcing HF018 to NA since HF018 has no step data - come up with a better way to do this if using different data set")
  fullData$PureNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # drop HF018 since they don't have step data
  fullData$ExplicitNYHAClass[fullData$StudyIdentifier == "HF018"] <- NA # as above
  
  return(fullData)
}

createViewableFullData <- function(fullData)
{
  viewableFullData <- fullData; 
  viewableFullData$StepData <- NULL  # drop StepData since this is what causes the problem
  return(viewableFullData)
}
