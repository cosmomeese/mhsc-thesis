### Main Function to excecute Hidden Markov Model workflow

## Instructions:
# load up the .RData file containing 'm_cData'
# Run this function

#set these appropriate and optionally run them
hmm.Generator.CodeVersion <- "0.3"


#------------------------------------------------------
## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("dplyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(dplyr)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("unnestStepDataFrame",
             #"confuseMat",
             #"improvedGammaDist",
             #"hmm_applyModels",
             "hmm_cleanData",
             "hmm_convertRawData",
             "hmm_extractMicroSimProperties",
             #"hmm_generateModels",
             "hmm_microSimulate",
             #"hmm_scoreModels",
             "hmm_viewScores",
             "hmm_common"
             )

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
if(exists('srcCreateFcn', mode='function'))
{
  rm(srcCreateFcn) #remove the extra unneeded variables
}

# PRE-CHECKS ##########################################################
# VERIFY ALL THE REQUIRED PIECES ARE IN PLACE FIRST (BELOW)

if(!exists("m_cData"))
{
  stop("\nE: Could not find Data frame 'm_cData'. Make sure Data is loaded")
}

if(!exists("simonData.Importer.CodeVersion"))
{
  warning("\nW: Could not verify data version number. Please make sure Data was properly loaded. \nThere should be a variable 'simonData.Importer.CodeVersion' that exists to specify the data version.")
  
}else
{
  if(suppressWarnings(is.na(as.numeric(simonData.Importer.CodeVersion)))) #suppressWarnings because otherwise it gives an annoying warning message whenever data is not a numeric
  {
    warning(paste("\nW: Could not verify data version number since version is non-numeric."))
    
  } else if(as.numeric(simonData.Importer.CodeVersion) < 1.2)
  {
    stop(paste("\nE: Script does not support loaded m_cData (version v",simonData.Importer.CodeVersion,"). Script requires at least v1.2", sep=""))
    
  }else if(as.numeric(simonData.Importer.CodeVersion) > 1.2)
  {
    warning(paste("\nW: You are using an unverified m_cData version (v",simonData.Importer.CodeVersion,"). Code only tested on v1.2", sep=""))
  }
}

# mhsmm_global.env <- new.env()

#------------------------------------------------------
# SOME TIME SAVING PARAMETERS
SKIP_DATA_FETCH = FALSE
SKIP_MICROSIMULATION = TRUE
SKIP_HMM_TRAIN = FALSE

## Do Science!

if(!SKIP_DATA_FETCH)
{
  # Import/Format
  data.form <- hmm_convertRawData(rawData=m_cData)
  
  # Pre-Process
  data.clean <- hmm_cleanData(data.form); rm(data.form)
  
  data.train <- data.clean; rm(data.clean)
} else { cat("\nM: Skipped loading dataset...") }

stop("Function not developed past this point")

if(!SKIP_HMM_TRAIN)
{
  # Generate Potential Models
  potentialModelList <- hmm_generateModels(data.train)
}

if(!SKIP_MICROSIMULATION)
{
  # Microsimulate test data
  stat_properties <- hmm_extractMicroSimProperties(data.train)
  data.test <- hmm_microSimulate(data.clean, stat_properties); rm(stat_properties); #rm(data.train)
}else
{
  cat("\nM: Skipped microsimluation...")
  data.test <- data.train; rm(data.train)
}

# Apply Models to Each Patient
patientProbability.df <- hmm_applyModels(potentialModelList,data.test)

# Extract Ideal Model
trueClasses <-
modelScores.df <- hmm_scoreModels(patientProbability.df,trueClasses)

# Print out some Info
hmm_viewScores(modelScores.df)

# Clean-up functions & variables (since this is a script)

# compress & save important variables in list of constants used
CONSTANTS <- sapply(c( 'NYHA_CLASS_VEC',
                      'MAX_FINITE_VALUE',
                      'MIN_FINITE_VALUE',
                      'UNSCALEDMAX.STEPS',
                      'UNSCALEDMIN.STEPS',
                      'RESCALEDMAX.STEPS',
                      'RESCALEDMIN.STEPS',
                      'RESCALEFACTOR.STEPS',
                      'NORMALIZED_STUDY_DAY_START'),
                    FUN=get)

rm(list=names(CONSTANTS))  # remove individual variables
rm(DEBUG_LEVEL,DEBUGLVL.ALL,DEBUGLVL.DEBUG)  # remove now irrelevant debug variables
rm(SKIP_DATA_FETCH,SKIP_HMM_TRAIN,SKIP_MICROSIMULATION)  # remove (also) now irrelevant skip variables

rm(list=unlist(fcns)) #remove functions
rm(fcns)
