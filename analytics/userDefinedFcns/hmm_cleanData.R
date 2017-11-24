### Clean Raw Data for Hidden Markov Model Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c()
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
#library()

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("hmm_common")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

# -------------------------------------------------------

# global parameters

# local functions
hmm_cleanData <- function(formattedData)
{
  # clean data here
  return(formattedData)
}