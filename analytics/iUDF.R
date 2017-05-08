### Import Script for user defined helper functions

###----------- HELPER FUNCTION
importFolder <- function(basePath,folder)
{
  dirPath <- sprintf("%s/%s",basePath,folder)
  sourceDirectory(dirPath, modifiedOnly=TRUE,verbose=TRUE)
}
###----------- END HELPER FUNCTION

# Setup

basePath <- "C:/Users/cosmomeese/Google Drive/Business/Work/University/Degree - Masters/3a - Thesis/code/analytics"

## Generic\
folder <- "userDefinedFcns"
importFolder(basePath,folder)

## Simon's Thesis
#folder <- "simonsThesisFcns"
#importFolder(basePath,folder)


# Cleanup
rm(basePath,folder)

