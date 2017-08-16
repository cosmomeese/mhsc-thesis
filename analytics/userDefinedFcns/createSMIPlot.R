### Script to create Signal Mirror Image Plot from Simon Bromberg's Thesis Data (Fitbit/CPS Data)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tcltk","ggplot2","ggthemes","pryr","devtools","bigmemory","progress")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tcltk)
library(ggplot2)
library(ggthemes)
library(bigmemory)
library(progress)#bar
library(pryr) #for memory management
library(devtools)

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","unnestStepDataFrame.R",
             sep=""))
rm(sourceDir)

###Initial Setup #--------------------------

if(exists("m_cData")){

  unnestedDF <- unnestStepDataFrame(m_cData)
  
  noZeroes <- unnestedDF %>% filter(Steps > 0)
  maxSteps <- max(noZeroes$Steps,na.rm = TRUE)
  
  charOffset = as.integer(-127) #offset to make a char (is this needed?)
  
  #normalize range (using this method: rescaling steps from 0-180 to allow for 
  toRange = c(0,-charOffset)
  fromRange = 2*c(0,maxSteps)
  class2 <- noZeroes %>% group_by(StudyIdentifier,Day,Time,NYHAClass) %>%
                          filter(NYHAClass == 'II') %>%
                          .$Steps %>% scales::rescale(to=toRange,from=fromRange) %>% as.integer 
  
  class3 <- noZeroes %>% group_by(StudyIdentifier,Day,Time,NYHAClass) %>% 
                          filter(NYHAClass == 'III') %>% 
                          .$Steps %>% scales::rescale(to=toRange,from=fromRange)  %>% as.integer
  
  #clear some space in memory
  rm(noZeroes,toRange,fromRange)
  
  cat("\nCreating diff matrix\n")
  require("bigmemory")
  big.diffMatrix <- big.matrix(length(class2), length(class3), type="char", init=NA, shared=TRUE) #NOTE char must be signed - therefore will be between -128 to 127 (hence apply offset))
  cat("\nCreated diff matrix\n")
  
  
  ###Parallel Fun Starts #--------------------------
  cat("\nPopulating diff matrix\n")
  require("parallel")
  xVec <- class2
  yVec <- class3
  
  #clear some space in memory
  rm(class2,class3)
  
  #Create progress bar
  require("progress")
  pb <- progress_bar$new(
    format = "  populating [:bar] :current / :total in :elapsed (ETA ~:eta)",
    total = length(yVec), clear = FALSE, width= 60)
  
  options(bigmemory.typecast.warning=FALSE) #suppress warnings 
  for(j in 1:length(yVec))
  {
    pb$tick()
    xVecWOffset <- xVec - yVec[[j]]
  
    big.diffMatrix[,j] <- xVecWOffset
  }
  options(bigmemory.typecast.warning=TRUE) #renable suppress warnings 
  cat("\nPopulated diff matrix\n")
  
  ###Cleanup #--------------------------
  cat("\nWriting out big.matrix to file (check for dialog behind RStudio GUI)\n")
  filepath <- tcltk::tk_choose.dir(getwd(),"Choose directory in which to save big.matrix (or cancel to cancel writing)")
  fullfilepath <- file.path(filepath, "bigDiffMatrix.txt")
  if(0 == length(filepath))
  {
    cat("\nDidn't write out big.matrix (USER CANCELLED OUT)\n")
  }else #must be in front of curly brace - thanks R!
  {
    cat("\nStarted writing big.matrix to:\n\t", fullfilepath)
    filebacked.matrix <- deepcopy(x=big.diffMatrix,
                                  backingfile="bigDiffMatrix.bigMat",
                                  backingpath=filepath,
                                  descriptorfile="bigDiffMatrix.desc")
    cat("\nSuccess! Completed writing out big.matrix")
  }
  #rm(big.diffMatrix)
}else
{
  cat("\nImport m_cData object before running this script\nSimonData.Importer V1.2\nSimonExtraMetrics V1.0\n")
}