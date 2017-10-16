### Determine Data Distribution Script

## NOTE!!! MAKE SURE TO LOAD DISTRIBUTION BEFORE RUNNING SCRIPT (MODIFY BELOW):
dataVector <- testSet[[1]]$StepData[[1]]$Intraday[[3]]$Steps

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("fitdistrplus","logspline")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(fitdistrplus)
library(logspline)

descdist(dataVector, discrete = FALSE, boot = 500)

readline(prompt="Press [enter] to continue")

descdist(dataVector, discrete = TRUE, boot = 500)