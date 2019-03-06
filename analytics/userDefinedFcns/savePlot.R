### ggplot function

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("tidyverse","glue")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(glue)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("removeInvalidFileNameChars")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
#rm(srcCreateFcn) #remove the extra unneeded variables

# START -------------------------------------------------------

# N.B. print out plot before using savePlot
savePlot <- function(fileName,
                     filePrefix="plots/",
                     plot,
                     isGGPlot=FALSE,
                     height=8,
                     width=11,
                     units="in",
                     res.dpi = 300)
{
  fileName <- removeInvalidFileNameChars(fileName)
  fullFileName <- glue('{filePrefix}{fileName}')
  
  #create directory
  tryCatch({
    dir.create(file.path(getwd(), dirname(fullFileName)),
               recursive = TRUE)
  }, warning = function (war) {
    
    if(!endsWith(war$message,"already exists"))
    {
      warning(war)
    }
    
  }, error = function(err) {
    stop(err)
  }, finally = {
  }) # END tryCatch
  
  if(isGGPlot)
  {
    ggsave(fullFileName,
           plot=plot,
           path = getwd(),
           height = height,
           width = width,
           units = units,
           dpi = res.dpi)
    cat("Printed ",fullFileName,"\n",sep="")
  }
  else
  {
    tryCatch({
      # create title
      dev.copy(png,
               fullFileName,
               width = width,
               height = height,
               units = units,
               res = res.dpi)
      cat("Printed ",fullFileName,"\n",sep="")
    },error = function(err) {
      cat("\n")
      print(glue("Failed at printing:
                         {fullFileName}"))
      stop(err)
    }, finally = {
      dev.off()
    })
  }
}

# END ------------------------------------------------------- 