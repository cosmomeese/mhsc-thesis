### Plot Gamma Distribution (should auto plot state distributions of HMMs stored in hmm.activity)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","dplyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

library(ggplot2)
library(dplyr)

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","improvedGammaDist.R",
             sep=""))
rm(sourceDir)

#######################################################################

# Data Prep

# GENERATED FROM RUNNING hiddenMarkovModel.R
#NYHA_CLASS_VEC <- c("II","III")
#RESCALEDMIN.STEPS = 1
#RESCALEDMAX.STEPS = 301
RESOLUTION.XAXIS = (RESCALEDMAX.STEPS - RESCALEDMIN.STEPS) * 0.01
#hmm.activity = ... you sort of need to run hiddenMarkovModel for this or upload a saved copy

# END DATA PREP

# EXAMPLE GAMMA DISTRIBUTION
#shape <- 1.20417*(10^(64));  # recall  for large k approximate normal w/ mu = k*theta
#scale <- 1.486*(10^(-62));  # recall for large k approximate normal w/ sigma = sqrt(k)*theta
#range = c(178.939662:178.939663)
shape = 10
scale = 31.62778
range = c(1,300)
mean <- shape*scale
stdev <- sqrt(shape)*scale
ggplot(data.frame(x=range), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=mean, sd=stdev), colour="red") +
  stat_function(fun=dgamma, args=list(shape=shape, scale=scale), linetype = 2)
  
#ggplot(data.frame(x=c(0:300)), aes(x)) +
#  stat_function(fun=dgamma, args=list(shape=shape, scale=scale))
#ggplot(data.frame(x=c(0:100)), 
#       aes(x)) + geom_point(aes(y=dgamma(x, shape, scale)), colour="red")
# EXAMPLE GAMMA DISTRIBUTION

#individual step distributions
baseTitle <- "Gamma Distribution Plot"
#so that facets are grouped by NYHAClass (vs Study Identifier#) we need to the factor levels for ggplot

### https://stackoverflow.com/a/8197703
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
####

xaxis.vals <- seq(RESCALEDMIN.STEPS,RESCALEDMAX.STEPS,RESOLUTION.XAXIS)
for(class in NYHA_CLASS_VEC)
{
  
  nStates <- hmm.activity[[class]]$model$J
  ePars <- hmm.activity[[class]]$model$parms.emission
  #ePars$shape <- c(1,100,200)
  #ePars$scale <- c(1,1,1)
  
  paramsDF <- data.frame(state = 1:nStates,
                         shape = ePars$shape,
                         scale = ePars$scale)
                         
  

  funData.plot <- lapply(1:nrow(paramsDF), function(i) {
                        args <- paramsDF[i,-1]
                        data.frame(state=paramsDF$state[i],
                                   x=xaxis.vals/RESCALEFACTOR.STEPS,
                                   y=do.call(dgammaPlus,c(list(x=xaxis.vals),args))
                        )
  }) %>% bind_rows
  
  plot <- ggplot(funData.plot,
                 aes(x=x,
                     y=y,
                     colour=as.factor(state))) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    geom_line() +
    scale_x_continuous("Step Count (Steps/minute)", limits=c(UNSCALEDMIN.STEPS,UNSCALEDMAX.STEPS)) +
    scale_y_continuous("Probability", limits=c(0,1)) +
    ggtitle(paste(baseTitle," (NYHA ", class, ")", sep = ""))
  
  print(plot)
}