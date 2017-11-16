### Plot Gamma Distribution (should auto plot state distributions of HMMs stored in hmm.activity)

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("ggplot2","dplyr","tidyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

library(ggplot2)
library(dplyr)
library(tidyr)

sourceDir <- "userDefinedFcns"
source(paste(sourceDir,"/","improvedGammaDist.R",
             sep=""))
rm(sourceDir)

#######################################################################

### Data Prep ####

# GENERATED FROM RUNNING hiddenMarkovModel.R
#NYHA_CLASS_VEC <- c("II","III")
#RESCALEDMIN.STEPS = 1
#RESCALEDMAX.STEPS = 301
RESOLUTION.XAXIS = (RESCALEDMAX.STEPS - RESCALEDMIN.STEPS) * 0.01
#hmm.activity = ... you sort of need to run hiddenMarkovModel for this or upload a saved copy

# END DATA PREP

#### EXAMPLE GAMMA DISTRIBUTION ####
#shape <- 1.20417*(10^(64));  # recall  for large k approximate normal w/ mu = k*theta
#scale <- 1.486*(10^(-62));  # recall for large k approximate normal w/ sigma = sqrt(k)*theta
#range = c(178.939662:178.939663)
shape = 30
scale = 1
range = c(1,300)
mean <- shape*scale
stdev <- sqrt(shape)*scale
ggplot(data.frame(x=range), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=mean, sd=stdev), colour="red") +
  stat_function(fun=dgamma, args=list(shape=shape, scale=scale), linetype = 2)
rm(shape,scale,range,mean,stdev)
# END PLOTTING EXAMPLE GAMMA DISTRIBUTION


#### HMM EMISSION DISTRIBUTIONS ####
#shape = c(0.000333, 0.533333, 6.533333)
#scale = c(6.09*10^(-0.5), 9.74*10^(-2), 1.19)
initialStepGuess.means <- c(1,40,100) * RESCALEFACTOR.STEPS
initialStepGuess.variances <- c(1,80,1000) * (RESCALEFACTOR.STEPS^2)
#initialStepGuess.stdevs <- sqrt(initialStepGuess.variances)

# recall that you can define your own functions
INIT.EMIS <- list(shape = (initialStepGuess.means^2)/(initialStepGuess.variances),  # for large k, k = (mu/sigma)^2
                  scale = (initialStepGuess.variances)/(initialStepGuess.means),  # for large k, theta = (sigma^2)/mu
                  type = "gamma")  # for gamma distribution
shape = INIT.EMIS$shape
scale = INIT.EMIS$scale
range = UNSCALEDMIN.STEPS:UNSCALEDMAX.STEPS * RESCALEFACTOR.STEPS
mean <- shape*scale
stdev <- sqrt(shape)*scale

datalist <- list() # https://stackoverflow.com/a/29419402
for(i in 1:min(length(mean),length(stdev)))
{
  dat <- data.frame(x=range/RESCALEFACTOR.STEPS,value=dgammaPlus(range,shape=shape[[i]],scale=scale[[i]]))
  dat$state <- i # maybe you want to keep track of which iteration produced it
  #dat$param <- paste("Shape=",shape[[i]]," Scale=",scale[[i]],sep="")
  datalist[[i]] <- dat # add it to your list
}

values <- do.call(rbind, datalist)
values$state <- as.factor(values$state)

ggplot(data=values,
       aes(x=x, y=value, colour=state)) +
  geom_line(alpha=1) +
  scale_x_continuous("Step Count (Steps/minute)")#, limits=c(UNSCALEDMIN.STEPS,UNSCALEDMAX.STEPS))
#### END PLOT EMISSION DISTRIBUTIONS


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

# Plotting Approximations of Gamma Function ###########################

# https://stackoverflow.com/a/3777592
# setup constants
x=0:100/100
shape = 10^(-100)
scale = 1

# compute approximation ####
const <- 1/gamma(shape + 1)
omega <- shape/(exp(1)*(1-shape))
lambda <- (1/shape) - 1

# compute envelope function (scaled to 1)
#posX <- x[x>=0]
#negX <- x[x<0]
#envelopeFcn = numeric(length(x))
#envelopeFcn[x>=0] <- const*exp(-posX)
#envelopeFcn[x<0] <- const*omega*lambda*exp(lambda*negX)

# scale envelope function (to scale)
#result <- (1/(scale^shape))*envelopeFcn^(1/scale)
#result <- 10^(envelopeFcn+shape)
result <- 1/(1+omega)*dexp(x,rate=1) + omega*(1+omega)*(dexp(-x,rate=lambda))
result <- exp(result+shape)
# end compute approximation

# compute true
result2 = dgamma(x,shape=shape,scale=scale)

# gather into a format so we can plot
z <- data.frame(x=x,approx=result,true=result2) %>%
        gather(type,value,approx:true)

ggplot(data=z,
       aes(x=x, y=value, colour=type)) +
    geom_line(alpha=0.7)