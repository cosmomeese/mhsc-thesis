library(ggplot2)
library(dplyr)

# Data Prep

# GENERATED FROM RUNNING hiddenMarkovModel.R
#nyhaClasses <- c("II","III")
#rescaledMin.steps = 1
#rescaledMax.steps = 301
resolution.xaxis = 0.1
#hmm.activity = ... you sort of need to run hiddenMarkovModel for this or upload a saved copy

# END DATA PREP

shape <- 5;
scale <- 0.5;
ggplot(data.frame(x=c(0,10)), aes(x)) + stat_function(fun=dgamma, args=list(shape, scale))
#ggplot(data.frame(x=c(0:100)), 
#       aes(x)) + geom_point(aes(y=dgamma(x, shape, scale)), colour="red")


#individual step distributions
baseTitle <- "Gamma Distribution Plot"
#so that facets are grouped by NYHAClass (vs Study Identifier#) we need to the factor levels for ggplot

### https://stackoverflow.com/a/8197703
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
####

xaxis.vals <- seq(rescaledMin.steps,rescaledMax.steps,resolution.xaxis)
for(class in nyhaClasses)
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
                                   x=xaxis.vals,
                                   y=do.call(dgamma,c(list(x=xaxis.vals),args))
                        )
  }) %>% bind_rows
  
  plot <- ggplot(funData.plot,
                 aes(x=x,
                     y=y,
                     colour=as.factor(state))) +
    theme_tufte(base_family = "serif", 
                ticks = FALSE) +
    geom_line() +
    scale_x_continuous("Step Count (Steps/minute)", limits=c(rescaledMin.steps,rescaledMax.steps)) +
    scale_y_continuous("Probability", limits=c(0,0.10)) +
    ggtitle(paste(baseTitle," (NYHA ", class, ")", sep = ""))
  
  print(plot)
}