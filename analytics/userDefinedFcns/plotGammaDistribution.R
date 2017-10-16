library(ggplot2)
shape = 5;
scale = 0.5;
ggplot(data.frame(x=c(0,10)), aes(x)) + stat_function(fun=dgamma, args=list(shape, scale))
#ggplot(data.frame(x=c(0:100)), 
#       aes(x)) + geom_point(aes(y=dgamma(x, shape, scale)), colour="red")