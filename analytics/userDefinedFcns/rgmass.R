#================================================================================#
# Goal: Sample small-shape gamma random variables via accept-reject              #
# Paper: arXiv:1302.1884                                                         #
# Function: rgamss                                                               #
# Authors: C. Liu, R. Martin (www.math.uic.edu/~rgmartin), and N. Syring         #
# Version: 2nd (04/07/2015)                                                      #
#================================================================================#

# INPUT
# n = sample size
# shape = shape parameter (same for all samples)
# scale = scale parameter (same for all samples; default = 1)
# do.log = logical -- return values on log scale

# OUTPUT
# vector of length n containing gamma samples;
# on log-scale depending on do.log or magnitude of shape

rgamss <- function(n, shape, scale=1, do.log=TRUE) {
  
  a <- shape
  if(a > 0.2) oo <- rgamma(n = n, shape = a, rate = 1 / scale) else {
    
    e1 <- 2.71828182845905
    L <- 1 / a - 1
    w <- a / e1 / (1 - a)
    ww <- 1 / (1 + w)
    eta <- function(z) if(z >= 0) exp(-z) else w * L * exp(L * z)
    h <- function(z) exp(-z - exp(-z / a))
    rh <- function(a) {
      
      repeat {
        
        U <- runif(1)
        z <- if(U <= ww) -log(U / ww) else log(runif(1)) / L
        if(h(z) / eta(z) > runif(1)) return(z)
        
      }
      
    }
    Z <- numeric(n)
    if(length(n)>1)
    {
      numElementsToGenerate = length(n)
    }
    else
    {
      numElementsToGenerate = n
    }
      
    for(i in 1:numElementsToGenerate) Z[i] <- rh(a)
    o <- log(scale) - Z / a
    if(!do.log) {
      
      oo <- exp(o)
      #if(any(oo == 0)) {
      #  
      #  oo <- o
      #  warning("Output given on log-scale since shape is small")
      #  
      #}
      
    } else oo <- o
    
  }
  return(oo)
  
}