
# improved gamma density function
# estimates gamma distribution (approximate with normal if shape (k) is high, and approximate with exponential based function if shape (k) is low from)
# refer to https://arxiv.org/pdf/1302.1884.pdf to background

UPPER_APPROX_THRESHOLD <- 30*10^(2)  # normal limit theorem = 30 w/ 2 orders of magnitude for error (eng. safety factor + Fermi Approx)
LOWER_APPROX_THRESHOLD <- 0.02 # one order of magnitude less than code from: https://arxiv.org/pdf/1302.1884.pdf


## IMPROVED GAMMA
dgammaPlus <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE)
{

  result <- tryCatch({
    
    if(shape > UPPER_APPROX_THRESHOLD)
    {
      mean <- shape*scale
      stdev <- sqrt(shape)*scale
      result <- dnorm(x, mean=mean, sd=stdev, log=log)
    }
    else if (shape < LOWER_APPROX_THRESHOLD)
    {
      # https://arxiv.org/pdf/1302.1884.pdf
      lambda <- shape^(-1) - 1
      omega <- shape/(exp(1)*(1-shape))
      fac <- 1/(1+omega)
      result <- fac*dexp(x, rate=1) - omega*fac*dexp(x,rate=lambda)
      if(log)
      {
        result <- log(result)
      }
    }
    else
    {
      result <- dgamma(x, shape=shape,scale=scale, log=log)
    }
    result  # return (but don't wrap in a return since this isn't a function --- confusing. thanks R)
    
  }, warning = function(war) {
    # warning handler picks up where error was generated
    
    #print(paste("MY_WARNING:  ",war))  # suppress our printing of warning
    
    if("NaNs produced" == war$message)
    {
      # as scale -> 0 the gamma distribution begins to approximate a normal distribution
      # the result at sd=scale=0 is a Infinite distribution at mean and 0 everywhere else
      #result <- dnorm(x, mean=RESCALEDMIN.STEPS,sd=model$parms.emission$scale[j])
      #maxDensity = RESCALEDMAX.STEPS-RESCALEDMIN.STEPS+1
      #result[result > maxDensity] <- maxDensity #cap the maximum probability (to get rid of Inf which causes problems for hmmfit). We use the maximum density per unit permitted based on range of acceptable step values.
      #return(result)
      war$message = paste(war$message, "shape=", shape, "scale=", scale,sep="")
    }
    # no other special handling; throw error
    warning(war)
    
  }, error = function(err) {
    # error handler picks up where error was generated
    # no special handling; throw error
    stop(err)
  }, finally = {
    # finally code block will run regardless of error occurance (therefore can't use err or war here)
    
  }) # END tryCatch
  
  # determine if parameters are invalid (and let the user know)
  if(any(is.na(result)))
  {
    printDebug = TRUE
    cat(" = INVALID")  #N.B. will be appended after last line
  }
  else
  {
    cat(" = V")
  }
  result[is.na(result)] = 0  #if it returns an na then it's an invalid result
  
  return(result)
}


rgammaPlus <- function(n, shape, rate = 1, scale = 1/rate)
{
  if(shape > UPPER_APPROX_THRESHOLD)
  {
    mean <- shape*scale
    stdev <- sqrt(shape)*scale
    result <- rnorm(n, mean=mean, sd=stdev)
  }
  else if (shape < LOWER_APPROX_THRESHOLD)
  {
    result <- rgamss(n, shape=shape, scale=scale) #http://www4.stat.ncsu.edu/~rmartin/Codes/rgamss.R (below)
  }
  else
  {
    result <- rgamma(n, shape=shape,scale=scale) 
  }
  return(result)
}

#===============================================================================================

# BELOW IS FROM HERE: https://arxiv.org/pdf/1302.1884.pdf
  
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