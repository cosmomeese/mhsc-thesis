cat('started')
x = trainData.class[[class]]
model = start.val
mstep = mstep.dist

K = nrow(model$trans)
if(mode(x)=="numeric" | mode(x)=="integer") {
  warning('x is a primitive vector.  Assuming single sequence.')
  N = NN = NROW(x)
}else{
  N = NN = x$N
  x = x$x
}

if(K<2) stop("K must be larger than one.")	
if(any(dim(model$trans)!=K)) stop("dimensions of a incorrect")
if(length(model$init)!=K) stop("dimensions of st incorrect")
if(NROW(x)!=sum(N)) stop("dimensions incorrect")
if(length(N)==1){ NN=1 
}else NN = length(N)

if(is.null(mstep)) 
{
    if(is.null(model$mstep))
    {
      stop("mstep not specified")
    }
    else
    {
      mstep=model$mstep      
    }
}

f=model$dens.emission

loglik=numeric(maxit)
loglik=
loglik[1]=-Inf
gam = double(K*sum(N))

cat('\nbegin iteration')
for(i in 1:maxit) {
  cat('\niteration ', i)
  p = sapply(1:K,fn <- function(state) f(x,state,model))
  if(any(apply(p,1,max)==0)) stop("Some values have emission pdf=0 for all states!  Check your model parameters")
  if(any(is.na(p)|p==Inf)) stop("The emission pdf returned NA/NaN/Inf for some values, this indicates a problem with the parameter estimation in the M-step has occurred")
  
  #e-step
  estep_out = .C("mo_estep_hmm",a=as.double(t(model$transition)),pi=as.double(t(model$init)),p=as.double(t(p)),
                 N=as.integer(N),nsequences=as.integer(NN), K=as.integer(K),
                 alpha=double((K+1)*sum(N)) ,beta=double(K*sum(N)),gam=gam,ll=double(1),PACKAGE='mhsmm')
  #m-step
  loglik[i]=estep_out$ll                                   
  if(i>1)    if(abs(loglik[i]-loglik[i-1])<tol) break("Converged")
  #    if((loglik[i]-loglik[i-1])<(-tol)) stop(paste("loglikelihood has decreased on iteration",i))
  gam = matrix(estep_out$gam,ncol=K)
  if(any(colSums(gam)==0)) stop("Error: at least one state has an expected number of occurences equal to 0.\n This may be caused by bad starting parameters are insufficent sample size")
  if(any(is.na(gam))) stop("Error: one or more of the state probabilities was NA/NaN. This usually means the model is ill-specified for the data")
  
  if(i == 17)
  {
    cat('break')
  }
    
  if(length(formals(mstep))==2) {
    model$parms.emission = mstep(x,gam)
  }else if(length(formals(mstep))==4) {
    alpha = matrix(estep_out$alpha,ncol=K+1)
    beta = matrix(estep_out$beta,ncol=K)
    model$parms.emission = mstep(x,gam,alpha,beta)
  }else {
    stop("Error: M-step function is invalid.")
  }
  if(!lock.transition) {
    model$transition=matrix(estep_out$a,nrow=K,byrow=TRUE)
    model$init=estep_out$pi
    model$init[model$init<0]=0
    model$transition[model$transition<0]=0
  }
}


ret = list(model=model,K=K,f=f,mstep=mstep,gam=gam,loglik=loglik[!is.na(loglik)],N=N,p=gam,yhat=apply(gam,1,which.max))
class(ret) <- "hmm"
return(ret)	