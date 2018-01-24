# tests to see how to use the depmix API to update depmix models 
# below we compare a bunch of different models after toying with
# their parameters to see under which conditions they react; changing or remaining the same


# common data
response <- list(formula("rt~1"),formula("corr~1"))
transition <- formula("~Pacc")
fam <- list(gaussian(),multinomial("identity"))
ntimes <- c(168,134,137)


#### 2 and 3 State Case #####

for(states in 2:3)
{
  cat("State: ",states," - ",sep="")
  modl <- depmix(response,data=speed,transition=transition,nstates=states,
                 family=fam,ntimes=ntimes)
  
  modl.fitted <- fit(modl, verbose=FALSE)
  fbTru <- forwardbackward(modl.fitted,return.all=FALSE)
  cat("True:", fbTru$logLike, sep="")
  
  dupModl <- depmix(response,
                     data=speed,
                     transition=transition,
                     family=fam,
                     nstates=states,
                     ntimes=ntimes)
  dupModl <- setpars(dupModl, getpars(modl.fitted))
  fbDup <- forwardbackward(dupModl,return.all=FALSE)
  cat("\nDuplicate:", fbDup$logLike, sep="")
  
  subModlA <- depmix(response,
                     data=subSpeed,
                     transition=transition,
                     family=fam,
                     nstates=states,
                     ntimes=ntimes[[1]])
  subModlA <- setpars(subModlA, getpars(modl.fitted))
  fbA <- forwardbackward(subModlA,return.all=FALSE)
  cat("\nPatient A:", fbA$logLike, sep="")
  
  subModlB <- depmix(response,
                     data=subSpeedB,
                     transition=transition,
                     family=fam,
                     nstates=states,
                     ntimes=ntimes[[2]])
  subModlB <- setpars(subModlB, getpars(modl.fitted))
  fbB <- forwardbackward(subModlB,return.all=FALSE)
  cat("\nPatient B:", fbB$logLike, sep="")
  
  subModlC <- depmix(response,
                     data=subSpeedC,
                     transition=transition,
                     family=fam,
                     nstates=states,
                     ntimes=ntimes[[3]])
  subModlC <- setpars(subModlC, getpars(modl.fitted))
  fbC <- forwardbackward(subModlC,return.all=FALSE)
  cat("\nPatient C:", fbC$logLike, sep="")
  cat("\n\n")
}
