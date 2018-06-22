### Analyze Function for Simon Bromberg's Thesis Data (Fitbit/CPS Data)


analyzeSimonData <- function(processedData,suppressPrompt = FALSE)
{
  
  ## Import Required Libraries
  sourceDir_a <- "userDefinedFcns"
  fcns_generic <- list()
  fcns_analysis <- list("analyzeSimonData_initialModels",
                        "analyzeSimonData_plinioRequestedData")
  
  # Perform actual import
  srcCreateFcn_a <- function(sfcn,sourceDir) #helper function to import
  {
    source(paste(sourceDir,"/",sfcn,".R",
                 sep=""))
  }
  invisible(lapply(append(fcns_generic,fcns_analysis),srcCreateFcn_a,
                   sourceDir=sourceDir_a)) #use function
  rm(srcCreateFcn_a,fcns_generic) #remove the extra unneeded variables
  
  #N.B. THERE'S A BETTER FUNCTION FOR THIS IN analyzeData_common FCN=parseVarName
  #function to split camelcase strings from https://stackoverflow.com/a/8407047
  split_camelcase <- function(...){
    strings <- unlist(list(...))
    strings <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", strings)
    strings <- gsub("(?!^)(?=[[:upper:]])", " ", strings, perl = TRUE)
    return(strsplit(tolower(strings), " ")[[1]])
  }
  
  
  ## Start Function Proper

  beforeString <- "\n\n #################################### "
  afterString <- sapply(lapply(strsplit(beforeString,NULL),rev),paste,collapse="")
  models <- list()
  for(fcnName in fcns_analysis)
  {
    sub <- strsplit(fcnName,"[_]")[[1]][-1] #get list for 1st string and rop first element
    sub <- split_camelcase(sub)
    sub <- toupper(paste(sub,collapse = " ")) #combine all into string and make uppercase
    
    
    cat(beforeString, sub , afterString)
    
    fun <- get(fcnName)
    models[[fcnName]] <- fun(processedData)

    if(!suppressPrompt)
    {
      invisible(readline(prompt="Press [enter] to continue")) #invisible so it doesn't print output to console
    }
  }
  
  cat(beforeString, "END (ANALYZED WITH " , length(fcns_analyis), " FUNCTIONS", afterString)
  
  return(models)
}

