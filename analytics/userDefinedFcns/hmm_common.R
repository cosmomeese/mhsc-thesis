### Hidden Markov Model Common Definitions & Functions

# SOME BASIC DEFINITIONS
CONSTANTS <- list() # declare
CONSTANTS$NYHA_CLASS_VEC = as.factor(c("II","III"))

## DEBUG LEVELS
DEBUG_LEVEL <- 4
#DEBUGLVL.ERR <- 1
#DEBUGLVL.WARN <- 2
DEBUGLVL.INFO <- 3
DEBUGLVL.DEBUG <- 4
DEBUGLVL.ALL <- DEBUGLVL.DEBUG + 1


# compress & save important variables in list of constants used
CONSTANTS$MIN_FINITE_VALUE <- .Machine$double.xmin #https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html
CONSTANTS$MAX_FINITE_VALUE <- .Machine$double.xmax #https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html

CONSTANTS$UNSCALEDMIN.STEPS <- 0
CONSTANTS$UNSCALEDMAX.STEPS <- 300  # assume max value is 255 per minute
CONSTANTS$RESCALEDMIN.STEPS <- CONSTANTS$MIN_FINITE_VALUE #1 OR + 1/(-1 + UNSCALEDMAX.STEPS - UNSCALEDMIN.STEPS)
CONSTANTS$RESCALEDMAX.STEPS <- 1 #301
CONSTANTS$RESCALEFACTOR.STEPS <- (CONSTANTS$RESCALEDMAX.STEPS - CONSTANTS$RESCALEDMIN.STEPS) / (CONSTANTS$UNSCALEDMAX.STEPS - CONSTANTS$UNSCALEDMIN.STEPS)

CONSTANTS$NORMALIZED_STUDY_DAY_START <- as.POSIXct("9000-01-01 00:00:00", tz="UTC")
# CONSTANTS <- sapply(c( 'NYHA_CLASS_VEC',
#                        'MAX_FINITE_VALUE',
#                        'MIN_FINITE_VALUE',
#                        'UNSCALEDMAX.STEPS',
#                        'UNSCALEDMIN.STEPS',
#                        'RESCALEDMAX.STEPS',
#                        'RESCALEDMIN.STEPS',
#                        'RESCALEFACTOR.STEPS',
#                        'NORMALIZED_STUDY_DAY_START'),
#                     FUN=get)
# 
# rm(list=names(CONSTANTS))  # remove individual variables

################################################################################
# Common Helper Functions
# from: https://stackoverflow.com/a/32685034
stat.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# returns true/false if given the global DEBUG_LEVEL variable you should perform the action given the specified debug threshold level
debugGivenThreshold <- function(threshold) {
  return(threshold <= DEBUG_LEVEL)
}

# for depmixS4 if you want to use multiple sequences you must specify length of each sequence. This extracts that from the dataframe using assuming they are each uniquely labelled in the column col
getSequenceLengths <- function(df,col)
{
  ## Extract the series lengths
  ntimes <- as.numeric(table(df[col]))  # get table (and length of that table) of each patient sequence in set
  ntimes <- ntimes[ntimes != 0]  # drop zero length vectors of each patient sequence in set (i.e. patients not in set)
  return(ntimes)
}

# bins training or test data given the specified interval. 
# if a column contains all NAs it is summarized as NA, otherwise the default na.action occurs
# if all columns contain all NAs provides error
binData <- function(dataToBin, interval)
{
  # https://stackoverflow.com/a/13915931
  
  formul <- as.formula(". ~ StudyIdentifier + DateTime")
  common_colNames <- c('StudyIdentifier','DateTime')
  
  NCLASS_COLNAME <- 'NYHAClass'
  STEPS_COLNAME <- 'Steps'
  HR_COLNAME <- 'HeartRate'
  uniqueColList <- list(dplyr::first,
                        base::sum,
                        base::mean)  # N.B. ORDER MATTERS! (match to below)
  names(uniqueColList) <- c(NCLASS_COLNAME,STEPS_COLNAME,HR_COLNAME) # N.B. ORDER MATTERS! (match to above)
  
  dataToBin.cut <- dataToBin
  ##### BREAK IS NA in dataToBin.cut[ , which(....)]
  #stop("breaks if NA in dataToBin.cut[, which(....)]")
  dataToBin.cut$Steps[dataToBin.cut$Steps <= CONSTANTS$MIN_FINITE_VALUE] <- 1/CONSTANTS$UNSCALEDMAX.STEPS
  dataToBin.cut$DateTime <- dataToBin.cut %>% group_by(StudyIdentifier) %>% .$DateTime %>% cut(interval)
  
  
  # find the column with NA's
  naRows <- dplyr::summarise_all(data.frame(is.na(dataToBin.cut)),
                                 fcn <- function(x){all(x==TRUE)})  # summarize columns
  naRows.dirty <- dplyr::select(naRows,
                                names(naRows)[naRows[1,]==TRUE])  # keep only the columns w/ na's
  naRows.clean <- dplyr::select(naRows,
                                names(naRows)[naRows[1,]==FALSE])  # keep only the columns w/ na's
  naRows.cleanNoCommon <- dplyr::select(naRows.clean,which(!(names(naRows.clean) %in% common_colNames)))
  
  # verify there are no dirty common columns (which will cause misalignments or other bad things - so we stop user from doing this)
  naRows.dirtyCommon <- dplyr::select(naRows.dirty, which(names(naRows.dirty) %in% common_colNames))
  if(ncol(naRows.dirtyCommon))
  {
    stop(paste('dataToBin.cut contains NAs in column ', names(naRows.dirtyCommon), ' which is a common column and therefore is not permitted to contain NAs\n',sep=""))
  }
  
  firstCol <- TRUE
  for(colName in names(naRows.cleanNoCommon))  # only do the clean columns
  {
    uniqueColName <- colName # extract a 'unique' clean column name
    fun <- uniqueColList[[uniqueColName]]  # find the corresponding function
    
    # pull only the relevant columns: the unique clean column + the common columns
    inputData <- dataToBin.cut[ , which(names(dataToBin.cut) %in% c(uniqueColName,common_colNames))]
    
    intermediateData <- aggregate(formul,
                                  data=inputData,
                                  FUN=fun)
    
    if(firstCol) # first time just create (no need to merge)
    {
      finalData <- intermediateData
      firstCol <- FALSE
    }else   # all other times merge intermediate data with final data
    {

      finalData <- intermediateData %>% merge(y=finalData, by = common_colNames, all.y = TRUE)
    }
  }
  if(firstCol)
  {
    # if first column is not initialized implication is that finalData was not created therefore there are no clean unique columns (therefore all NA except for common columns)
    stop("Suspect that provided dataToBin contains no columns without NA's (except possibly the common columns ", 
         paste(common_colNames,collapse=", "),
         ")")
  }
  for(colName in names(naRows.dirty))
  {
    finalData[colName] <- NA
  }
  
  return(finalData)
}
