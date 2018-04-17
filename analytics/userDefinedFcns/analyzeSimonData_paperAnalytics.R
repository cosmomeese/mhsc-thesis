### Function to calculate Demographics + Statistics for Simon Bromberg's Thesis Data (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true

### Generate HMM Models for Hidden Markov Model Script

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("plyr","tidyverse","reshape2","glue", "ggthemes")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)
# load the library
library(tidyverse)
library(ggthemes)  # for tufte themes
library(plyr)
library(dplyr)
library(reshape2)
library(glue)

## Import Required Functions
#sourceDir <- "userDefinedFcns"
#fcns <- list("hmm_common")

# Perform actual import
#srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
#{
#  source(paste(sourceDir,"/",sfcn,".R",
#               sep=""))
#}
#invisible(lapply(fcns,srcCreateFcn,
#                 sourceDir=sourceDir)) #use function
#rm(srcCreateFcn) #remove the extra unneeded variables

# START -------------------------------------------------------

# Prep ================================================


#' Get P Value Significance Star Code
#'
#' \code{getSigCode} returns the significance star code for the input P value
#'
#' @param p.value a numeric (the p.value)
#' @return for
#' p < 0.001: ***
#' p < 0.01 : **
#' p < 0.05 : *
#' p < 0.1  : .
#' p >= 0.1 : (blank space)
#' invalid p: ?
#'
#' #'   
#' @examples 
#' getSigCode(0.049)
#' *
#' 
#' getSigCode(2)
#' ?
#' 
#' getSigCode(-0.01)
#' ?
#' 
#' getSigCode(NA)
#' ?
#' 
getSigCode <- function(p.value)
{
  if(!(is.numeric(p.value) && is.finite(p.value)))
  {
    p.value <- 2  # give it an invalue value
  }
  
  # determine sigCode
  if(       p.value > 1 || 
            p.value < 0)    { sigCode <- '?'
  } else if(p.value < 0.001){ sigCode <- '***'
  } else if(p.value < 0.01) { sigCode <- '**'
  } else if(p.value < 0.05) { sigCode <- '*'
  } else if(p.value < 0.1)  { sigCode <- '.'
  } else {                    sigCode <- ''}
  
  return(sigCode)
}

minMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

removeInvalidFileNameChars <- function(x, isWindowsOS=TRUE)
{
  mask <- "/"
  windowsMask <- "<>:\\\"|?*"
  if(isWindowsOS)
  {
    mask <- glue("{mask}{windowsMask}")
  }
  x <- str_replace_all(x, glue("[[{mask}]]"), " ")
  return(x)
}

parseVarName <- function(x)
{
  # remove StepData. prefix
  x <- sub('StepData.', '', x)
  # replace all the .'s with spaces
  x <- str_replace_all(x, "[[.]]", " ")
  # split camel case
  #x <- trimws(gsub("([[:upper:]])", " \\1", x))
  return(x)
}


#### Add Pure NYHA Class ##############################

if(!exists("m_cData"))
{
  stop("Missing m_cData. Couldn't execute. Load .Rdata file w/ m_cData and retry. \nData File 'ConvertedRawDataForHMM-v0.3-fromSource-v1.2.RData' will do.")

} else {

  FULL_DATA <- m_cData
  
  # Prep ==================================
  
  #### Add BMI ##########################################
  # BMI = weight[kg] / (height[m]^2)
  FULL_DATA <- FULL_DATA %>% mutate(BMI = Weight / ((Height/100)^2))
  
  #### Clean up factor levels ###########################
  
  ## Factor Levels
  # All factors with dirty factor names
  FACTOR_LEVELS <- list()
  FACTOR_LEVELS$ALL <- levels(FULL_DATA$NYHAClassMixed)
  # Factors where the given class occurs in any of the dirty factor level names, including as a mixed class
  FACTOR_LEVELS$ANY1 <- grep("(\\bI\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY2 <- grep("(\\bII\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY3 <- grep("(\\bIII\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  FACTOR_LEVELS$ANY4 <- grep("(\\bIV\\b)",FACTOR_LEVELS$ALL, perl=TRUE, value = TRUE)
  # Factors where the given class occurs in any of the dirty factor level names, but not as a mixed class
  FACTOR_LEVELS$PURE1 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY1,FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4)
  FACTOR_LEVELS$PURE2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1)
  FACTOR_LEVELS$PURE3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2)
  FACTOR_LEVELS$PURE4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$ANY1),FACTOR_LEVELS$ANY2),FACTOR_LEVELS$ANY3)
  # Factor levels used in original Raghad analysis where e.g. I/II or II are considered class II, II/III or III => III, III/IV or IV => IV
  FACTOR_LEVELS$ROUNDDOWN2 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY2,FACTOR_LEVELS$ANY3),FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1)
  FACTOR_LEVELS$ROUNDDOWN3 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY3,FACTOR_LEVELS$ANY4),FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$ROUNDDOWN4 <- setdiff(setdiff(setdiff(FACTOR_LEVELS$ANY4,FACTOR_LEVELS$PURE1),FACTOR_LEVELS$PURE2),FACTOR_LEVELS$PURE3)
  # Factor levels where class is mixed I/II, II/III, III/IV
  FACTOR_LEVELS$MIXED12 <- setdiff(FACTOR_LEVELS$ROUNDDOWN2,FACTOR_LEVELS$PURE2)
  FACTOR_LEVELS$MIXED23 <- setdiff(FACTOR_LEVELS$ROUNDDOWN3,FACTOR_LEVELS$PURE3)
  FACTOR_LEVELS$MIXED34 <- setdiff(FACTOR_LEVELS$ROUNDDOWN4,FACTOR_LEVELS$PURE4)
  
  
  #### Add MixedNYHAClass (already in Simon Data) #######
  
  # Add NYHAClass, i.e. those using rounddown technique
  # already done (manually) in Simon's data set (in fact we're used it above as the 'cleaned up' factor names
  
  factorLevels2Index <- function(levels)
  {
    temp <- !is.na(factor(FULL_DATA$NYHAClassMixed, levels=levels))
    return(temp)
  }
  
  #### Add PureNYHAClass ################################
  
  # Add PureNYHAClass, i.e. just those with true class II and true class III, etc.
  pureNYHAClassVector <- vector(mode="character",nrow(FULL_DATA))
  pureFactorIndicies <- factorLevels2Index(c(FACTOR_LEVELS$PURE1,
                                              FACTOR_LEVELS$PURE2,
                                              FACTOR_LEVELS$PURE3,
                                              FACTOR_LEVELS$PURE4))  # get indices for NYHA Classes that are pure 1, 2, 3 or 4 only
  pureNYHAClassVector[pureFactorIndicies] <- as.character(FULL_DATA$NYHAClass[pureFactorIndicies])  # select/copy the 'cleaned up' factor names
  FULL_DATA$PureNYHAClass <- factor(pureNYHAClassVector, levels=levels(FULL_DATA$NYHAClass))  # convert to factor
  rm(pureFactorIndicies,pureNYHAClassVector)
  
  #### Add ExplicitNYHAClass ############################
  
  # Add ExplicitNYHAClass, i.e. those with class I, I/II, II, II/III, III, III/IV, IV
  explicitNYHAClassVector <- vector(mode="character",nrow(FULL_DATA))
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE1))] <- "I"  # populate pure I
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED12))] <- "I/II"  # populate mixed I/II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE2))] <- "II"  # populate pure II
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED23))] <- "II/III"  # populate mixed II/III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE3))] <- "III"  # populate pure III
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$MIXED34))] <- "III/IV"  # populate mixed III/IV
  explicitNYHAClassVector[factorLevels2Index(c(FACTOR_LEVELS$PURE4))] <- "IV"  # populate pure IV
  FULL_DATA$ExplicitNYHAClass <- factor(explicitNYHAClassVector,levels=c("I","I/II","II","II/III","III","III/IV","IV"))
  rm(explicitNYHAClassVector)

  # Remove participants where we have missing important (see step) data
  warning("Forcing HF018 to NA since HF018 has no step data - come up with a better way to do this if using different data set")
  FULL_DATA$PureNYHAClass[FULL_DATA$StudyIdentifier == "HF018"] <- NA # drop HF018 since they don't have step data
  FULL_DATA$ExplicitNYHAClass[FULL_DATA$StudyIdentifier == "HF018"] <- NA # as above

  
  # Create FULL_DATA.ForViewing for easier viewing of data using: View(FULL_DATA.ForViewing)
  FULL_DATA.ForViewing <- FULL_DATA; 
  FULL_DATA.ForViewing$StepData <- NULL  # drop StepData since this is what causes the problem
  
  # For debugging
  # FULL_DATA.NYHAOnly <- FULL_DATA[,c('StudyIdentifier','NYHAClassMixed','NYHAClass','PureNYHAClass','ExplicitNYHAClass')]
  
  # For distribution of participants by Sex in each class
  FULL_DATA.bySex <- FULL_DATA[,c('StudyIdentifier','Sex','ExplicitNYHAClass')]
  FULL_DATA.bySex <- FULL_DATA.bySex %>% arrange(ExplicitNYHAClass, Sex)
  
#### Analysis #########################################
  
  #melt so they're all together (also use ForViewing so that we don't melt the raw step data)
  melted <- reshape2::melt(FULL_DATA.ForViewing, id.vars=c("StudyIdentifier", 
                                                        "NYHAClass",
                                                        "PureNYHAClass",
                                                        "ExplicitNYHAClass"))
  
# Summary Statistics ==================================
  
  # for each grouping type now we want to calculate some summary statistics
  groupingType <- c("NYHAClass","PureNYHAClass","ExplicitNYHAClass")
  
  summarizedBy <- list()
  
#### Calculate ########################################
  
  cat('START Summary Statistics Calculation ==============================', sep="")
  
  for(group in groupingType)
  {
    # calculate summary statistics
    temp <- plyr::ddply(melted,
                        c(group, "variable"),
                        dplyr::summarise,
                        mean = mean(as.numeric(value),na.rm=TRUE), 
                        sd = sd(as.numeric(value),na.rm=TRUE),
                        sem = sd(as.numeric(value),na.rm=TRUE)/sqrt(sum(!(is.na(as.numeric(value))))),
                        n = sum(!(is.na(as.numeric(value)))))  # i.e. length without NAs
    # sort by variable name for easier visual comparison (if desired)
    temp <- temp %>% arrange(variable)
    
    # store
    summarizedBy[[group]] <- temp
  
    # N.B. this is sloppy and full of code smell
    # now extract them for convenience 
    # (it's easier to use the view function w/ named Data vs Values using RStudio interface)
    assign(paste("summarizedBy",group,sep=""),  # var name
           temp)  # var value
    assign(paste("summarizedBy",group,".noNAs", sep=""),  # var name
           temp %>% tidyr::drop_na_(group))  # var value
  }
  
  
# Kruskal-Wallis rank sum test ========================
# for each grouping type run a kruskal test on each variable
  
  cat('START Kruskal-Wallis rank sum tests ===============================', sep="")
  
  group <- group[1] # pick any one, it should be fine
  temp.noNAs <- summarizedBy[[group]] %>% tidyr::drop_na_(group)
  ktests.saved <- list()
  
  for(group in groupingType)
  {
    ktests.saved[[group]] <- list()
  }
  
  for(var in as.vector(t(unique(temp.noNAs['variable']))))
  {
      cat('\nVAR: ', var, '-------------------------------\n', sep="")
      
      for(group in groupingType)
      {
      
      # check that it's a good column
      colAsVec <- FULL_DATA[[var]]
      isNotAllNAs <- !all(is.na(colAsVec))
      isNotAllSameValue <- !length(unique(colAsVec[!is.na(colAsVec)])) == 1 # N.B. ignores na
      if(isNotAllNAs && 
         isNotAllSameValue)  # make sure it's not all NAs
      {
        # prep to run ktest for this variable
        formula <- as.formula(glue("{var} ~ {group}"))
      
        tryCatch({
          # run ktest
          ktest <- kruskal.test(formula, 
                                data=FULL_DATA)
          # add sig code to ktest output
          ktest$data.name <- glue('{getSigCode(ktest$p.value)} \\
                                  {ktest$data.name} \\
                                  {getSigCode(ktest$p.value)}')
          
          # save for later
          ktests.saved[[group]][[var]] <- ktest
          # print out for viewing!
          print(ktest)
  
        #}, warning = function (war) {
        }, error = function(err) {
          printableFormula <- capture.output(print(formula))
          print(glue("E:      Kruskal-Wallis rank sum test
  
                     data: while trying {printableFormula}
                     Caught Error: {err} "))
        }, finally = {
        }) # END tryCatch
      }
    }
  }
  
  
# Plotting All Results ================================
  
  if(GENERATE_GRAPHS)
  {
    for(group in groupingType)
    {
      temp.noNAs <- summarizedBy[[group]] %>% tidyr::drop_na_(group)
      for (var in as.vector(t(unique(temp.noNAs['variable']))))
      {
        
        cat('\nVAR: ', var, '-------------------', sep="")
        
        # check that it's a good column
        colAsVec <- FULL_DATA[[var]]
        isNotAllNAs <- !all(is.na(colAsVec))
        isNumeric <- is.numeric(colAsVec)
        if(isNotAllNAs &&
           isNumeric)  # make sure it's not all NAs
        {
          p.value <- ktests.saved[[group]][[var]]$p.value
          if(is.null(p.value))
          {
            p.value <- NaN
          }
          roundDigits <- 4
          ttl <- glue("{parseVarName(var)} per {group} [w/ Stat Summary] ",
                      "(p={round(p.value,digits=roundDigits)}",
                      "){getSigCode(p.value)}")
          cat('    PLOTTED', sep="")
          plot <- ggplot(data = FULL_DATA, 
                         aes_string(group,var,color=group)) +
            theme_tufte(base_family = "serif", 
                        ticks = FALSE) +
            stat_summary(fun.data=minMeanSEMMax, geom="boxplot") +
            geom_jitter() + 
            labs(title = ttl,
                 caption = "Simon Bromberg's Thesis Data") +
            xlab(group)
          print(plot)
          
          if(SAVE_GRAPHS)
          {
            # N.B. make sure savettl doesn't have invalid characters,
            # but if you keep getting 'unable to start png() device'
            # run dev.off() in Console to make sure you closed the last plot
            tryCatch({
              # create title
              savettl <- removeInvalidFileNameChars(
                glue("{parseVarName(var)} per {group} (w Stat Summary)"))
              saveName <- glue('{getwd()}/plots/SimonPaper/{savettl}.png')
              dev.copy(png, saveName, width = 600, height = 700)
              cat(' & SAVED', sep="")
            },error = function(err) {
              cat("\n")
              print(glue("Tried to print:
                         {saveName}"))
              stop(err)
            }, finally = {
              dev.off()
            })
            
          }
          
        }
        else
        {
          cat('NOT Plotted', sep="")
        }
        cat('\n', sep="")
      }
      }
    
  }

}

# END -------------------------------------------------------