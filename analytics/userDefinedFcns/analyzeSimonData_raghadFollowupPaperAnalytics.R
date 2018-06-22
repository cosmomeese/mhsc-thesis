### Function to calculate Demographics + Statistics for Follow up to Raghad's Paper using Simon Bromberg's Thesis Data (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true
SAVE_CSVS <- TRUE
SAVE_STAT_TEST <- TRUE

## Install & Load Required Library Packages
# Install if it's not installed on this computer
pkg <- c("plyr","tidyverse","reshape2","glue", "ggthemes","hmisc")
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
library(Hmisc)

## Import Required Functions
sourceDir <- "userDefinedFcns"
fcns <- list("analyzeData_common",
             "analyzeSimonData_common")

# Perform actual import
srcCreateFcn <- function(sfcn,sourceDir) #helper function to import
{
  source(paste(sourceDir,"/",sfcn,".R",
               sep=""))
}
invisible(lapply(fcns,srcCreateFcn,
                 sourceDir=sourceDir)) #use function
rm(srcCreateFcn) #remove the extra unneeded variables

# START -------------------------------------------------------

# Prep ================================================

#### Add Pure NYHA Class ##############################

if(!exists("m_cData"))
{
  stop("Missing m_cData. Couldn't execute. Load .Rdata file w/ m_cData and retry. \nData File 'ConvertedRawDataForHMM-v0.3-fromSource-v1.2.RData' will do.")
  
} else {
  
  fullData <- m_cData
  
  # Prep ==================================
  
  #### Add BMI ##########################################
  # BMI = weight[kg] / (height[m]^2)
  fullData <- addBMIColumn(fullData)
  
  #### Clean up factor levels ###########################
  
  fullData <- addMissingClassGroupings(fullData)
  
  #### Tag fullData as immutable
  FULL_DATA <- fullData
  rm(fullData)
  
  # Create FULL_DATA.ForViewing for easier viewing of data using: View(FULL_DATA.ForViewing)
  FULL_DATA.ForViewing <- createViewableFullData(FULL_DATA);
  
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
  
  cat('\nSTART Summary Statistics Calculation ==============================', sep="")
  
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
  
  cat('\nSTART Summary Statistics Calculation (Overall + rcorr) ====================', sep="")
  
  ### rcorr ####
  #wrapper for rcorr for dplyr function
  # x = x for rcorr
  # y = y for rcorr
  # out_name : r, P or n depending on desired variable from rcorr output matrix (for intersection element of x & y)
  # N.B. if number of (!NA) observations < 4 then will return NA instead of rcorr (since rcorr doesn't support < 4 obs.)
  rcorrForDplyr <- function(x,y,out_name)
  {
    n.x <- sum(!(is.na(as.numeric(x))))
    n.y <- sum(!(is.na(as.numeric(y))))
    isR2 <- 'r2'==out_name
    
    if(out_name %in% c('r','P','n','r2'))
    {
      out_val <- out_name
      if(isR2)
      {
        out_val <- 'r'
      }
      
      if(n.x > 4 && n.y > 4)
      {
        #browser()
        result <- rcorr(as.numeric(x),as.numeric(y))[[out_val]][2]
        if(isR2)
        {
          result <- result^2
        }
      }
      else
      {
        result <- NA
      }
      
    }
    else
    {
      stop("out_name must be either r, r2, P, or n")
    }
    return(result)
  }
  
  
  # calculate summary statistics
  summarizedOverall <- plyr::ddply(melted %>% tidyr::drop_na_("NYHAClass"),
                                   c("variable"),
                                   dplyr::summarise,
                                   mean = mean(as.numeric(value),na.rm=TRUE), 
                                   sd = sd(as.numeric(value),na.rm=TRUE),
                                   sem = sd(as.numeric(value),na.rm=TRUE)/sqrt(sum(!(is.na(as.numeric(value))))),
                                   n = sum(!(is.na(as.numeric(value)))))  # i.e. length without NAs
  
  vars <- c("r","r2","P")
  for(var in vars)
  {
    for(group in groupingType)
    {
      vName <- paste(group,tolower(var),sep=".")
      
      overallTemp <- plyr::ddply(melted %>% tidyr::drop_na_("NYHAClass"),
                                 c("variable"),
                                 dplyr::summarise,
                                 !!vName := rcorrForDplyr(value, (!!as.name(group)), var))
      
      summarizedOverall <- merge.data.frame(x=summarizedOverall,
                                            y=overallTemp,
                                            by='variable',
                                            all=TRUE)
    }
  }
  
  # Kruskal-Wallis rank sum test ========================
  # for each grouping type run a kruskal test on each variable
  
  cat('\nSTART Kruskal-Wallis rank sum tests ===============================', sep="")
  
  kwFile <- paste(getwd(),"/results/summaryOfSimonDataPaperAnalyticsKW.txt",sep="")
  group <- group[1] # pick any one, it should be fine
  temp.noNAs <- summarizedBy[[group]] %>% tidyr::drop_na_(group)
  ktests.saved <- list()
  
  for(group in groupingType)
  {
    ktests.saved[[group]] <- list()
  }
  
  # print out for viewing!
  if(SAVE_STAT_TEST)
  {
    #redirect to output file
    sink(kwFile)
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
  if(SAVE_STAT_TEST)
  {
    #redirect back to console
    sink(NULL)
    cat("\ndone!")
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
          ttl <- glue("{parseVarName(var,TRUE)} per {parseVarName(group,TRUE)} [w/ Stat Summary] ",
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
            ylab(parseVarName(var,TRUE)) +
            xlab(parseVarName(group,TRUE))
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

  # Save Files as CSV ===================================
  
  if(SAVE_CSVS)
  {
    write.csv(summarizedByExplicitNYHAClass.noNAs, file = "results/summarizedByExplicitNYHAClass.csv")
    write.csv(summarizedByNYHAClass.noNAs, file = "results/summarizedByNYHAClass.csv")
    write.csv(summarizedByPureNYHAClass.noNAs, file = "results/summarizedByPureNYHAClass.csv")
    write.csv(summarizedOverall, file = "results/summarizedOverall.csv")
    
  }
}

# END ------------------------------------------------------- 