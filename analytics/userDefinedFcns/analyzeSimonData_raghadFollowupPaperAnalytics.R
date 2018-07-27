### Function to calculate Demographics + Statistics for Follow up to Raghad's Paper using Simon Bromberg's Thesis Data (Fitbit/CPS Data)

GENERATE_GRAPHS <- FALSE
SAVE_GRAPHS <- FALSE  # N.B. GENERATE_GRAPHS must also be true
SAVE_CSVS <- FALSE
SAVE_STAT_TEST <- FALSE
SAVE_JMIR_TABLE <- TRUE

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
  fullData$NYHAClass <- fullData$RoundDownNYHAClass
  fullData$RoundDownNYHAClass <- NULL
  
  fullData <- cleanHandednessColumnForAnalysis(fullData)
  
  fullData <- cleanWristbandColumnForAnalysis(fullData)
  
  fullData <- cleanSexColumnForAnalysis(fullData)
  
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
  # N.B. warning message: attributes are not identical across measure variables; they will be dropped 
  # ^ can be safely ignored (https://stackoverflow.com/questions/25688897/reshape2-melt-warning-message)
  # it just means that the 'value' column results have no factor levels (which is expected)
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
  
  #### generate kw results data frame  ####
  
  ktests.df <- data.frame()
  for(groupName in names(ktests.saved))
  {
    for(varName in names(ktests.saved[[groupName]]))
    {
      ktests.subdf <- data.frame(Group=groupName,
                                 variable=varName,
                                 kw.ChiSquared=ktests.saved[[groupName]][[varName]]$statistic,
                                 kw.DoF=ktests.saved[[groupName]][[varName]]$parameter,
                                 kw.p=ktests.saved[[groupName]][[varName]]$p.value)
      #ktests.df.list[[length(ktests.df.list)+1]] <- ktest.subdf
      
      ktests.df <- rbind(ktests.df,ktests.subdf)
    }
  }
  rownames(ktests.df) <- NULL
  rm(ktests.subdf)
  
  ### now one that's formated for reading/csv output
  
  mergeVarName <- "variable"
  ktests.csvOutput <- data.frame(character())
  colnames(ktests.csvOutput) <- mergeVarName
  for(groupName in names(ktests.saved))
  {
    ktst.df <- ktests.df[ktests.df$Group==groupName,]
    ktst.df$Group <- NULL # drop Group col
    colSubset <- !(colnames(ktst.df) %in% c(mergeVarName))
    colnames(ktst.df)[colSubset] <- paste0(groupName,".",colnames(ktst.df)[colSubset])
    ktests.csvOutput <- merge(x=ktests.csvOutput,
                              y=ktst.df,
                              by=mergeVarName,
                              all.y=TRUE,
                              sort=FALSE)
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
    write.csv(ktests.csvOutput,file="results/kruskal-wallis-tests.csv")
  }
  
  # Save Files as JMIR Table (for Paper) ===================================
  
  if(SAVE_JMIR_TABLE)
  {
    
    #### WARNING!!! HERE THERE BE DRAGONS ####
    # welcome to some of the worst code I have ever written
    
    pThreshold <- 0.05
    dirPath <- "../../Other/Follow-up on Raghad's Study"
    fileNamePrefix <- "analyzeSimonData_raghadFollowupPaperAnalytics_"
    tableRowDictFileName <- "TableRowDictionary.csv"
    acronymDictFileName <- "AcronymDictionary.csv"
    
    NYHAClassColNames <- c("II{M} (= I/II + II)",
                           "III{M} (= II/III + III)")
    
    SORTING_COLUMN <- "SortPriority"
    
    NEW_VARIABLE_COLUMNS <- c("Duration",
                              "Watts",
                              "Watts.Predicted.Percentage",
                              "SBP.Resting",
                              "DBP.Resting",
                              "HR.Resting",
                              "O2Sat.Resting",
                              "FEV.Resting",
                              "FEV.Resting.Percentage",
                              "FVC.Resting",
                              "FVC.Resting.Percentage",
                              "SBP","DBP",
                              "HR","HR.1min","HR.1minDrop",
                              "O2Sat",
                              "VO2.perBodyWeight",
                              "VO2.Predicted.Relative",
                              "VO2.Predicted.Relative.Percentage",
                              "VO2",
                              "VO2.Predicted.Absolute",
                              "VO2.Predicted.Absolute.Percentage",
                              "AT","VO2.Peak.Measured.Percentage",
                              "VO2.Peak.Predicted.Percentage",
                              "VE.Peak",
                              "VCO2.Peak",
                              "VEperVCO2.atAT",
                              "VEperVCO2.Peak",
                              "RER.Peak",
                              "StepData.HighestValuedStreak",
                              "StepData.LongestActiveStreak",
                              "StepData.OverallMean",
                              "StepData.OverallStdDev",
                              "StepData.OverallSkewness",
                              "StepData.OverallKurtosis",
                              "StepData.MaxActiveMinutes_Pure",
                              "StepData.MeanActiveMinutes_Pure",
                              "StepData.StdDevActiveMinutes_Pure",
                              "StepData.ModeActiveMinutes_Pure",
                              "StepData.TotalActiveMinutes_Pure",
                              "StepData.MaxMETClass.BelowMin",
                              "StepData.MeanMETClass.BelowMin",
                              "StepData.StdDevMETClass.BelowMin",
                              "StepData.ModeMETClass.BelowMin",
                              "StepData.TotalMETClass.BelowMin",
                              "StepData.MaxActiveMinutes_PosMET",
                              "StepData.MeanActiveMinutes_PosMET",
                              "StepData.StdDevActiveMinutes_PosMET",
                              "StepData.ModeActiveMinutes_PosMET",
                              "StepData.TotalActiveMinutes_PosMET",
                              "StepData.MaxMETClassIV",
                              "StepData.MeanMETClassIV",
                              "StepData.StdDevMETClassIV",
                              "StepData.ModeMETClassIV",
                              "StepData.TotalMETClassIV",
                              "StepData.MaxMETClassIII",
                              "StepData.MeanMETClassIII",
                              "StepData.StdDevMETClassIII",
                              "StepData.ModeMETClassIII",
                              "StepData.TotalMETClassIII",
                              "StepData.MaxMETClassII",
                              "StepData.MeanMETClassII",
                              "StepData.StdDevMETClassII",
                              "StepData.ModeMETClassII",
                              "StepData.TotalMETClassII",
                              "StepData.MaxMETClassI",
                              "StepData.MeanMETClassI",
                              "StepData.StdDevMETClassI",
                              "StepData.ModeMETClassI",
                              "StepData.TotalMETClassI",
                              "StepData.OverallMETClassIV.Percentage",
                              "StepData.OverallMETClassIII.Percentage",
                              "StepData.OverallMETClassII.Percentage",
                              "StepData.OverallMETClassI.Percentage",
                              "StepData.OverallMETClass.BelowMin.PercentageAll",
                              "StepData.OverallMETClassIV.PercentageAll",
                              "StepData.OverallMETClassIII.PercentageAll",
                              "StepData.OverallMETClassII.PercentageAll",
                              "StepData.OverallMETClassI.PercentageAll")
    
    UNDESIRED_COLUMNS <- c("NYHAClassMixed",
                           "HFDiagnosisYear",
                           "EjectionFraction",
                           "HFTreatmentsToDate",
                           "RegularPhysicalActivities",
                           "Exercises",
                           "DeviceID",
                           "ID",
                           "CPSDate",
                           "TestEnd.Reason",
                           "TestEnd.Symptom",
                           "RPEper20.Peak",
                           "PETCO2.Peak",
                           "OUES",
                           "TotalRiskScore",
                           NEW_VARIABLE_COLUMNS)
    
    #### Get Files
    
    require(readr)

    # combine with getwd() + evalute the ".."'s
    dirPath <- normalizePath(file.path(getwd(),dirPath), winslash="/")
    
    # assemble final full path
    tableDictFullPath <- file.path(dirPath,paste0(fileNamePrefix,
                                                  tableRowDictFileName))
    acronymDictFullPath <- file.path(dirPath,paste0(fileNamePrefix,
                                                    acronymDictFileName))
    
    tableDict <- read_csv(tableDictFullPath, 
                          col_types = cols(OutputBase = col_skip(), 
                                           OutputPrefix = col_skip()))
    
    acronymDF <- read_csv(acronymDictFullPath)
    acronymDict <- setNames(as.character(acronymDF$Description),
                            acronymDF$Acronym)
    rm(acronymDF)    
      
    #### Unmelt Results
    
    # change factor names to new column names
    # N.B. strip factor levels since otherwise it causes problems with rbind
    roundedClassSummary <-  summarizedByNYHAClass.noNAs %>%
      mutate(NYHAClass=as.character(factor(NYHAClass,
                                           labels=NYHAClassColNames)))
    roundedClassSummary$Group <- "Rounded"
    # explicit names don't need to be changed to anything fancier
    # N.B. strip factor levels since otherwise it causes problems with rbind
    explicitClassSummary <- summarizedByExplicitNYHAClass.noNAs %>%
      mutate(NYHAClass=as.character(ExplicitNYHAClass))
    explicitClassSummary$ExplicitNYHAClass <- NULL
    explicitClassSummary$Group <- "Explicit"
    
    combinedSummaryByClass <- rbind(roundedClassSummary,
                                    explicitClassSummary)
    rm(roundedClassSummary, explicitClassSummary)
    #make a factor again
    combinedSummaryByClass$NYHAClass <- as.factor(combinedSummaryByClass$NYHAClass)
    
    value.vars <- c("mean","sd","n")
    isFirstRun <- TRUE
    for(value.var in value.vars)
    {
      temp <- dcast(combinedSummaryByClass,
                    Group + NYHAClass ~ variable,
                    value.var=value.var)
      temp$Metric <- value.var
      if(isFirstRun)
      {
        unmeltSummary <- temp
        isFirstRun <- FALSE
      }
      else
      {
        unmeltSummary <- rbind(unmeltSummary, temp)
      }
    }
    # move last column (Metric) back to front (for easier reading)
    unmeltSummary <- unmeltSummary %>%
                      select(Metric, everything())
    
    # rename NYHAClass to what it will actually be: the header
    unmeltSummary$Header <- unmeltSummary$NYHAClass
    unmeltSummary$NYHAClass <- NULL
    
    fwdTranspose <- function(df,headervar='variable')
    {
      # transpose
      df <- as.data.frame(t(df))
      # variables are now rownames, but we want as column
      df[[headervar]] <- factor(row.names(df)) 
      # move last column (variable) back to front (for easier reading)
      df <- df %>%
        select(!!!headervar, everything())
      # reset row numbers (again for easier reading)
      rownames(df) <- seq(length=nrow(df))
      return(df)
    }
    
    bwdTranspose <- function(df,headervar='variable',dropFirstCol=FALSE)
    {
      originalVarNames <- df[[headervar]]
      dfToTrans <- df
      if(dropFirstCol)
      {
        dfToTrans <- df[,-1]
      }
      df.t <- as.data.frame(t(dfToTrans))
      colnames(df.t) <- originalVarNames
      return(df.t)
    }
    
    #combined summarized overall with ktests
    summarizedOverallktests <- merge(summarizedOverall,
                                     ktests.csvOutput,
                                     all=TRUE,
                                     by='variable',
                                     sort=FALSE)
    
    unmeltSummary.T <- fwdTranspose(unmeltSummary)
    summarizedOverall.bT <- bwdTranspose(summarizedOverallktests)
    
    # transpose summarizedOverall data frame to make it easier to add group, metric & header
    
    
    determineGroup <- function(df)
    {
      vec <- rownames(df)
      var.values <- setNames(c('Rounded','Pure','Explicit'),
                             c('NYHAClass','PureNYHAClass','ExplicitNYHAClass'))
      for(var.value in names(var.values))
      {
        # get columns with var.values as prefix follows by a .
        regex <- paste0('\\b',var.value,'.')
        varCols <- grep(regex,
                        vec)
        vec[varCols] <- var.values[var.value]
      }
      return(vec)
    }
    
    determineMetric <- function(df)
    {
      vec <- rownames(df)
      var.values <- c('r','r2','p','kw.p','kw.ChiSquared','kw.DoF')
      for(var.value in var.values)
      {
        # get columns with var.values as suffix:
        # beginning of string followed by "a word" (any number of 
        # alphanumerics and _ followed by at most {1} period character
        # then the var.value and end of string)
        regex <- paste0('^\\w+(\\.){1}',var.value,'\\b')
        varCols <- grep(regex,
                        vec)
        vec[varCols] <- var.value
      }
      return(vec)
    }
    
    determineHeader <- function(groupVec,metricVec)
    {
      vec <- metricVec
      groupHeaderPart <- setNames(c('','(II vs. III)','(all)'),
                                  c('Rounded','Pure','Explicit'))
      metricHeaderPart <- setNames(c('R','R^2 ','P-value (Pearson)', "P-value", "Chi{^2}","KW Degrees of Freedom"),
                                   c('r','r2','p','kw.p','kw.ChiSquared','kw.DoF'))
      for(eleIdx in 1:length(metricVec))
      {
        ele <- NA
        metricEle <- metricHeaderPart[metricVec[[eleIdx]]]
        groupEle <- groupHeaderPart[groupVec[[eleIdx]]]
        
        if(!is.na(metricEle))
        {
          ele <- metricEle
          if(!is.na(groupEle) && (groupEle != ''))
          {
            ele <- paste0(ele, " ", groupEle)
          }
        }
        vec[[eleIdx]] <- ele
      }
      return(vec)
    }
    
    # add group, metric + header
    summarizedOverall.bT$Group <- determineGroup(summarizedOverall.bT)
    summarizedOverall.bT$Metric <- determineMetric(summarizedOverall.bT)
    summarizedOverall.bT$Header <- determineHeader(groupVec=summarizedOverall.bT$Group,
                                                  metricVec=summarizedOverall.bT$Metric)
    
    # transpose back
    summarizedOverall.fbT <- fwdTranspose(summarizedOverall.bT)
    rm(summarizedOverall.bT)
    
    # merge
    mergeByColName <- 'variable'
    unmeltSummary.T <- merge(x=unmeltSummary.T,
                           y=summarizedOverall.fbT,
                           by=mergeByColName,
                           sort=FALSE, # don't reorder rows post join (but it does put it at the end..)
                           all.x = TRUE # left join
    )
    unmeltSummary <- bwdTranspose(unmeltSummary.T,
                                  dropFirstCol=TRUE)
    unmeltSummary <- unmeltSummary %>%
                      select(Header, everything())
    
    dropNonUsefulVariablesObservations <- function(df,undesiredColumns=UNDESIRED_COLUMNS)
    {
      df <- df[ , !names(df) %in% undesiredColumns]
      return(df)
    }
    
    unmeltSummaryTrimmed <- dropNonUsefulVariablesObservations(unmeltSummary)
    
    # Identify significant vs. non-significant values
    roundedTName <- "Rounded"
    nonRoundedTName <- "Pure+All"
    
    classTables <- list()

    # Split the combined table into our two tables
    classTables[[roundedTName]] <- unmeltSummaryTrimmed %>% 
                              subset(.$Group %in% c("Rounded"))
    classTables[[nonRoundedTName]] <- unmeltSummaryTrimmed %>% 
                                 subset(.$Group %in% c("Pure","Explicit"))
    classTables[[roundedTName]] <- fwdTranspose(classTables[[roundedTName]])
    classTables[[nonRoundedTName]] <- fwdTranspose(classTables[[nonRoundedTName]])
    
    subsetSignificantVariables <- function(df,
                                           levelOfSignificance=pThreshold,
                                           getNonSignificant=FALSE,
                                           significanceVarCode="kw.p")
    {
      require(glue)
      
      tableHeaderVars <- c("Header","Metric","Group")
      
      # get columns with r as suffix
      # get columns with var.values as suffix:
      # beginning of string followed by "a word" (any number of 
      # alphanumerics and _ followed by at most {1} period character
      # then the significance var code and end of string)
      regex <- paste0('^\\w+(\\.){1}',significanceVarCode,'\\b')
      varCols <- colnames(df)[grep(regex,colnames(df))]

      dfname <- "dfValuesOnly"
      # if value@varCols < level of Signifiance
      # and the varCol is not na then it's significant
      subsetCond <- glue("(!is.na({dfname}${varCols}) & ", 
                         "({dfname}${varCols} < {levelOfSignificance}))")
      #subsetCond <- glue("({varCols} < {levelOfSignificance})")
      
      
      # for any of the columns in the list (so ||)
      subsetCond <- glue::collapse(subsetCond," | ")
      if(getNonSignificant)
      {
        subsetCond <- glue("!({subsetCond})")
      }
      
      #make values numeric
      dfHeadersOnly <- df %>% subset(variable %in% tableHeaderVars)
      dfValuesOnly <- df %>% subset(!(variable %in% tableHeaderVars))
      
      variablesCol.bac <- dfValuesOnly$variable
      indx <- sapply(dfValuesOnly, is.factor)
      dfValuesOnly[indx] <- lapply(dfValuesOnly[indx], function(x) as.numeric(as.character(x)))
      dfValuesOnly$variable <- variablesCol.bac
      rm(variablesCol.bac)
      
    
      dfValuesSubset <- dfValuesOnly[eval(parse(text=subsetCond)),]
      dfValuesSubset[] <- lapply(dfValuesSubset[], function(x) as.character(x))
      df <- rbind(dfHeadersOnly,dfValuesSubset,
                  stringsAsFactors=FALSE)
      return(df)
    }
    
    sigTName <- "Significant"
    notSigTName <- "NonSignificant"
    for(tableName in names(classTables))
    {
      tableVal <- classTables[[tableName]]
      if(is.data.frame(tableVal))
      {
        classTables[[tableName]] <- list()
      }
      classTables[[tableName]][[sigTName]] <- subsetSignificantVariables(tableVal)
      classTables[[tableName]][[notSigTName]] <- subsetSignificantVariables(tableVal,
                                                                          getNonSignificant=TRUE)
      
    }

    #### Generate Acronmyms for Tables
    
    # also adds the units
    generateFinalRowNameAndFootnotes <- function(outputTable,
                                                 acronymDict,
                                                 tableDict,
                                                 outputTableMergeName="variable",
                                                 tableDictMergeName="VarName",
                                                 sortingColumn=SORTING_COLUMN)
    {
      
      require(stringr)
      #### A helper function (thank you flodel)
      # https://stackoverflow.com/a/25881167
      extend <- function(alphabet) function(i) {
        base10toA <- function(n, A) {
          stopifnot(n >= 0L)
          N <- length(A)
          j <- n %/% N 
          if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
        }   
        vapply(i-1L, base10toA, character(1L), alphabet)
      }
      moreletters <- extend(letters)
      
      footnoteLetterDict <- c()
      tableFootNotes <- c()
      
      # first merge
    
      updatedTable <- merge(x=outputTable,
                            y=tableDict,
                            all.x=TRUE,
                            by.x=outputTableMergeName,
                            by.y=tableDictMergeName,
                            sort=FALSE)
      # order final table by sort priority     
      
      updatedTable <- updatedTable[order(updatedTable[[sortingColumn]]),]
      
      
      # extract possible acronymns, sort decreasing order
      # to ensure that the longer acronyms are always 
      # tested (& replaced) first.
      # replace every acronym makes this problem too
      # complicated to solve for what we're trying to
      # accomplish
      possibleAcronyms <- sort(names(acronymDict))
      
      nextLetterIdx <- 1
      
      for(rowIdx in 1:nrow(updatedTable))
      {
        rowContent <- updatedTable[rowIdx,]
        
        fullOutputName <- rowContent$FullOutputName
        
        if(!is.na(fullOutputName))
        {
          # find best matching acronym
          # N.B. we only replace 1 because it gets too
          # complicated to do multiple ones since we then
          # have to seperate, e.g. 'SBP', vs 'SBP, Resting'
          # which is not presently worth the effort to fix
          matchedAcronym <- ""
          for(possibleAcronym in possibleAcronyms)
          {
            isAcronymInOutputName <- grepl(possibleAcronym,
                                           fullOutputName,
                                           fixed=TRUE)
            # we do assess 'better match' by length for now
            # if it's longer it matches more of string
            # therefore better match
            #browser()
            isNewAcronymBetterMatch <- str_length(possibleAcronym) > str_length(matchedAcronym)
            if(isAcronymInOutputName && isNewAcronymBetterMatch)
            {
              matchedAcronym <- possibleAcronym
            }
          }
          
          # now add to footnotes & modify output name
          
          if(str_length(matchedAcronym) > 0)
          {
            isAcronymAlreadyAssignedLetter <- !(is.null(footnoteLetterDict) ||
                                                  is.na(footnoteLetterDict[matchedAcronym]))
            if(isAcronymAlreadyAssignedLetter)
            {
              nextCaretLetter <- footnoteLetterDict[[matchedAcronym]]
            }
            else
            {
              nextCaretLetter <- paste0('{',
                                        moreletters(nextLetterIdx),
                                        '}')
              footnoteLetterDict[matchedAcronym] <- nextCaretLetter
              # create foodnote
              footnote <- paste0(nextCaretLetter,
                                 matchedAcronym,
                                 ": ",
                                 acronymDict[[matchedAcronym]],
                                 "\n")
              # add to footnote table
              tableFootNotes <- rbind(tableFootNotes,
                                      footnote)
              nextLetterIdx <- nextLetterIdx + 1
            }
            
            # add caret letter immediately after acronmym
            fullOutputName <- gsub(pattern=matchedAcronym,
                                   replacement=paste0(matchedAcronym,
                                                      nextCaretLetter),
                                   x=fullOutputName,
                                   fixed=TRUE)
          }
        }
        
        
        
        # add units
        rowLabelWithUnits <- fullOutputName
        hasUnits <- is.character(rowContent$Units) && 
          !is.na(rowContent$Units)
        if(hasUnits)
        { # actually add units
          rowLabelWithUnits <- paste0(rowLabelWithUnits,
                                      " [",rowContent$Units,"]")
        }
        updatedTable[rowIdx,]$FullOutputName <- rowLabelWithUnits
      }
      
      updatedTable <- updatedTable %>%
                        select(FullOutputName, everything())

      # reorder headers
      updatedTable <- updatedTable[order(updatedTable[[sortingColumn]]),]
      #tableHeaderVars <- c("Header","Metric","Group")
      #tableHeadersOnly <- updatedTable %>% subset(variable %in% tableHeaderVars)
      #tableValuesOnly <- updatedTable %>% subset(!(variable %in% tableHeaderVars))
      #updatedTable <- rbind(tableHeadersOnly,tableValuesOnly,
      #                      stringsAsFactors=FALSE)
      
      # drop undesired columns
      updatedTable[[sortingColumn]] <- NULL
      updatedTable$variable <- NULL
      updatedTable$Units <- NULL
      
      result <- list()
      result$Footnotes <- tableFootNotes
      result$UpdatedTable <- updatedTable
      return(result)
    }
    
    footNoteTxtFileName <- "footnotes.txt"
    dividerPipeCharSet <- "=============="
    
    # clear previous file
    sink(file.path(dirPath,
                   footNoteTxtFileName))
    sink()
    
    for(tableName in names(classTables))
    {
      tableVal <- classTables[[tableName]]
      for(subTableName in names(tableVal))
      {
        subTable <- tableVal[[subTableName]]
        subTable <- generateFinalRowNameAndFootnotes(outputTable=subTable,
                                                     acronymDict=acronymDict,
                                                     tableDict=tableDict)
        # save for debugging of course
        classTables[[tableName]][[subTableName]] <- subTable
        
        tableRootName <- glue("{tableName}-{subTableName}")
        csvFileSaveName <- glue("{tableRootName}.csv")
        write.csv(subTable$UpdatedTable, 
                  file = file.path(dirPath,
                                   csvFileSaveName))
        
        sink(file.path(dirPath,
                       footNoteTxtFileName),
             append=TRUE)
        cat("\n",dividerPipeCharSet,tableRootName,dividerPipeCharSet,"\n\n")
        print(collapse(subTable$Footnotes))
        sink()
      }
    }
  }
  

}

# END ------------------------------------------------------- 