# important saved model

### WARN: NOT CLEAN!!

mtd <- 'rf'
PLOT_TYPE <- 'VarBar'
totalVarImp <- varImportance

graphTitlePrefix <- fullGraphTitlePrefix
modelNamePlaceholderString <- MODEL_NAME_PLACEHOLDER_CHAR
tableRowDictionary <- tableRowDict
saveGraphs <- TRUE

completeGraphTitlePrefix <- gsub(modelNamePlaceholderString,mtd,graphTitlePrefix,
                                 fixed=TRUE)

# put variables as row names
featureColName <- "Feature" #N.B. not dynamically changed in ggplot fcn
rownames(totalVarImp[[mtd]]) <- totalVarImp[[mtd]]$Variable
# drop the seperate column for variable
totalVarImp[[mtd]]$Variable <- NULL
featureColNum <- which(names(totalVarImp[[mtd]]) == featureColName)
totalVarImp[[mtd]][,-featureColNum] <- apply(totalVarImp[[mtd]][,-featureColNum],
                                             MARGIN = 2, #use columns
                                             FUN = function(X) (X - min(X, na.rm=NA_REMOVE_FLAG))/diff(range(X,na.rm=NA_REMOVE_FLAG)))

# get only the VarName and FullOutputName cols (the first two columns)
tableDictFeatureColName <- 'VarName' #N.B. not dynamically changed in ggplot fcn
colsToKeep <- c(tableDictFeatureColName,'FullOutputName')
tableNamesDictOnly <- tableRowDictionary[,colnames(tableRowDictionary) %in% colsToKeep]

# melt the data frame from wide to long
plotDF <- reshape2::melt(totalVarImp[[mtd]], 
                         id.var=featureColName,
                         variable.name="Fold",
                         value.name="Importance")

plotDFWithOutputNames <- merge(plotDF,
                               tableNamesDictOnly,
                               by.x=featureColName,
                               by.y=tableDictFeatureColName,
                               all.x=TRUE)

# get title quickly before plotting
ttl <- gsub(modelNamePlaceholderString,mtd,graphTitlePrefix,
            fixed=TRUE)
# order so they'll be nicely in order of importance
trimmedPlotDF <- plotDFWithOutputNames %>% 
                  group_by(Feature, FullOutputName) %>%
                  summarize(Importance = sum(Importance)) %>%
                  arrange(desc(Importance))

top10Features <- trimmedPlotDF$Feature[1:10]


p <- plotDFWithOutputNames %>% filter(Feature %in% top10Features) %>%
                                      ggplot(aes(x = reorder(.$FullOutputName, .$Importance, 
                                                      function(x) {sum(x,na.rm=TRUE)}), # to order in descending order
                                          y = Importance,
                                          fill = Fold)) +
                                      geom_bar(stat = "identity") +
                                      labs(title = ttl,
                                           x = "Feature")  +
                                      coord_flip() +
                                      theme(legend.position = "none")

plot(p)

if(saveGraphs)
{
  source("userDefinedFcns/savePlot.R")
  fileName <- glue("{PLOT_TYPE}_{completeGraphTitlePrefix}.png")
  filePrefix <- ""
  savePlot(fileName,
           filePrefix=filePrefix,
           rc[[mtd]],
           isGGPlot=FALSE)
}
