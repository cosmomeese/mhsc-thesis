# To debug mismatched metaDataAndTokenLabels

mtoken <- metaDataAndTokens.list

##### Print out mappings ####
  ## fcn
printOutMappings <- function(mtoken)
{
  for(idx in 1:length(mtoken))
  {
    cat(names(mtoken)[[idx]], " -> ", as.character(mtoken[[idx]]$StudyIdentifier),"\n")
  }
}
  ## actually do it
printOutMappings(mtoken)

#### Replace ####
  ## fcn

removeAndTagDuplicatedNames <- function(mtoken)
{
  TOKEN_COL_NAME <- "Token.Wrapped"
  duplicates <- duplicated(names(mtoken))
  duplicate.NameKeys <- names(mtoken)[duplicates]
  # remove duplicates
  mtokenAttributes <- attributes(mtoken)
  
  # drop the names attribute
  namesAttributeName <- 'names'
  mtokenAttributes <- mtokenAttributes[!names(mtokenAttributes) %in% c(namesAttributeName)]
  mtoken <- mtoken[!duplicates] #N.B. which causes loss of attributes
  mtoken.names <- names(mtoken)
  mtokenAttributes[[namesAttributeName]] <- mtoken.names
  attributes(mtoken)<-mtokenAttributes
  
  # flag token for reset
  for(dup in duplicate.NameKeys)
  {
    mtoken[[dup]][[TOKEN_COL_NAME]] <- NA
  }
  
  return(mtoken)
}

# deprecated, this was to fix a previous error
removeResetTokenLabel <- function(mtoken)
for(idx in 1:length(metaDataAndTokens.list))
{
  if("RESET_TOKEN" %in% names(metaDataAndTokens.list[[idx]]))
  {
    metaDataAndTokens.list[[idx]]$RESET_TOKEN <- NULL
  }
  
}
rm(removeResetTokenLabel)

relabelMappings <- function(mtoken, asIDX_notStudyID=FALSE)
{
  for(idx in 1:length(mtoken))
  {
    if(asIDX_notStudyID)
    {
      newName <- idx
    }
    else
    {
      newName <- as.character(mtoken[[idx]]$StudyIdentifier)
    }
    names(mtoken)[[idx]] <- newName
  }
  mtoken <- removeAndTagDuplicatedNames(mtoken)
  return(mtoken)
}

  ## actually do it

# first pass should relabel all to a benign name (to prevent too much token loss)
mtoken <- relabelMappings(mtoken, asIDX_notStudyID=TRUE)
# manually check that mappings are clear
printOutMappings(mtoken)
# second pass should relabel (as possible) to study id
mtoken <- relabelMappings(mtoken, asIDX_notStudyID=FALSE)
# print out final mappings
printOutMappings(mtoken)

rm(printOutMappings,relabelMappings,removeAndTagDuplicatedNames)
cat('if it worked then: \n1) save mtoken back as metaDataAndTokens.list: metaDataAndTokens.list <- mtoken\n2) rm(mtoken), and \n3) save environment: ave.image("H:/raghads data/JFBDataRawPull.Rdata") (otherwise beware R will add an extra .Rdata to filename) ')