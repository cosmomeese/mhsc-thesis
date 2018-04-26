### Data Analysis Common Definitions & Functions

# SOME BASIC DEFINITIONS

## DEBUG LEVELS

# compress & save important variables in list of constants used

################################################################################
# Common Helper Functions


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

# thank you Ryan Witt for inspiration https://gist.github.com/ryanwitt/2911560
confusion.glm <- function(data, model) {
  prediction <- data$predictions
  #prediction <- ifelse(predict(model, data, type='response') > 0.5, 
  #                     TRUE, FALSE)
  
  #model.predictVars <- attr(attr(models[[1]]$model,"terms"),"term.labels")
  model.predictVars <- model$coefnames
  #model.allVars <- names(attr(attr(models[[1]]$model,"terms"),"dataClasses"))
  model.allVars <- names(attr(model$terms,"dataClasses"))
  model.responseVar <- setdiff(model.allVars,model.predictVars)
  
  response <- data[[model.responseVar]]
  #if(length(levels(response)) > 2)
  #{
  #  stop(glue("Too many factor levels ({length(levels(response))}) : {paste(levels(response), collapse= ', ')}"))
  #}
  #responseAsBool <- as.logical(as.numeric(response) - 1)
  #names(responseAsBool) <- rownames(data)
  confusion <- confusionMatrix(data=prediction,
                               reference=response,
                               dnn= c("AI","Physician"))
  confusion
}
