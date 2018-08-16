### removeInvalidFileNameChars Function

################################################################################
require(glue)
require(stringr)

removeInvalidFileNameChars <- function(x, isWindowsOS=TRUE)
{
  mask <- "/"
  windowsMask <- "<>:\\\"|?*\\\\"
  if(isWindowsOS)
  {
    mask <- glue("{mask}{windowsMask}")
  }
  x <- str_replace_all(x, glue("[[{mask}]]"), " ")
  return(x)
}