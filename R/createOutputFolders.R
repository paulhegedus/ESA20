#' @title Creates results folders for each field.
#'
#' @description Makes a 'results' folder if not present, and makes a folder
#' within for each field.
#'
#' @param fields Character vector with field names.
#' @return Folders in a 'results' folder for each field.
createOutputsFolder <- function(fields){
  ## create new folder in 'results' folder
  cwd <- paste0("results") # outputs working directory
  if (!file.exists(cwd)) { 
    dir.create(cwd)
  } else {
    for (i in 1:length(fields)) {
      nwd <- paste0("results/", fields[i])
      if (!file.exists(nwd)) {
        dir.create(nwd)
      }
    }
  }
  return(invisible())
}