library(data.table)

#' @title Subsetting vector by pattern matching
#'
#' @description Subset a character vector by matching a character pattern. 
#'
#' @param pattern Character pattern to search. 
#' @param vec Character vector to search and subset.
#' @return Subset of character vector containing pattern.
subsetCharVector <- function(pattern, vec){
  stopifnot(
    length(pattern) == 1,
    !is.null(pattern),
    !is.null(vec),
    is.character(pattern),
    is.character(vec)
  )
  out <- vec[grep(pattern, vec)]
  return(out)
}

#' @title Wrapper for subsetCharVector()
#'
#' @description Subsets 'recent' and 'previous' years from a vector of file
#' names and returns a list with 'ry' and 'py' elements with file names for
#' 'recent' and 'previous' years, respectively.
#'
#' @param field_vec Vector of file names from one field and multiple years.
#' @param ry Character with years from the most recent year crop harvested 
#' from the field.
#' @param py Character with years from the  previous year crop harvested 
#' from the field.
#' @return List with filenames separated into recent and previous years.
subsetYears <- function(field_vec, ry, py) {
  stopifnot(
    is.character(field_vec),
    is.character(ry),
    is.character(py)
  )
  year_list <- list(ry = ry,
                    py = py)
  year_list <- lapply(year_list, subsetCharVector, field_vec)
  return(year_list)
}
