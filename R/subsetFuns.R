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