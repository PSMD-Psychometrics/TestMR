#' Takes a substring of a character string counting from the RHS
#' 
#' @description This function allows the user to extract a substring of a character string whilst containing the start
#' and stop values from the RHS of the string. The user is also given the option of reversing the extracted string before
#' returning the extracted value.
#' 
#' @usage substrRight(x, start, stop, reverse)
#' 
#' @param x a character vector
#' @param start integer. The first element to be replaced (counting from the RHS).
#' @param stop integer. The last element to be replaced (counting from the RHS).
#' @param reverse logical. Reverse the ordering of characters. Defaults to FALSE.
#' 
#' @examples 
#' x <- '91726352EXTRACT ME2345'
#' substrRight(x = x, start = 5, stop = 14)
#' 
#' x <- '15679NODNOL'
#' substrRight(x = x, start = 1, stop = 6, reverse = TRUE)
#' 
#' @export
substrRight <- function(x = NA_character_,
                        start = NA_integer_,
                        stop = NA_integer_,
                        reverse = FALSE){
  
  # Error input capture procedures.
  if(is.na(x)) stop('Please provide a character string!')
  if(is.na(start)) stop('Please provide a start value!')
  if(is.na(stop)) stop('Please provide a finish value!')
  if(!is.character(x)) stop('Please enter a character string!')
  if(nchar(x) < 1) stop('Please enter a non-empty character string!')
  if(start > nchar(x)) stop('Starting value is outside of the character count range!')
  if(start < 1) stop('Starting value is outside of the character count range!')
  if(stop > nchar(x)) stop('Finishing value is outside of the character count range!')
  if(stop < start) stop('Starting value is greater than finishing value!')
  
  # The new substring procedure.
  x <- substr(x,
              start = (nchar(x) - (stop - 1)),
              stop = (nchar(x) - (start - 1)))
  
  # If requested reverse the ordering of characters
  x <- ifelse(reverse, 
              sapply(lapply(strsplit(x, NULL), rev), paste, collapse = ""),
              x)
  
  # Return character string
  return(x)
  
}