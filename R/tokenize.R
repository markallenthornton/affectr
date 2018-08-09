#' break character strings into words
#'
#' @description Separates a character string into individual words. Wraps stringr package str_split.
#' @usage tokenize(x)
#' @param x length 1 character vector
#' @return words character vector of tokenized words
#' @seealso \code{\link{str_split}} \code{\link{boundary}}
#' @examples
#' tokenize("This is a wonderful R package, isn't it?")
#' @export tokenize
#' @import stringr

tokenize <- function(x){
  if (length(x)>1){
    stop("tokenize only accepts length 1 character vectors")
  }
  if (!is.character(x)){
    stop("tokenize only accepts character vectors")
  }
  words <- str_split(x,boundary("word"))[[1]]
  return(words)
}
