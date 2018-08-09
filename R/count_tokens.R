#' counts the frequency of unique tokens
#'
#' @description Call word tokenizer, then count the frequency of each unique word.
#' @usage count_tokens(x)
#' @param x length 1 character vector
#' @return A table of word frequencies
#' @seealso \code{\link{str_split}} \code{\link{boundary}} \code{\link{tokenize}}
#' @examples
#' count_tokens("If this function is working right, it is going to return a count of three for the word, 'is.'")
#' @export count_tokens

count_tokens<-function(x){
  if (length(x)>1){
    stop("count_tokens only accepts length 1 character vectors")
  }
  if (!is.character(x)){
    stop("count_tokens only accepts character vectors")
  }
  tks <- tokenize(x)
  return(table(tks))
}
