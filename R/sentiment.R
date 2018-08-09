#' Conduct dictionary-based sentiment analysis.
#'
#' @description Tokenizes character vectors, counts words, and then takes weighted average to estimate sentiment.
#' @usage sentiment(x,tokens,scores)
#' @param x length n character vector. If you're input dictionary is lower case only, then be sure to preprocess inputs to x using tolower.
#' @param tokens length m character vector containing dictionary words
#' @param scores m x d numerical matrix containing scores for each dictionary word on d-dimensions. Values can be weighted continuous scores or binary.
#' @return n x d numerical matrix with scores on x for dimensions d
#' @seealso \code{\link{count_tokens}} \code{\link{tokenize}}
#' @examples
#' dictionary <- c("great","good","bad","awful")
#' scores <- matrix(c(1,.5,-.5,-1),4,1)
#' sentiment("This package is great.",dictionary,scores)
#' sentiment("This package is good.",dictionary,scores)
#' sentiment("This package is bad.",dictionary,scores)
#' sentiment("This package is awful.",dictionary,scores)
#' @export sentiment


sentiment <- function(x,tokens,scores){
  if (!is.character(tokens)){
    stop("tokens must be a character vector")
  }
  if (!is.character(x)){
    stop("x must be a character vector")
  }
  if (is.vector(scores)){
    scores <- matrix(scores,length(scores),1)
  }
  ndim <- dim(scores)[2]
  if (length(x)>1){
    xt <- paste(x,collapse = " ")
  } else {
    xt <- x
  }
  ut <- unique(tokenize(xt))
  sel <- tokens %in% ut
  tokens <- tokens[sel]
  scores <- scores[sel,]
  if (sum(sel)==1){
    scores <- matrix(scores,ndim,1)
  }
  vals <- matrix(NA,length(x),ndim)
  colnames(vals)<-colnames(scores)
  for (i in 1:length(x)){
    tk.count <- count_tokens(x[i])
    tk.count <- tk.count[names(tk.count) %in% tokens]
    curtks <- names(tk.count)
    sel <- tokens %in% curtks
    curtokens <- tokens[sel]
    curscores <- scores[sel,]
    if(sum(sel)==1){
      curscores <- matrix(curscores,1,ndim)
    }
    if (sum(sel) != 0){
      for (j in 1:ndim){
        vals[i,j] <- mean(unlist(lapply(curtks, function(w) rep(curscores[curtokens==w,j],tk.count[w]))))
      }
    }
  }
  return(vals)
}
