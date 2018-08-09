#' Sentiment analysis with included 3-dimensional affect space consiting of rationality, social impact, and valence.
#'
#' @description Calls the sentiment function using the built-in dictionary. Note that this function will be slow on its first call, because the dictionary is only loaded if needed. This dictionary covers approximately 2 million words from the fastText common crawl pre-trained word vectors. The dimensions distinguish rational from emotional states, intense social states from others, and positive from negative states, respectively. See here for validation details: https://github.com/markallenthornton/3daffect
#' @usage affect3d(x)
#' @param x length n character vector. Inputs do not need to be shifted to lower case due to wide coverage of dictionary.
#' @return n x 3 numerical matrix with scores on x for the dimensions of rationality, social impact, and valence. Scores are relative to 166 mental state words, and should not be interpreted in absolute terms with respect to 0.
#' @seealso \code{\link{sentiment}}
#' @examples
#' # High rationality (cognitive states)
#' affect3d("They play chess, computer games, and critical thinking puzzles.")
#' @export affect3d


affect3d <- function(x){
  return(sentiment(x,rownames(dict),dict))
}
