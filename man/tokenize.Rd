% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{tokenize}
\alias{tokenize}
\title{break character strings into words}
\usage{
tokenize(x)
}
\arguments{
\item{x}{length 1 character vector}
}
\value{
words character vector of tokenized words
}
\description{
Separates a character string into individual words. Wraps stringr package str_split.
}
\examples{
tokenize("This is a wonderful R package, isn't it?")
}
\seealso{
\code{\link{str_split}} \code{\link{boundary}}
}
