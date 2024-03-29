% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{precision_check}
\alias{precision_check}
\title{Manually check positive matches}
\format{
A data.frame
}
\usage{
data(precision_check)
}
\description{
100 event - article pairs that are considered as matches in the analysis presented in the paper.
That is, using the preprocessing and document similarity calculation settings used in this
online appendix, and using the similarity threshold of 6.54 as obtained by the threshold estimation procedure.
The purpose of this data is to provide additional validation.
}
\details{
The other gold standard is a completely random sample of articles for which all possible matches were coded,
which is required for getting a good indication of recall, but at the cost that only a limited number of
positive matches (113) is obtained, and only for a limited number of articles (27). This additional validation
of a random sample of 100 positive matches, for 100 unique guardian articles and 98 unique gtd events, provides a better
indication of the precision. In addition, comments are provided for all matches where it's not immediately obvious
that the article does or does not cover the GTD event.
}
\examples{
head(precision_check)
}
\keyword{datasets}
