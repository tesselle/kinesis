% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{get_option}
\alias{get_option}
\title{Get App Options}
\usage{
get_option(name = NULL, default = NULL)
}
\arguments{
\item{name}{A \code{\link{character}} string specifying the name of an option to get.
If \code{NULL} (the default), all options are returned.}

\item{default}{A value to be returned if the option is not currently set.}
}
\value{
The value of a \pkg{Shiny} option (see \code{\link[shiny:shinyOptions]{shiny::getShinyOption()}}).
}
\description{
Get App Options
}
\author{
N. Frerebeau
}
\keyword{internal}
