% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kinesis-internal.R
\name{build_numeric_input}
\alias{build_numeric_input}
\alias{render_numeric_input}
\title{Build Numeric Input}
\usage{
build_numeric_input(id, x)

render_numeric_input(id)
}
\arguments{
\item{id}{A \code{\link{character}} string specifying the namespace.}

\item{x}{A reactive \code{data.frame} (typically returned by \code{\link[=import_server]{import_server()}}).}
}
\value{
\itemize{
\item \code{build_numeric_input()} returns a reactive \code{\link{numeric}} vector
(side effect: render numeric input controls).
\item \code{render_numeric_input()} is called for its side effects
(creates UI elements).
}
}
\description{
Build Numeric Input
}
\keyword{internal}
