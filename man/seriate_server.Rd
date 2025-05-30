% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seriate.R
\name{seriate_server}
\alias{seriate_server}
\title{CA Seriation Server}
\usage{
seriate_server(id, x, order)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's
UI function.}

\item{x}{A reactive \code{data.frame} (typically returned by \code{\link[=import_server]{import_server()}}).}

\item{order}{A reactive \R object (coercible by \code{\link[kairos:as_seriation]{kairos::as_seriation()}}).}
}
\value{
A reactive \code{\link[kairos:PermutationOrder-class]{kairos::AveragePermutationOrder}} object.
}
\description{
CA Seriation Server
}
\seealso{
\code{\link[=seriate_ui]{seriate_ui()}}

Other chronology modules: 
\code{\link{aoristic_server}()},
\code{\link{aoristic_ui}()},
\code{\link{seriate_ui}()},
\code{\link{time_interval_server}()},
\code{\link{time_interval_ui}()}
}
\concept{chronology modules}
\keyword{internal}
