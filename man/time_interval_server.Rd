% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_interval.R
\name{time_interval_server}
\alias{time_interval_server}
\title{Aoristic Analysis Server}
\usage{
time_interval_server(id, x)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's
UI function.}

\item{x}{A reactive \code{data.frame}.}
}
\value{
A reactive \code{\link{list}}.
}
\description{
Aoristic Analysis Server
}
\seealso{
\code{\link[=time_interval_ui]{time_interval_ui()}}

Other chronology modules: 
\code{\link{aoristic_server}()},
\code{\link{aoristic_ui}()},
\code{\link{seriate_server}()},
\code{\link{seriate_ui}()},
\code{\link{time_interval_ui}()}
}
\concept{chronology modules}
\keyword{internal}
