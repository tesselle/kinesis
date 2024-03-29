% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_data.R
\name{missing_server}
\alias{missing_server}
\title{Missing Values Server}
\usage{
missing_server(id, x)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's
UI function.}

\item{x}{A reactive \code{data.frame} (typically returned by
\code{\link[=import_server]{import_server()}}).}
}
\value{
A reactive \code{data.frame}.
}
\description{
Missing Values Server
}
\seealso{
\code{\link[=missing_ui]{missing_ui()}}

Other generic modules: 
\code{\link{data_server}()},
\code{\link{data_ui}()},
\code{\link{import_server}()},
\code{\link{import_ui}()},
\code{\link{missing_ui}()},
\code{\link{prepare_server}()},
\code{\link{prepare_ui}()}
}
\concept{generic modules}
\keyword{internal}
