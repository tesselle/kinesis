% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coda_statistics.R
\name{coda_summary_server}
\alias{coda_summary_server}
\title{Compositional Data Summary Server}
\usage{
coda_summary_server(id, x)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's
UI function.}

\item{x}{A reactive \code{\link[nexus:CompositionMatrix-class]{nexus::CompositionMatrix}} object.}
}
\value{
No return value, called for side effects.
}
\description{
Compositional Data Summary Server
}
\seealso{
\code{\link[=coda_summary_ui]{coda_summary_ui()}}

Other coda modules: 
\code{\link{coda_barplot_server}()},
\code{\link{coda_barplot_ui}()},
\code{\link{coda_hclust_server}()},
\code{\link{coda_hclust_ui}()},
\code{\link{coda_hist_server}()},
\code{\link{coda_hist_ui}()},
\code{\link{coda_outliers_server}()},
\code{\link{coda_outliers_ui}()},
\code{\link{coda_server}()},
\code{\link{coda_summary_ui}()},
\code{\link{coda_ui}()},
\code{\link{logratio_server}()},
\code{\link{logratio_ui}()}
}
\concept{coda modules}
\keyword{internal}
