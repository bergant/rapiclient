% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operations.R
\name{result_handlers}
\alias{result_handlers}
\alias{content_or_stop}
\alias{content_or_warning}
\alias{content_or_message}
\title{Simple functions to handle http response}
\usage{
content_or_stop(x)

content_or_warning(x)

content_or_message(x)
}
\arguments{
\item{x}{A response object from httr package (see \link[httr]{response}
object in \pkg{httr} package  documentation)}
}
\value{
Content of http response
}
\description{
When creating operations from api one can define
how the response from http should be handled.
These functions can be used for simple result handling.
}
\details{
See \code{\link{get_operations}} for details.
}
\section{Functions}{
\itemize{
\item \code{content_or_warning()}: Returns content or issues a warning

\item \code{content_or_message()}: Returns content or prints a message

}}
\examples{
api_file <- system.file(
  "extdata", "sample_specs", "petstore.yaml",
  package = "rapiclient", mustWork = TRUE
)
api <- get_api(api_file)
operations <- get_operations(api, handle_response = content_or_stop)

}
