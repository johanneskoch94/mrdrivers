% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolFillWith.R
\name{toolFillWith}
\alias{toolFillWith}
\title{toolFillWith}
\usage{
toolFillWith(data, fill, dataName = "data", fillName = "fill", verbose = FALSE)
}
\arguments{
\item{data}{magpie object to fill}

\item{fill}{magpie object to fill data with}
}
\value{
The completed magpie object
}
\description{
Fill in countries in "data" with no values, with values from "fill". Only countries with no data at all get filled
in!
}
\keyword{internal}
