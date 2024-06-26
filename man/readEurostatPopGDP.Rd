% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readEurostatPopGDP.R
\name{downloadEurostatPopGDP}
\alias{downloadEurostatPopGDP}
\alias{readEurostatPopGDP}
\alias{convertEurostatPopGDP}
\title{Read Eurostat Population and GDP data}
\usage{
downloadEurostatPopGDP(subtype)

readEurostatPopGDP(subtype)

convertEurostatPopGDP(x, subtype)
}
\arguments{
\item{subtype}{A string. Available subtypes are:
\itemize{
\item "population": Population, ref demo_gind
\item "population_projections": Population projections, ref proj_19np
\item "GDP": GDP, ref nama_10_gdp
\item "GDPgr_projections_short": Projected GDP growth rates, 2023 forecast.
\item "GDPgr_projections_long":
}}

\item{x}{MAgPIE object returned by readEurostatPopGDP}
}
\value{
The read-in data, usually a magpie object. If supplementary is TRUE a list including
the data and metadata is returned instead. The temporal and data dimensionality
should match the source data. The spatial dimension should either match the source data or,
if the convert argument is set to TRUE, should be on ISO code country level.
}
\description{
Download, read and convert Eurostat population and GDP data.
}
\examples{
\dontrun{
readSource("EurostatPopGDP", subtype = "population")
}
}
\seealso{
\code{\link[madrat:readSource]{madrat::readSource()}} and \code{\link[madrat:downloadSource]{madrat::downloadSource()}}
}
