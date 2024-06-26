% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readJames2019.R
\name{readJames2019}
\alias{readJames2019}
\alias{convertJames2019}
\title{Read James 2019 updated dataset}
\usage{
readJames2019(subtype)

convertJames2019(x, subtype)
}
\arguments{
\item{subtype}{String indicating the data series}

\item{x}{MAgPIE object returned by readJames2019}
}
\value{
GDP per capita in USD05 in PPP or MER as magpie object
}
\description{
Read-in GDP per-capita data from the publication James, Spencer L., Paul
Gubbins, Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
2015." Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
from a .csv file to a magclass object
}
\details{
2019 dataset from personal communication w/ B Bodirsky
}
\section{Functions}{
\itemize{
\item \code{convertJames2019()}: convert function

}}
\examples{
\dontrun{
readSource("James2019", subtype = "IHME_USD05_PPP_pc")
}

}
\seealso{
\code{\link[madrat:readSource]{madrat::readSource()}} and \code{\link[madrat:downloadSource]{madrat::downloadSource()}}
}
