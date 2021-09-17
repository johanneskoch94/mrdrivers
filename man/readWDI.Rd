% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readWDI.R
\name{readWDI}
\alias{readWDI}
\title{Read WDI}
\usage{
readWDI(subtype)
}
\arguments{
\item{subtype}{A string. Type of WDI data that should be read. Use the
worldbank indicator abbreviation. Available subtypes are: \itemize{
\item \code{"SP.POP.TOTL"}: Population, total
\item \code{"NY.GDP.MKTP.PP.KD"}: GDP,PPP (constant 2011 international Dollar)
\item \code{"NY.GDP.MKTP.PP.CD"}: GDP, PPP (current international Dollar)
\item \code{"NY.GDP.MKTP.CD"}: GDP MER (current US Dollar)
\item \code{"NY.GDP.MKTP.KD"}: GDP MER (constant 2010 USDollar)
\item \code{"NY.GDP.MKTP.KN"}: GDP LCU (constant LCU)
\item \code{"NY.GDP.MKTP.CN"}: GDP LCU (current LCU)
\item \code{"NY.GDP.DEFL.KD.ZG"}: Country GDP deflator (annual \%)
\item \code{"SP.URB.TOTL.IN.ZS"}: Urban population (percentage of total)
\item \code{"NY.GDP.PCAP.CN"}:GDP, LCU, per capita (current LCU)
\item \code{"NY.GDP.PCAP.PP.KD"}: GDP PPP, per capita (2017 international $)
\item \code{"NY.GDP.PCAP.KD"}: GDP, MER, per capita (2010 US$)
\item \code{"NV.AGR.TOTL.KD"}: Ag GDP, MER, (2010 US$)
\item \code{"NV.AGR.TOTL.CD"}: Ag GDP, MER, (current US$)
\item \code{"NY.GDP.PCAP.CD"}: GDP MER, per capita(current US$)
\item \code{"NY.GDP.PCAP.PP.CD"}: GDP PPP, per capita (current int$)}}
}
\value{
A magpie object of the WDI data
}
\description{
Read-in WDI (World development indicators) data .Rds file as magpie
object.
}
\examples{
\dontrun{ 
library(mrdrivers)
readSource("WDI", subtype = "SP.POP.TOTL")}

}
\seealso{
\code{\link[madrat:readSource]{madrat::readSource()}}

Other "Past" population functions: 
\code{\link{readEurostat}()},
\code{\link{readMissingIslands}()},
\code{\link{readUN_PopDiv}()}

Other "Past" GDP functions: 
\code{\link{readEurostat}()},
\code{\link{readMissingIslands}()}

Other "Past" GDPpc functions: 
\code{\link{readIMF}()},
\code{\link{readJames2019}()},
\code{\link{readPWT}()}

Other WDI functions: 
\code{\link{convertWDI}()},
\code{\link{downloadWDI}()}
}
\concept{"Past" GDP functions}
\concept{"Past" GDPpc functions}
\concept{"Past" population functions}
\concept{WDI functions}