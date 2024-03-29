% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ET_summary.R
\name{ET_summary}
\alias{ET_summary}
\title{Calculate evapotranspiration of various types}
\usage{
ET_summary(Ta, Rn, U2, VPD = VPD, Pa = 101.325, G = NULL, alpha = 1.26)
}
\arguments{
\item{Ta}{air temperature [degC]}

\item{Rn}{surface net radiation [W m-2]}

\item{U2}{2 meters wind speed [m s-1]}

\item{VPD}{vapor pressure deficit [kPa]}

\item{Pa}{air pressure [kPa]}

\item{G}{soil heat flux [W m-2]}

\item{alpha}{Priestley-Taylor coefficient}
}
\value{
A tibble containing various types of evapotranspiration results
}
\description{
Calculate evapotranspiration of various types
}
\examples{
ET_summary(20, 50, 3, 0.5)
}
