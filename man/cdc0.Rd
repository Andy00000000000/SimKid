% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdc0.R
\docType{data}
\name{cdc0}
\alias{cdc0}
\title{U.S. Centers for Disease Control and Prevention (CDC) Growth Charts of Weight, Height, and BMI for Age}
\format{
\subsection{\code{cdc0}}{

A data frame with 1,398 rows and 16 columns:
\describe{
\item{CHART}{Growth chart label}
\item{VAR}{Demographic variable (WTKG is weight in kg, HTCM is height in cm, BMI is body mass index in kg/m^2)}
\item{SEXF}{Female sex indicator (0 is male; 1 is female)}
\item{AGEGRP}{Age group bucket in months}
\item{L}{Power in the Box-Cox transformation (calculation of VAR using age)}
\item{M}{Median (calculation of VAR using age)}
\item{S}{Generalized coefficient of variation (calculation of VAR using age)}
\item{P3}{3rd percentile of the given VAR}
\item{P5}{5th percentile of the given VAR}
\item{P10}{10th percentile of the given VAR}
\item{P25}{25th percentile of the given VAR}
\item{P50}{50th percentile of the given VAR}
\item{P75}{75th percentile of the given VAR}
\item{P90}{90th percentile of the given VAR}
\item{P95}{95th percentile of the given VAR}
\item{P97}{97th percentile of the given VAR}
}
}
}
\source{
\url{https://www.cdc.gov/growthcharts/cdc-data-files.htm}
}
\usage{
cdc0
}
\description{
Original CSV data files were manipulated into a more usable format.
}
\keyword{datasets}
