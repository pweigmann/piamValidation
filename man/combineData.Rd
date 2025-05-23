% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineData.R
\name{combineData}
\alias{combineData}
\title{Combine scenario and reference data with thresholds}
\usage{
combineData(scenData, cfgRow, histData = NULL)
}
\arguments{
\item{scenData}{scenario data for one variable}

\item{cfgRow}{one row of a config file}

\item{histData}{reference data}
}
\description{
for one row of cfg: filter and merge relevant scenario data with cfg
results in one df that contains scenario data, reference data and thresholds
}
