% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validationReport.R
\name{validationReport}
\alias{validationReport}
\title{perform validateScenarios and create an .html report using .Rmd templates}
\usage{
validationReport(dataPath, config, report = "default", outputDir = "output")
}
\arguments{
\item{dataPath}{one or multiple path(s) to scenario data in .mif or .csv
format}

\item{config}{name a config from inst/config ("validationConfig_<name>.csv")
or give a full path to a separate configuration file}

\item{report}{name a .Rmd from inst/markdown ("validationReport_<name>.Rmd")
to be rendered or give a full path to a separate .Rmd file}

\item{outputDir}{choose a directory to save validation reports to}
}
\description{
perform validateScenarios and create an .html report using .Rmd templates
}
