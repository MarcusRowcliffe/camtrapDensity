% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/camtrapDensity.R
\name{plot_deployment_schedule}
\alias{plot_deployment_schedule}
\title{Plot a deployment Gantt chart}
\usage{
plot_deployment_schedule(package)
}
\arguments{
\item{package}{Camera trap data package object, as returned by
\code{\link[camtraptor]{read_camtrap_dp}}.}
}
\description{
Plots an interactive Gantt chart illustrating deployment times (black lines)
and the occurrence of observations within those deployments (orange bars).
Useful for checking errors in specification of deployment start and end
dates, and visualising spatiotemporal distribution of observations.
}
\examples{
  \dontrun{pkg <- read_camtrapDP("./data/datapackage.json")}
  data(pkg)
  plot_deployment_schedule(pkg)
}
