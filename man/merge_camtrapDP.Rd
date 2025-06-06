% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/camtrapDensity.R
\name{merge_camtrapDP}
\alias{merge_camtrapDP}
\title{Merge Camtrap DP datapackages}
\usage{
merge_camtrapDP(pkgs)
}
\arguments{
\item{pkgs}{A list of camera trap data packages, as returned by
\code{\link{read_camtrapDP}}.}
}
\value{
A single datapackage with component dataframes merged.
}
\description{
Merges a list of several Camtrap DP datapackages into a single datapackage.
}
\details{
Original datapackage metadata are stored in named top-level
 components of the output list, along with a single data component with
 the usual dataframes merged across datapackages, and additional
 \code{packageName} fields indicating the datapackage from which data row
 originates.
}
\examples{
  \dontrun{bigpkg <- merge_camtrapDP(list(pkg1, pkg2))}
}
