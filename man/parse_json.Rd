% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_json.R
\name{parse_json}
\alias{parse_json}
\title{Parse metadata json files from ITRDB}
\usage{
parse_json(dir, center = TRUE, logfile = NULL)
}
\arguments{
\item{dir}{file directory where json files are saved}

\item{center}{logical whether to report the center of the field site as a single lat/lon or a full location bounding box. Defaults to TRUE.}

\item{logfile}{directory to the log file. If NULL uses the current working directory.}
}
\value{
dataframe of ID, lat, lon, elevation
}
\description{
Parse metadata json files from ITRDB
}
\examples{
\dontrun{

}
}
