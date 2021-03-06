% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_crn.R
\name{read_crn}
\alias{read_crn}
\title{Read Tuscon Format Chronology File}
\usage{
read_crn(fname, header = NULL, encoding = getOption("encoding"), long = TRUE)
}
\arguments{
\item{fname}{a character vector giving the file name of the crn file.}

\item{header}{logical flag indicating whether the file has a header. If NULL then the function will attempt to determine if a header exists}

\item{encoding}{the name of the encoding to be used when reading the crn file. Usually the default value will work, but a crn file written in a non-default encoding may crash the function. In that case, identifying the encoding and specifying it here should fix the problem. Examples of popular encodings available on many systems are "ASCII", "UTF-8", and "latin1" alias "ISO-8859-1". See the help of file.}

\item{long}{logical flag indicating whether to automatically detect when an input file uses more than 4 characters for the decade. If FALSE, the function assumes 6 characters are used for the site ID and 4 characters for the decade, which is the standard. If TRUE (the default), long records may work.}
}
\value{
A data.frame with each chronology in columns and the years as rows. The chronology IDs are the column names and the years are the row names. If the file includes sample depth that is included as the last column (samp.depth). The output class is class "crn" and "data.frame"
}
\description{
This function reads in a Tucson (decadal) format file of tree-ring chronologies (.crn). It is a modification of the dplR function `read.crn` that does not print to screen.
}
\details{
This reads in a standard crn file as defined according to the standards of the ITRDB at http://www1.ncdc.noaa.gov/pub/data/paleo/treering/treeinfo.txt. Despite the standards at the ITRDB, this occasionally fails due to formatting problems.
}
\examples{
\dontrun{
}
}
\author{
Original Authors: Andy Bunn. Patched and improved by Mikko Korpela.
}
