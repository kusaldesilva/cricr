% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPL_Match_ID.R
\name{iplmatchdata}
\alias{iplmatchdata}
\title{Scrape match id data of IPL matches}
\usage{
iplmatchdata(Year, TeamGender)
}
\arguments{
\item{Year}{IPL tournament year}

\item{TeamGender}{gender of the team}
}
\value{
data frame with IPL match id data
}
\description{
Scrape match id data of IPL matches
}
\examples{
iplmatchdata()
iplmatchdata(2008:2012, 'men')
}
\author{
Kusal De Silva and Rajitha M. Silva
}
