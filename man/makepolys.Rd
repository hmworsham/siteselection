% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{makepolys}
\alias{makepolys}
\title{Make polygons}
\usage{
makepolys(input, radius, shape = "rectangle", target.crs = 32613)
}
\arguments{
\item{input}{Dataframe. Should specify site ID, latitude, and longitude}

\item{radius}{Numeric. Desired radius of output polygon}

\item{shape}{Character string. One of 'rectangle' or 'circle'. Default is 'rectangle'}

\item{target.crs}{Numeric. Desired output CRS in the form of a numeric EPSG code}
}
\value{
sf object containing polygons
}
\description{
Creates polygons from input coordinates, radius, and CRS
}
