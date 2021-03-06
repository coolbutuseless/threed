% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh3d-conversion.R
\name{as.mesh3d.data.frame}
\alias{as.mesh3d.data.frame}
\title{Convert data.frame into mesh3d object}
\usage{
\method{as.mesh3d}{data.frame}(x, ...)
}
\arguments{
\item{x}{data.frame with columns: x, y, z, element_id  Optional columns include:
c('vnx', 'vny', 'vnz') for vertex normals.  c('fnx', 'fny', 'fnz')
for face normals.}

\item{...}{ignored}
}
\value{
mesh3d object
}
\description{
Convert data.frame into mesh3d object
}
