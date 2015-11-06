\docType{class}
\name{gmkinws}
\alias{gmkinws}
\title{A workspace class for gmkin}
\format{An \code{\link{R6Class}} generator object.}
\usage{
gmkinws
}
\description{
Datasets, models and fits are stored in lists.
}
\section{Fields}{

\describe{
\item{\code{observed}}{Names of the observed variables in the datasets, named
by the names used in the models contained in field m}

\item{\code{ds}}{A list of datasets compatible with mkinfit (long format)}

\item{\code{m}}{A list of mkinmod models}

\item{\code{f}}{A list of mkinfit objects}
}}
