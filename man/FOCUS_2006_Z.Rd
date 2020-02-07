\name{FOCUS_2006_Z}
\alias{FOCUS_2006_Z}
\docType{data}
\title{
  Example gmkin workspace for Dataset Z from the FOCUS Kinetics report
}
\description{
  Data taken from FOCUS (2006), Appendix 7, p. 350.
}
\usage{FOCUS_2006_Z}
\format{
  An \code{\link{gmkinws}} object with dataset Z taken from FOCUS (2006),
  together with most of the models used in the corresponding vignette.
}
\examples{
  \dontrun{
  save(FOCUS_2006_Z, file = "FOCUS_2006_Z.RData")
  # Now you can load the file "FOCUS_2006_Z.RData" from gmkin
  }
}
\source{
  FOCUS (2006) \dQuote{Guidance Document on Estimating Persistence and
  Degradation Kinetics from Environmental Fate Studies on Pesticides in EU
  Registration} Report of the FOCUS Work Group on Degradation Kinetics,
  EC Document Reference Sanco/10058/2005 version 2.0, 434 pp,
  \url{http://esdac.jrc.ec.europa.eu/projects/degradation-kinetics}
}
\keyword{datasets}
