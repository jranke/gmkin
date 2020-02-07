\name{FOCUS_2006}
\alias{FOCUS_2006}
\docType{data}
\title{
  Example gmkin workspace for Datasets C and D from the FOCUS Kinetics report
}
\description{
  An \code{\link{gmkinws}} object with data taken from FOCUS (2006), Appendix
  3. This is the the workspace that is loaded into \code{\link{gmkin}} by
  default.
}
\usage{FOCUS_2006}
\format{
  A list named \code{FOCUS_2006} containing the components
  needed to populate the gmkin user interface.
}
\examples{
  \dontrun{
  save(FOCUS_2006, file = "FOCUS_2006.RData")
  # Now you can load the file "FOCUS_2006.RData" from gmkin,
  # restoring the workspace that is loaded at startup.
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
