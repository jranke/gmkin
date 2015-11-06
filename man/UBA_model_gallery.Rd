\name{UBA_model_gallery}
\alias{UBA_model_gallery}
\docType{data}
\title{
  A collection of some standard kinetic models
}
\description{
  The UBA model gallery contains predefined kinetic models for some common use
  cases, with metabolites named as in the commonly used PELMO degradation
  scheme (Jene 1998).  Please use the model gallery of the \code{\link{gmkin}}
  GUI to see their definitions.  The models use the topology of the models
  defined by Michael Klein (2010) for DegKinManager, with the exception of the
  kinetic sorption model.
}
\usage{data("UBA_model_gallery")}
\format{
  A list containing 9 lists of four \code{\link{mkinmod}} models.
}
\references{
  Klein M (2010) Calculation of rates constants according to FOCUS degradation
  kinetics using DegKinManager and ModelMaker. Project report of UBA project
  FKZ 360 03 048

  Jene B (1998) PELMO 3.00 Manual extension, Staatliche Lehr- und
  Forschungsanstalt für Landwirtschaft, Weinbau und Gartenbau, D-67435
  Neustadt/Wstr.
}
\examples{
data(UBA_model_gallery)
}
\keyword{datasets}
