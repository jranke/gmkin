\name{gmkin}
\alias{gmkin}
\title{
  Start a graphical user interface (GUI) based on the \code{gWidgetsWWW2} toolkit.
}
\description{
  This function starts a browser based GUI. Please visit the 
  \href{http://github.com/jverzani/gWidgetsWWW2}{github page of gWidgetsWWW2} 
  for an explanation how this toolkit works.
}
\usage{
  gmkin(script_name, show.log = FALSE)
}
\arguments{
  \item{script_name}{
    During development, a script name with a local working version
    of gmkin can be passed. Defaults to the location of the gmkin.R script
    shipped with the package.
  }
  \item{show.log}{
    During development, it may be useful to see the log of the Rook apps.
  }
}
\examples{
  \dontrun{gmkin()} # Start the gmkin GUI
}
\value{
  The function is called for its side effect, namely starting the GUI in a browser.
}
\author{
  Johannes Ranke
}
