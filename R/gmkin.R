# Copyright (C) 2015 Johannes Ranke
# Contact: jranke@uni-bremen.de

# This file is part of the R package gmkin

# gmkin is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>

# This makes use of the ::: operator
# To avoid this, gWidgetsWWW2 needs to be adapted

#' Start a graphical user interface (GUI) based on the \code{gWidgetsWWW2} toolkit.
#'
#' This function starts a browser based GUI. Please visit the 
#' \href{http://github.com/jverzani/gWidgetsWWW2}{github page of gWidgetsWWW2} 
#' for an explanation how this toolkit works.
#'
#' @param script_name During development, a script name with a local working
#'   version of gmkin can be passed. Defaults to the location of the gmkin.R
#'   script shipped with the package.
#' @param show.log During development, it may be useful to see the log of the
#' Rook apps.
#' @return The function is called for its side effect, namely starting the GUI
#'   in a browser. For the curious, the desperate or the adventurous, the gmkin
#'   app (a GWidgetsApp object) is returned.
#' @export
gmkin <- function(script_name, show.log = FALSE) {
  if (missing(script_name)) {
    script_name = system.file("GUI/gmkin.R", package = "gmkin")
  }
  session_manager = gWidgetsWWW2:::make_session_manager()
  r_httpd <- gWidgetsWWW2:::R_http$get_instance()
  r_httpd$start()
  r_httpd$load_gw(session_manager)
  gmkin_app <- r_httpd$load_app(script_name, "gmkin", session_manager,
                   open_page = TRUE, show.log = show.log)
  gmkin_png <- Rook::Static$new(
    urls = c("/"),
    root = system.file("GUI/png", package="gmkin"))
  r_httpd$R$add(Rook::RhttpdApp$new(gmkin_png, name="gmkin_png"))
  invisible(gmkin_app)
}

get_current_session <- function(app) {
  s_names <- names(app$session_manager$sessions)
  names(app$session_manager$sessions[[s_names[length(s_names)]]]$e)
  s_env <- app$session_manager$sessions[[s_names[length(s_names)]]]$e
  return(s_env)
}
