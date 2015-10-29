# This makes use of the ::: operator
# To avoid this, gWidgetsWWW2 needs to be adapted
gmkin <- function(script_name, show.log = FALSE) {
  if (missing(script_name)) {
    script_name = system.file("GUI/gmkin.R", package = "gmkin")
  }
  session_manager = gWidgetsWWW2:::make_session_manager()
  r_httpd <- gWidgetsWWW2:::R_http$get_instance()
  r_httpd$start()
  r_httpd$load_gw(session_manager)
  r_httpd$load_app(script_name, "gmkin", session_manager,
                   open_page = TRUE, show.log = show.log)
  gmkin_png <- Rook::Static$new(
    urls = c("/"),
    root = system.file("GUI/png", package="gmkin"))
  r_httpd$R$add(Rook::RhttpdApp$new(gmkin_png, name="gmkin_png"))
}
