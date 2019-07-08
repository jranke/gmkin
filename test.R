library(gmkin)

app <- gmkin(script = "~/git/gmkin/inst/GUI/gmkin.R")
s <- gmkin:::get_current_session(app)

s$show_fit_option_widgets(TRUE)
s$f.run$call_Ext("enable")

names(s)
s$ftmp$reweight.max.iter
s$ftmp$maxit
names(s$ftmp)

s$ws$ftmp$maxit
s$f.run$call_Ext("enable")
s$f.gg.parms[,]
s$show_fit_option_widgets
svalue(s$f.gg.opts.reweight.tol) <-
svalue(s$f.gg.opts.error_model)
svalue(s$f.gg.opts.error_model_algorithm)
visible(s$f.gg.opts.g) <- TRUE
