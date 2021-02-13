# gWidgetsWWW2 GUI for mkin {{{1

# Copyright (C) 2013-2016,2018,2019,2021 Johannes Ranke
# Portions of this file are copyright (C) 2013 Eurofins Regulatory AG, Switzerland
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

# Configuration {{{1
# Widgets {{{2
# Three column layout
left_width = 250
right_width = 500
# Widget heights in left column
ds_height = 142
m_height = 142
f_height = 142
# Model editor gcombobox with mkinmod model definition
gcb_observed_width = 100
gcb_type_width = 70
gcb_to_width = 160
gcb_sink_width = 70
# Plotting {{{2
plot_formats <- c("png", "pdf")
if (requireNamespace("devEMF", quietly = TRUE)) {
  plot_formats = c("emf", plot_formats)
}
plot_format <- plot_formats[[1]]
# Options {{{2
options(width = 75) # For summary
# Keybinding {{{2
save_keybinding = "Shift-F12"
# Set the GUI title and create the basic widget layout {{{1
# Three panel layout {{{2
window_title <- paste0("gmkin ", packageVersion("gmkin"),
                       "- Browser based GUI for kinetic evaluations using mkin")
w  <- gwindow(window_title)
sb <- gstatusbar(paste("Powered by gWidgetsWWW2 (ExtJS, Rook)",
                       "and mkin (deSolve, numDeriv)",
                       "--- Working directory is", getwd()), cont = w)

bl <- gborderlayout(cont = w,
                    panels = c("center", "west", "east"),
                    collapsible = list(west = FALSE))

bl$set_panel_size("west", left_width)
bl$set_panel_size("east", right_width)

center <- gnotebook(cont = bl, where = "center")
center$add_handler("tabchange",
                  function(h, ...) {
                    if (svalue(h$obj) == 1) {
                      svalue(right) <<- 1
                    }
                  })
left   <- gvbox(cont = bl, use.scrollwindow = TRUE, where = "west", spacing = 0)
right   <- gnotebook(cont = bl, use.scrollwindow = TRUE, where = "east")
right$add_handler("tabchange",
                  function(h, ...) {
                    if (svalue(h$obj) == 3 && ! model_gallery_created) {
                      create_model_gallery()
                    }
                  })
# })

# Helper functions {{{1
# Override function for making it possible to override original data points using the GUI {{{2
override <- function(d) {
  if (!is.null(d$override)) {
    d_new <- data.frame(name = d$name, time = d$time,
                        value = ifelse(is.na(d$override), d$value, d$override),
                        err = d$err)
    return(d_new)
  } else {
    return(d)
  }
}
# Update dataframe with projects {{{2
update_p.df <- function() {
  wd_projects <- gsub(".gmkinws", "", dir(pattern = ".gmkinws$"))
  if (length(wd_projects) > 0) {
    p.df.wd <- data.frame(Name = wd_projects,
                          Source = rep("working directory",
                                       length(wd_projects)),
                          stringsAsFactors = FALSE)
    p.df <<- rbind(p.df.package, p.df.wd)
  } else {
    p.df <<- p.df.package
  }
  p.gtable[,] <- p.df
  p.line.import.p[,] <- c("", p.df$Name)
}
# Update dataframe with datasets {{{2
update_ds.df <- function() {
  if (is.na(ws$ds[1])) ds.df <<- ds.df.empty
  else ds.df <<- data.frame(Title = sapply(ws$ds, function(x) x$title), stringsAsFactors = FALSE)
  ds.gtable[,] <- ds.df
  ds.delete$call_Ext("disable")
  ds.copy$call_Ext("disable")
}
# Update dataframe with models {{{2
update_m.df <- function() {
  if (length(ws$m) == 0) {
    m.df <<- m.df.empty
    m.cur <<- m.empty
  }
  else m.df <<- data.frame(Name = sapply(ws$m, function(x) x$name), stringsAsFactors = FALSE)
  m.gtable[,] <- m.df
  update_m_editor()
  m.delete$call_Ext("disable")
  m.copy$call_Ext("disable")
}
# Update dataframe with fits {{{2
update_f.df <- function() {
  f.df <- data.frame(Name = ws$ftmp$Name, stringsAsFactors = FALSE)
  if (length(ws$f) > 0) {
    f.df.ws <- data.frame(Name = sapply(ws$f, function(x) x$name),
                          stringsAsFactors = FALSE)
    f.df <- rbind(f.df, f.df.ws)
  }
  f.df <<- f.df
  f.gtable[,] <- f.df
  get.initials.gc[,] <- paste("Result", f.df$Name)
  f.delete$call_Ext("disable")
}
# Generate the initial workspace {{{1
# Project workspace {{{2
ws <- gmkinws$new()
ws.import <- NA
# Initialise meta data objects so assignments within functions using <<- will {{{2
# update them in the right environment.
# Also create initial versions of meta data in order to be able to clear the workspace
p.df <- p.df.package <- data.frame(Name = c("FOCUS_2006", "FOCUS_2006_Z"),
                                   Source = rep("gmkin package", 2), stringsAsFactors = FALSE)
# Datasets {{{2
ds.empty <- mkinds$new(
  title = "New dataset", time_unit = "", unit = "",
  data = data.frame(
    name = rep(c("parent", "m1"), each = 5),
    time = rep(c(0, 1, 4, 7, 14), 2),
    value = c(100, rep(NA, 9)),
    override = as.numeric(NA), err = 1,
    stringsAsFactors = FALSE))
ds.cur <- ds.empty$clone()
ds.df <- ds.df.empty <- data.frame(Title = "", stringsAsFactors = FALSE)
# Models {{{2
m.empty <- mkinmod(parent = mkinsub("SFO"))
m.empty$name <- ""
m.empty$spec <- list()
m.cur <- m.empty
m.df <- m.df.empty <- data.frame(Name = "", stringsAsFactors = FALSE)
# Fits {{{2
f.df <- data.frame(Name = "", stringsAsFactors = FALSE)
ws$ftmp <- list(Name = "") # For storing the current configured fit
ftmp <- stmp <- NA         # For storing the currently active fit
# left: Explorer tables {{{1
# Frames {{{2
p.gf  <- gexpandgroup("Projects", cont = left, horizontal = FALSE, spacing = 0)
ds.gf <- gframe("Datasets", cont = left, horizontal = FALSE, spacing = 0)
m.gf <-  gframe("Models", cont = left, horizontal = FALSE, spacing = 0)
c.gf <-  gframe("Configuration", cont = left, horizontal = FALSE, spacing = 0)
f.gf <-  gframe("Results", cont = left, horizontal = FALSE, spacing = 0)

# Project explorer {{{2
# Initialize project list from the gmkin package and the current working directory
# The former must be manually amended if additional workspaces should be available
p.gtable <- gtable(p.df, cont = p.gf, width = left_width - 10, height = 120,
                   ext.args = list(resizable = TRUE, resizeHandles = 's'))
size(p.gtable) <- list(columnWidths = c(130, 100))
p.loaded <- NA # The index of the loaded project. We reset the selection to this when the user
               # does not confirm
p.modified <- FALSE # Keep track of modifications after loading
p.switcher <- function(h, ...) {
  p.cur <- h$row_index # h$row_index for clicked or doubleclick handlers, h$value for change
  project_switched <- FALSE
  switch_project <- function() {
    Name <- p.df[p.cur, "Name"]
    if (p.df[p.cur, "Source"] == "working directory") {
      load(paste0(Name, ".gmkinws"))
      ws <<- ws
    } else {
      ws <<-  get(Name)$clone()
    }
    svalue(center) <- 1
    svalue(c.ds) <- empty_conf_labels[1]
    svalue(c.m) <- empty_conf_labels[2]
    f.conf$call_Ext("disable")
    update_p_editor(p.cur)
    update_ds.df()
    update_m.df()
    update_f.df()
    show_fit_option_widgets(FALSE)
    f.run$call_Ext("disable")
    svalue(f.running.label) <- f.running_noconf
    p.loaded <<- p.cur
    project_switched <- TRUE
    p.modified <<- FALSE
    svalue(p.gtable) <<- p.cur
  }
  if (p.modified) {
    gconfirm("When you switch projects, you loose any unsaved changes. Proceed to switch?",
             handler = function(h, ...) {
      switch_project()
    })
  } else {
    switch_project()
    svalue(right) <<- 1
  }
  # We can reset the selection only if the project was not
  # switched. The following code gets executed during the confirmation dialogue,
  # i.e. before the potential switching
  if (!project_switched) {
    if (is.na(p.loaded)) {
      p.gtable$clear_selection()
    } else {
      p.gtable$set_index(p.loaded)
    }
  }
}
addHandlerClicked(p.gtable, p.switcher)
# Dataset explorer {{{2
ds.switcher <- function(h, ...) {
  ds.i <- h$row_index
  svalue(c.ds) <- ds.df[ds.i, "Title"]
  ds.cur <<- ws$ds[[ds.i]]
  update_ds_editor()
  ds.delete$call_Ext("enable")
  ds.copy$call_Ext("enable")
  if (!is.null(svalue(m.gtable, index = TRUE))) {
    if (length(svalue(m.gtable)) > 0) {
      if (!is.na(svalue(m.gtable))) f.conf$call_Ext("enable")
    }
  }
  svalue(center) <- 2
  svalue(right) <- 2
}
ds.gtable <- gtable(ds.df, cont = ds.gf, width = left_width - 10, height = ds_height,
                    ext.args = list(resizable = TRUE, resizeHandles = 's', hideHeaders = TRUE))
addHandlerClicked(ds.gtable, ds.switcher)
# Model explorer {{{2
m.switcher <- function(h, ...) {
  m.i <- h$row_index
  svalue(c.m) <- m.df[m.i, "Name"]
  m.cur <<- ws$m[[m.i]]
  update_m_editor()
  m.delete$call_Ext("enable")
  m.copy$call_Ext("enable")
  if (!is.null(svalue(ds.gtable, index = TRUE))) {
    if (length(svalue(ds.gtable)) > 0) {
      if (!is.na(svalue(ds.gtable))) f.conf$call_Ext("enable")
    }
  }
  svalue(center) <- 3
}
m.gtable <- gtable(m.df, cont = m.gf, width = left_width - 10, height = m_height,
                   ext.args = list(resizable = TRUE, resizeHandles = 's', hideHeaders = TRUE))
addHandlerClicked(m.gtable, m.switcher)
# Fit explorer {{{2
f.switcher <- function(h, ...) {
  if (h$row_index > 1) {
    f.i <- h$row_index - 1
    ftmp <<- ws$f[[f.i]]
    if (is.null(ftmp$optimised)) ftmp$optimised <<- TRUE
    f.delete$call_Ext("enable")
    f.keep$call_Ext("disable")
  } else {
    ftmp <<- ws$ftmp
  }
  c.ds$call_Ext("setText",
     paste0("<font color='gray'>", ftmp$ds$title, "</font>"), FALSE)
  c.m$call_Ext("setText",
     paste0("<font color='gray'>", ftmp$mkinmod$name, "</font>"), FALSE)
  f.conf$call_Ext("disable")

  ds.gtable$set_index(NA)
  m.gtable$set_index(NA)

  show.initial.gb.o$call_Ext("enable")
  update_f_conf()
  show_plot("Optimised")
  update_f_results()
}
f.gtable <- gtable(f.df, cont = f.gf, width = left_width - 10, height = f_height,
                   ext.args = list(resizable = TRUE, resizeHandles = 's', hideHeaders = TRUE))
addHandlerClicked(f.gtable, f.switcher)
# Configuration {{{2
empty_conf_labels <- paste0("<font color='gray'>Current ", c("dataset", "model"), "</font>")
c.ds <- glabel(empty_conf_labels[1], cont = c.gf, ext.args = list(margin = "0 0 0 5"))
c.m <- glabel(empty_conf_labels[2], cont = c.gf, ext.args = list(margin = "0 0 0 5"))

update_f_conf <- function() { # {{{3
  stmp <<- suppressWarnings(summary(ftmp))
  svalue(f.gg.opts.st) <<- ftmp$solution_type
  svalue(f.gg.opts.atol) <<- ftmp$atol
  svalue(f.gg.opts.rtol) <<- ftmp$rtol
  svalue(f.gg.opts.transform_rates) <<- ftmp$transform_rates
  svalue(f.gg.opts.transform_fractions) <<- ftmp$transform_fractions
  if (!is.null(ftmp$error_model_algorithm)) {
    svalue(f.gg.opts.error_model_algorithm) <<- ftmp$error_model_algorithm
    if (ftmp$error_model_algorithm == "OLS") {
      svalue(f.gg.opts.error_model) <<- "const"
    } else {
      svalue(f.gg.opts.error_model) <<- ftmp$error_model
    }
  }
  svalue(f.gg.opts.reweight.tol) <<- ftmp$reweight.tol
  svalue(f.gg.opts.reweight.max.iter) <<- ftmp$reweight.max.iter
  if (!is.null(ftmp$maxit)) {
    svalue(f.gg.opts.maxit) <<- ftmp$maxit
  }
  show_fit_option_widgets(TRUE)
  update_plot_obssel()
  f.gg.parms[,] <- get_Parameters(stmp, ftmp$optimised)
}
update_f_results <- function() { # {{{3
  svalue(r.name) <- ftmp$name
  r.parameters[] <- cbind(Parameter = rownames(stmp$bpar), stmp$bpar[, c(1, 4, 5, 6)])
  err.min <- 100 * stmp$errmin$err.min
  r.frames.chi2.gt[] <- cbind(Variable = rownames(stmp$errmin),
                              Error = signif(err.min, 3),
                              n.opt = stmp$errmin$n.optim,
                              df = stmp$errmin$df)
  if (is.null(stmp$ff)) r.frames.ff.gt[] <- ff.df.empty
  else r.frames.ff.gt[] <- cbind(Path = names(stmp$ff), ff = round(stmp$ff, 4))
  distimes <- format(stmp$distimes, digits = 3)
  delete(r.frames.distimes, r.frames.distimes.gt)
  delete(r.frames, r.frames.distimes)
  r.frames.distimes <<- gframe("Disappearance times", cont = r.frames, use.scrollwindow = TRUE,
                               horizontal = TRUE, spacing = 0)
  r.frames.distimes.gt <<- gtable(cbind(data.frame(Variable = rownames(stmp$distimes)), distimes),
                                 cont = r.frames.distimes,
                                 height = 150)
  size(r.frames.distimes.gt) <- list(columnWidths = c(60, rep(45, ncol(stmp$distimes))))
  svalue(f.gg.summary.filename) <- paste(ftmp$ds$title, "_", ftmp$mkinmod$name, ".txt", sep = "")
  svalue(f.gg.summary.listing) <- c("<style>
.summary pre{
  font-size: 11px;
  line-height: 16px;
}
</style>
<div class='summary'>
<pre> ", capture.output(summary(ftmp)), "</pre></div>")
  ds.e.gdf[,] <- ftmp$ds$data
  svalue(center) <- 5
}
update_plot_obssel <- function() {
   delete(f.gg.plotopts, f.gg.po.obssel)
   f.gg.po.obssel <<- gcheckboxgroup(names(ftmp$mkinmod$spec),
                                     cont = f.gg.plotopts, checked = TRUE)
}
configure_fit_handler <- function(h, ...) { # Configure fit button {{{3
  if (length(intersect(names(m.cur$spec), ds.cur$observed)) > 0) {
    if (is.null(m.cur$cf) && Sys.which("gcc") != "") {
      mtmp <- mkinmod(speclist = m.cur$spec, use_of_ff = m.cur$use_of_ff)
      mtmp$name <- m.cur$name
      m.cur <<- mtmp
    }
    ftmp <<- suppressWarnings(mkinfit(m.cur,
                              override(ds.cur$data),
                              quiet = TRUE,
                              control = list(iter.max = 0)))
    ftmp$optimised <<- FALSE
    ftmp$ds <<- ds.cur
    ws$ftmp <<- ftmp
    ws$ftmp$Name <<- "Temporary (not fitted)"
    update_f.df()
    update_f_conf()

    f.run$call_Ext("enable")
    svalue(f.running.label) <- "Fit configured and ready to run"
  } else {
    svalue(f.running.label) <- paste("No fit configured:",
                                     "The model and the dataset you selected do",
                                     "not share names for observed variables!")
    f.run$call_Ext("disable")
    show_fit_option_widgets(FALSE)
    show.initial.gb.u$call_Ext("disable")
    show.initial.gb.o$call_Ext("disable")
    f.gg.parms[,] <- Parameters.empty
  }
  svalue(center) <- 4
}
f.conf.line <- ggroup(cont = c.gf,
                      ext.args = list(layout = list(type = "vbox", align = "center")))
f.conf <- gbutton("<b>Configure fit</b>",
                  width = 100,
                  cont = f.conf.line,
                  handler = configure_fit_handler,
                  ext.args = list(disabled = TRUE))

# center: Project editor {{{1
p.editor  <- gframe("", horizontal = FALSE, cont = center,
                     label = "Project")
# Line with buttons {{{2
p.line.buttons <- ggroup(cont = p.editor, horizontal = TRUE)
# New {{{3
p.new.handler <- function(h, ...) {
    project_name <- "New project"
    svalue(p.name) <- project_name
    svalue(p.filename) <- file.path(getwd(), paste0(project_name, ".gmkinws"))
    svalue(p.observed) <- ""
    p.delete$call_Ext("disable")
    ws <<- gmkinws$new()
    update_ds.df()
    update_m.df()
    update_f.df()
}
p.new <- gbutton("New project", cont = p.line.buttons, handler = p.new.handler)

# Delete {{{3
p.delete.handler = function(h, ...) {
  filename <- file.path(getwd(), paste0(svalue(p.name), ".gmkinws"))
  gconfirm(paste0("Are you sure you want to delete ", filename, "?"),
           parent = w,
           handler = function(h, ...) {
             if (inherits(try(unlink(filename)), "try-error")) {
               gmessage("Deleting failed for an unknown reason", cont = w)
             } else {
               svalue(sb) <- paste("Deleted", filename)
               svalue(p.filename) <- ""
               svalue(p.observed) <- ""
               p.delete$call_Ext("disable")
               update_p.df()
             }
           })
}
p.delete <- gbutton("Delete project", cont = p.line.buttons,
                    handler = p.delete.handler,
                    ext.args = list(disabled = TRUE))
# Save {{{3
p.save.action <- gaction("Save project to project file", parent = w,
  handler = function(h, ...) {
    filename <- paste0(svalue(p.name), ".gmkinws")
    if (filename == ".gmkinws") {
      gmessage("Please enter a project name", parent = w)
    } else {
      try_to_save <- function (filename) {
        ws$clear_compiled()
        if (!inherits(try(save(ws, file = filename)),
                      "try-error")) {
          svalue(sb) <- paste("Saved project to file", filename,
                              "in working directory", getwd())
          update_p.df()
          p.modified <<- FALSE
          p.cur <- nrow(p.df)
          svalue(p.filename) <<- file.path(getwd(), filename)
          p.delete$call_Ext("enable")
          svalue(p.observed) <- paste(ws$observed, collapse = ", ")
          # gWidgetsWWW2 problem:
          #svalue(p.gtable) <<- p.cur # does not set the selection
        } else {
          gmessage("Saving failed for an unknown reason", parent = w)
        }
      }
      if (file.exists(filename)) {
        gconfirm(paste("File", filename, "exists. Overwrite?"),
                 parent = w,
                 handler = function(h, ...) {
          try_to_save(filename)
        })
      } else {
        try_to_save(filename)
      }
    }
  })
p.save.action$add_keybinding(save_keybinding)
p.save <- gbutton(action = p.save.action,
                  cont = p.line.buttons)
tooltip(p.save) <- paste("Press", save_keybinding, "to save")

# Project name {{{2
p.line.name <- ggroup(cont = p.editor, horizontal = TRUE)
p.name  <- gedit(label = "<b>Project name</b>",
                 initial.msg = "Enter project name",
                 width = 50, cont = p.line.name)

update_p_editor <- function(p.cur) {
  project_name <-  as.character(p.df[p.cur, "Name"])
  svalue(p.name)  <- project_name
  if (p.df[p.cur, "Source"] == "gmkin package") {
    svalue(p.filename) <- ""
    p.delete$call_Ext("disable")
  } else {
    svalue(p.filename) <- file.path(getwd(), paste0(project_name, ".gmkinws"))
    p.delete$call_Ext("enable")
  }
  svalue(p.observed) <- paste(ws$observed, collapse = ", ")
}
# Working directory {{{2
p.line.wd <- ggroup(cont = p.editor, horizontal = TRUE)
wd_handler <- function(h, ...) {
 target_wd <- svalue(p.wde)
 if (!dir.exists(target_wd)) {
   gmessage(paste("Directory", target_wd, "does not exist"), parent = w)
 } else {
   wd <- try(setwd(target_wd))
   if (inherits(wd, "try-error")) {
     gmessage(paste("Could not set working directory to", target_wd), parent = w)
   } else {
     svalue(sb) <- paste("Changed working directory to", wd)
     update_p.df()
   }
 }
}
p.wde <- gedit(getwd(), cont = p.line.wd, label = "<b>Working directory</b>", width = 50)
p.wde$add_handler_enter(wd_handler)
p.wdb <- gbutton("Change", cont = p.line.wd, handler = wd_handler)
tooltip(p.wdb) <- "Edit the box on the left and press enter to change"
# File name {{{2
p.line.file <- ggroup(cont = p.editor, horizontal = TRUE)
p.filename.gg  <- ggroup(width = 135, cont = p.line.file) # for spacing
p.filename.label <- glabel("Project file:", cont = p.filename.gg)
p.filename  <- glabel("", cont = p.line.file)
# Observed variables {{{2
p.line.observed <- ggroup(cont = p.editor, horizontal = TRUE)
p.observed.gg  <- ggroup(width = 135, cont = p.line.observed) # for spacing
p.observed.label <- glabel("Observed variables:", cont = p.observed.gg)
p.observed  <- glabel("", cont = p.line.observed)
# Import {{{2
p.line.import <- ggroup(cont = p.editor, horizontal = TRUE)
p.line.import.p <- gcombobox(c("", p.df$Name), label = "Import from", cont = p.line.import,
  handler = function(h, ...) {
    p.import <- svalue(h$obj, index = TRUE) - 1
    Name <- p.df[p.import, "Name"]
    if (p.df[p.import, "Source"] == "working directory") {
      load(paste0(Name, ".gmkinws"))
      ws.import <<- ws
    } else {
      ws.import <<- get(Name)
    }
    p.line.import.dst[,] <- data.frame(Title = sapply(ws.import$ds, function(x) x$title),
                                       stringsAsFactors = FALSE)
    p.line.import.mt[,] <- data.frame(Name = sapply(ws.import$m, function(x) x$name),
                                      stringsAsFactors = FALSE)
  })
p.line.import.frames <- ggroup(cont = p.editor, horizontal = TRUE)

p.line.import.dsf <- gframe("Datasets for import", cont = p.line.import.frames,
                            horizontal = FALSE, spacing = 0)
p.line.import.dst <- gtable(ds.df.empty, cont = p.line.import.dsf, multiple = TRUE,
                            width = left_width - 10, height = 160,
                            handler = function(h, ...) p.line.import.dsb$call_Ext("enable"))
p.line.import.dsb <- gbutton("Import selected", cont = p.line.import.dsf,
  ext.args = list(disabled = TRUE),
  handler = function(h, ...) {
    i <- svalue(p.line.import.dst, index = TRUE)
    ws$add_ds(ws.import$ds[i])
    update_ds.df()
    svalue(p.observed) <- paste(ws$observed, collapse = ", ")
    p.modified <<- TRUE
  }
)

p.line.import.mf <- gframe("Models for import", cont = p.line.import.frames,
                           horizontal = FALSE, spacing = 0)
p.line.import.mt <- gtable(m.df.empty, cont = p.line.import.mf, multiple = TRUE,
                            width = left_width - 10, height = 160,
                            handler = function(h, ...) p.line.import.mb$call_Ext("enable"))
p.line.import.mb <- gbutton("Import selected", cont = p.line.import.mf,
  ext.args = list(disabled = TRUE),
  handler = function(h, ...) {
    i <- svalue(p.line.import.mt, index = TRUE)
    ws$add_m(ws.import$m[i])
    update_m.df()
    m.gtable[,] <- m.df
    svalue(p.observed) <- paste(ws$observed, collapse = ", ")
    p.modified <<- TRUE
  }
)
# center: Dataset editor {{{1
ds.editor <- gframe("", horizontal = FALSE, cont = center, width = 540,
                     label = "Dataset")
# Handler functions {{{2
# For top row buttons {{{3
stage_dataset <- function(ds.new) {
  ds.cur <<- ds.new
  update_ds_editor()
  ds.copy$call_Ext("disable")
  ds.delete$call_Ext("disable")
}

new_dataset_handler <- function(h, ...) {
  ds.new <- ds.empty$clone()
  ds.new$title <- "New dataset"
  stage_dataset(ds.new)
}

copy_dataset_handler <- function(h, ...) {
  ds.new <- ds.cur$clone()
  ds.new$title <- paste("Copy of ", ds.cur$title)
  stage_dataset(ds.new)
}

delete_dataset_handler <- function(h, ...) {
  ds.i <- svalue(ds.gtable, index = TRUE)
  ws$delete_ds(ds.i)
  update_ds.df()
  p.modified <<- TRUE
}

keep_ds_changes_handler <- function(h, ...) {
  ds.i <- svalue(ds.gtable, index = TRUE)

  editor_title <- svalue(ds.title.ge)
  editor_ds <- mkinds$new(
    title = editor_title,
    data = ds.e.gdf[,],
    time_unit = svalue(ds.e.stu),
    unit = svalue(ds.e.obu))

  if (!is.null(ds.i) && !is.na(ds.i) && ws$ds[[ds.i]]$title == editor_title) {
      gconfirm(paste("Do you want to overwrite dataset", editor_title, "?"), parent = w,
               handler = function(h, ...) {
                 ws$ds[[ds.i]] <<- editor_ds
                 ds.cur <<- editor_ds
                 update_ds.df()
                 svalue(p.observed) <- paste(ws$observed, collapse = ", ")
                 p.modified <<- TRUE
                 update_ds_editor()
               })
  } else {
    ws$add_ds(list(editor_ds))
    ds.cur <<- editor_ds
    update_ds.df()
    svalue(p.observed) <- paste(ws$observed, collapse = ", ")
    ds.gtable$set_index(length(ws$ds))
    update_ds_editor()
  }
}

# For populating the dataset editor {{{3
empty_grid_handler <- function(h, ...) {
  obs <- strsplit(svalue(ds.e.obs), ", ")[[1]]
  sampling_times_to_parse <- paste0("c(", svalue(ds.e.st), ")")
  sampling_times <- eval(parse(text = sampling_times_to_parse))
  replicates <- as.numeric(svalue(ds.e.rep))
  new.data = data.frame(
    name = rep(obs, each = replicates * length(sampling_times)),
    time = as.numeric(rep(sampling_times, each = replicates, times = length(obs))),
    value = as.numeric(NA),
    override = as.numeric(NA),
    err = 1,
    stringsAsFactors = FALSE
  )
  ds.e.gdf[,] <- new.data
  svalue(right) <- 2
}

# For uploading {{{3
tmptextheader <- character(0)
load_text_file_with_data <- function(h, ...) {
  tmptextfile <<- normalizePath(svalue(h$obj), winslash = "/")
  tmptext <- readLines(tmptextfile, warn = FALSE)
  tmptextskip <<- 0
  for (tmptextline in tmptext) {
    if (grepl(":|#|/", tmptextline)) tmptextskip <<- tmptextskip + 1
    else break()
  }
  svalue(ds.e.up.skip) <- tmptextskip
  if (svalue(ds.e.up.header)) {
    tmptextheader <<- strsplit(tmptext[tmptextskip + 1],
                             " |\t|;|,")[[1]]
  }
  svalue(ds.e.up.wide.time) <- tmptextheader[[1]]
  svalue(ds.e.up.long.time) <- tmptextheader[[2]]
  svalue(ds.e.up.text) <- c("<pre>", c(tmptext[1:5], "\n...\n"), "</pre>")
  visible(ds.e.import) <- TRUE
}

new_ds_from_csv_handler <- function(h, ...) {
   tmpd <- try(read.table(tmptextfile,
                          skip = as.numeric(svalue(ds.e.up.skip)),
                          dec = svalue(ds.e.up.dec),
                          sep = switch(svalue(ds.e.up.sep),
                                       whitespace = "",
                                       ";" = ";",
                                       "," = ","),
                          header = svalue(ds.e.up.header),
                          stringsAsFactors = FALSE))
  if(svalue(ds.e.up.widelong) == "wide") {
    tmpdl <- mkin_wide_to_long(tmpd, time = as.character(svalue(ds.e.up.wide.time)))
    tmpdl$name <- as.character(tmpdl$name)
    tmpdl$override <- NA
    tmpdl$err <- 1
  } else {
    tmpdl <- data.frame(
      name = tmpd[[svalue(ds.e.up.long.name)]],
      time = tmpd[[svalue(ds.e.up.long.time)]],
      value = tmpd[[svalue(ds.e.up.long.value)]])
    tmpderr <- tmpd[[svalue(ds.e.up.long.err)]]
    if (!is.null(tmpderr)) tmpdl$err <- tmpderr
  }
  if (class(tmpd) != "try-error") {
    ds.cur <<- mkinds$new(
      title = "New import",
      time_unit = "",
      unit = "",
      data = tmpdl)
    if (is.null(ds.cur$data$err)) ds.cur$data$err <<- 1
    update_ds.df()
    update_ds_editor()
    svalue(right) <- 2
  } else {
    galert("Uploading failed", parent = "w")
  }
}

# Update the dataset editor {{{3
update_ds_editor <- function() {
  svalue(ds.title.ge) <- ds.cur$title
  svalue(ds.e.st) <- paste(ds.cur$sampling_times, collapse = ", ")
  svalue(ds.e.stu) <- ds.cur$time_unit
  svalue(ds.e.obs) <- paste(ds.cur$observed, collapse = ", ")
  svalue(ds.e.obu) <- ds.cur$unit
  svalue(ds.e.rep) <- ds.cur$replicates
  ds.e.gdf[,] <- ds.cur$data
  ds.keep$call_Ext("enable")
  visible(ds.e.import) <- FALSE
  svalue(ds.e.up.text) <- "<pre></pre>"
}
# Widget setup {{{2
# Line 1 with buttons {{{3
ds.e.buttons <- ggroup(cont = ds.editor, horizontal = TRUE)
ds.e.new <- gbutton("New dataset", cont = ds.e.buttons, handler = new_dataset_handler)
ds.copy <- gbutton("Copy dataset", cont = ds.e.buttons,
  handler = copy_dataset_handler, ext.args = list(disabled = TRUE))
ds.delete <- gbutton("Delete dataset", cont = ds.e.buttons,
  handler = delete_dataset_handler, ext.args = list(disabled = TRUE))
ds.keep <- gbutton("Keep changes", cont = ds.e.buttons, handler = keep_ds_changes_handler)
ds.keep$call_Ext("disable")

# Formlayout for meta data {{{3
ds.e.gfl <- gformlayout(cont = ds.editor)
ds.title.ge <- gedit(label = "<b>Dataset title</b>", width = 60, cont = ds.e.gfl)
addHandlerChanged(ds.title.ge, handler = function(h, ...) ds.keep$call_Ext("enable"))
ds.e.st     <- gedit(width = 60, label = "Sampling times", cont = ds.e.gfl)
ds.e.stu    <- gedit(width = 20, label = "Unit", cont = ds.e.gfl)
ds.e.rep    <- gedit(width = 20, label = "Replicates", cont = ds.e.gfl)
ds.e.obs    <- gedit(width = 60, label = "Observed", cont = ds.e.gfl)
ds.e.obu    <- gedit(width = 20, label = "Unit", cont = ds.e.gfl)
generate_grid.gb.line <- ggroup(cont = ds.editor)
generate_grid.gb <- gbutton("Generate grid for entering kinetic data", cont = generate_grid.gb.line,
  width = 250, handler = empty_grid_handler)
tooltip(generate_grid.gb) <- "Overwrites the kinetic data shown to the right"

# Data upload area {{{3
tmptextfile <- "" # Initialize file name for imported data
tmptextskip <- 0 # Initialize number of lines to be skipped
tmptexttime <- "V1" # Initialize name of time variable if no header row
upload_dataset.gf <- gfile(text = "Upload text file", cont = ds.editor,
        handler = load_text_file_with_data)


# Import options {{{3
ds.e.import <- ggroup(cont = ds.editor, horizontal = FALSE)
visible(ds.e.import) <- FALSE
ds.e.preview <- ggroup(cont = ds.e.import,
                      # width = 540,  height = 150,
                       ext.args = list(layout = list(type="vbox", align = "center")))
ds.e.up.text <- ghtml("<pre></pre>", cont = ds.e.preview, width = 530, height = 150)

ds.e.up.import.line <- ggroup(cont = ds.e.import)
ds.e.up.import <- gbutton("Import using options specified below", cont = ds.e.up.import.line,
                          width = 250, handler = new_ds_from_csv_handler)
ds.e.up.options <- ggroup(cont = ds.e.import, width = 200, horizontal = FALSE)
ds.e.up.skip <- gedit(tmptextskip, label = "Comment lines", width = 20, cont = ds.e.up.options)
ds.e.up.header <- gcheckbox(cont = ds.e.up.options, label = "Column names",
                            checked = TRUE)
ds.e.up.sep <- gcombobox(c("whitespace", ";", ","), cont = ds.e.up.options, width = 50,
                         selected = 1, label = "Separator")
ds.e.up.dec <- gcombobox(c(".", ","), cont = ds.e.up.options, width = 50,
                         selected = 1, label = "Decimal")
ds.e.up.widelong <- gradio(c("wide", "long"), horizontal = TRUE, width = 100,
                           label = "Format", cont = ds.e.up.options,
                           handler = function(h, ...) {
                             widelong = svalue(h$obj, index = TRUE)
                             svalue(ds.e.up.wlstack) <- widelong
                           })
ds.e.up.wlstack <- gstackwidget(cont = ds.e.import)
ds.e.up.wide <- ggroup(cont = ds.e.up.wlstack, horizontal = FALSE, width = 300)
ds.e.up.wide.time <- gedit(tmptexttime, cont = ds.e.up.wide, label = "Time column")
ds.e.up.long <- ggroup(cont = ds.e.up.wlstack, horizontal = FALSE, width = 300)
ds.e.up.long.name <- gedit("name", cont = ds.e.up.long, label = "Observed variables")
ds.e.up.long.time <- gedit(tmptexttime, cont = ds.e.up.long, label = "Time column")
ds.e.up.long.value <- gedit("value", cont = ds.e.up.long, label = "Value column")
ds.e.up.long.err <- gedit("err", cont = ds.e.up.long, label = "Relative errors")
svalue(ds.e.up.wlstack) <- 1

# center: Model editor {{{1
m.editor  <- gframe("", horizontal = FALSE, cont = center, width = 600,
                    label = "Model")
# Handler functions {{{2
# For top row buttons {{{3
stage_model <- function(m.new) {
  m.cur <<- m.new
  update_m_editor()
  m.copy$call_Ext("disable")
  m.delete$call_Ext("disable")
}

new_model_handler <- function(h, ...) {
  m.new <- m.empty
  m.new$name <- "New model"
  stage_model(m.new)
}

copy_model_handler <- function(h, ...) {
  m.new <- m.cur
  m.new$name <- paste("Copy of ", m.cur$name)
  stage_model(m.new)
}

delete_model_handler <- function(h, ...) {
  m.i <- svalue(m.gtable, index = TRUE)
  ws$delete_m(m.i)
  update_m.df()
  p.modified <<- TRUE
}

keep_m_changes_handler <- function(h, ...) {

  spec <- list()
  for (obs.i in 1:length(m.e.rows)) {
    to_string <- svalue(m.e.to[[obs.i]])
    if (length(to_string) == 0) to_vector = NULL
    else to_vector = strsplit(svalue(m.e.to[[obs.i]]), ", ")[[1]]
    spec[[obs.i]] <- mkinsub(svalue(m.e.type[[obs.i]]),
                          to = to_vector,
                          sink = svalue(m.e.sink[[obs.i]]))
    names(spec)[[obs.i]] <- svalue(m.e.obs[[obs.i]])
  }

  m.cur <<- mkinmod(use_of_ff = svalue(m.ff.gc),
                    speclist = spec)
  m.cur$name <<- svalue(m.name.ge)

  m.i <- svalue(m.gtable, index = TRUE)
  if (!is.null(m.i) && !is.na(m.i) && ws$m[[m.i]]$name == m.cur$name) {
    gconfirm(paste("Do you want to overwrite model", m.cur$name, "?"), parent = w,
      handler = function(h, ...) {
        ws$m[[m.i]] <- m.cur
        update_m.df()
        p.modified <<- TRUE
        svalue(p.observed) <- paste(ws$observed, collapse = ", ")
      })
  } else {
    ws$add_m(list(m.cur))
    update_m.df()
    p.modified <<- TRUE
    svalue(p.observed) <- paste(ws$observed, collapse = ", ")
  }
}
# Add and remove observed variables {{{3
add_observed <- function(obs.i) {
  if (obs.i == length(ws$observed)) {
    m.add_observed$call_Ext("disable")
  }
  m.e.rows[[obs.i]] <<- ggroup(cont = m.editor, horizontal = TRUE)
  m.e.obs[[obs.i]] <<- gcombobox(ws$observed,
                                 selected = obs.i,
                                 width = gcb_observed_width,
                                 cont = m.e.rows[[obs.i]])
  obs.types <- if (obs.i == 1) c("SFO", "FOMC", "DFOP", "HS", "SFORB")
    else c("SFO", "SFORB")
  m.e.type[[obs.i]] <<- gcombobox(obs.types, width = gcb_type_width,
                                  selected = 1L, cont = m.e.rows[[obs.i]])
  glabel("to", cont = m.e.rows[[obs.i]])
  m.e.to[[obs.i]] <<- gcombobox(ws$observed, selected = 0L,
                                width = gcb_to_width,
                                editable = TRUE,
                                ext.args = list(multiSelect = TRUE),
                                cont = m.e.rows[[obs.i]])
  m.e.sink[[obs.i]] <<- gcheckbox("Sink", width = gcb_sink_width,
                                  checked = TRUE, cont = m.e.rows[[obs.i]])
  if (obs.i > 1) {
    gbutton("Remove observed variable", handler = remove_observed_handler,
            action = obs.i, cont = m.e.rows[[obs.i]])
  }
}

add_observed_handler <- function(h, ...) {
  obs.i <- length(m.e.rows) + 1
  add_observed(obs.i)
}

remove_observed_handler <- function(h, ...) {
  m.cur$spec[[h$action]] <<- NULL
  update_m_editor()
}
# Update the model editor {{{3
update_m_editor <- function() {
  svalue(m.name.ge) <- m.cur$name
  svalue(m.ff.gc) <- m.cur$use_of_ff
  for (oldrow.i in seq_along(m.e.rows)) {
    delete(m.editor, m.e.rows[[oldrow.i]])
  }
  m.keep$call_Ext("enable")
  m.e.rows <<- m.e.obs <<- m.e.type <<- m.e.to <<- m.e.sink <<- list()
  if (length(m.cur$spec) == length(ws$observed)) {
    m.add_observed$call_Ext("disable")
  } else {
    m.add_observed$call_Ext("enable")
  }
  show_m_spec()
}
# Widget setup {{{2
# Line 1 with buttons {{{3
m.e.buttons <- ggroup(cont = m.editor, horizontal = TRUE)
m.e.new <- gbutton("New model", cont = m.e.buttons, handler = new_model_handler)
m.copy <- gbutton("Copy model", cont = m.e.buttons,
  handler = copy_model_handler, ext.args = list(disabled = TRUE))
m.delete <- gbutton("Delete model", cont = m.e.buttons,
  handler = delete_model_handler, ext.args = list(disabled = TRUE))
m.keep <- gbutton("Keep changes", cont = m.e.buttons, handler = keep_m_changes_handler)
m.keep$call_Ext("disable")

# Formlayout for meta data {{{3
m.e.gfl <- gformlayout(cont = m.editor)
m.name.ge <- gedit(label = "<b>Model name</b>", width = 60, cont = m.e.gfl)
m.ff.gc <- gcombobox(c("min", "max"), label = "Use of formation fractions",
                     cont = m.e.gfl)
svalue(m.ff.gc) <- m.cur$use_of_ff
m.add_observed.line <- ggroup(cont = m.editor)
m.add_observed <- gbutton("Add observed variable",
  width = 150,
  cont = m.add_observed.line,
  handler = add_observed_handler)
m.add_observed$call_Ext("disable")


# Model specification {{{3
m.e.rows <- m.e.obs <- m.e.type <- m.e.to <- m.e.sink <- list()

# Show the model specification {{{4
show_m_spec <- function() {
  for (obs.i in seq_along(m.cur$spec)) {
    obs.name <- names(m.cur$spec)[[obs.i]]

    add_observed(obs.i)

    svalue(m.e.obs[[obs.i]]) <<- obs.name
    svalue(m.e.type[[obs.i]]) <<- m.cur$spec[[obs.i]]$type
    obs.to = m.cur$spec[[obs.i]]$to
    obs.to_string_R = paste(obs.to, collapse = ", ")
    obs.to_string_JS = paste0("['", paste(obs.to, collapse = "', '"), "']")
    # Set R and Ext values separately, as multiple selections are not supported
    svalue(m.e.to[[obs.i]]) <<- obs.to_string_R
    m.e.to[[obs.i]]$call_Ext("select", String(obs.to_string_JS))
    svalue(m.e.sink[[obs.i]]) <<- m.cur$spec[[obs.i]]$sink
  }
}
show_m_spec()


# center: Fit configuration {{{1
f.config  <- gframe("", horizontal = FALSE, cont = center,
                    label = "Configuration")
# Handler functions {{{2
run_confirm_message <- paste("The progress of the fit is shown in the R console. ",
                             if (interactive()) { paste("You can cancel",
                               "the optimisation by switching to the window running R",
                               "and pressing Ctrl-C (in terminals) or Escape (in",
                               "the Windows R GUI). " ) } else "",
                             "Proceed to start the fit?", sep = "")
run_fit_handler <- function(h, ...) { #{{{3
  gconfirm(run_confirm_message, handler = function(h, ...)
    {
      Parameters <- f.gg.parms[,]
      Parameters.de <- subset(Parameters, Type == "deparm")
      deparms <- Parameters.de$Initial
      names(deparms) <- Parameters.de$Name
      defixed <- names(deparms[Parameters.de$Fixed])
      Parameters.ini <- subset(Parameters, Type == "state")
      iniparms <- Parameters.ini$Initial
      names(iniparms) <- sub("_0", "", Parameters.ini$Name)
      inifixed <- names(iniparms[Parameters.ini$Fixed])
      ftmp <<- mkinfit(m.cur, override(ds.cur$data),
                       state.ini = iniparms,
                       fixed_initials = inifixed,
                       parms.ini = deparms,
                       fixed_parms = defixed,
                       solution_type = svalue(f.gg.opts.st),
                       atol = as.numeric(svalue(f.gg.opts.atol)),
                       rtol = as.numeric(svalue(f.gg.opts.rtol)),
                       transform_rates = svalue(f.gg.opts.transform_rates),
                       transform_fractions = svalue(f.gg.opts.transform_fractions),
                       reweight.tol = as.numeric(svalue(f.gg.opts.reweight.tol)),
                       reweight.max.iter = as.numeric(svalue(f.gg.opts.reweight.max.iter)),
                       error_model = svalue(f.gg.opts.error_model),
                       error_model_algorithm = svalue(f.gg.opts.error_model_algorithm)
                       )
      ftmp$optimised <<- TRUE
      ftmp$ds <<- ds.cur
      ws$ftmp <<- ftmp
      ws$ftmp$Name <<- "Temporary (fitted)"
      ftmp$name <<- paste(m.cur$name, "-", ds.cur$title)
      update_f.df()
      stmp <<- summary(ftmp)
      f.gg.parms[,] <- get_Parameters(stmp, TRUE)

      show_plot("Optimised")

      f.keep$call_Ext("enable")
      show.initial.gb.o$call_Ext("enable")
      svalue(f.gg.opts.st) <- ftmp$solution_type

      svalue(f.running.label) <- "Terminated"
      update_f_results()
    })
  svalue(f.running.label) <- "Running..."
}
delete_fit_handler <- function(h, ...) { # {{{3
  f.i <- svalue(f.gtable, index = TRUE)
  if (f.i == 1) {
    gmessage("Will not delete temporary fit")
  } else {
    ws$delete_f(f.i - 1)
    update_f.df()
    p.modified <<- TRUE
  }
}
keep_fit_handler <- function(h, ...) { # {{{3
  ftmp$name <<- svalue(r.name)
  ws$add_f(list(ftmp))
  ws$ftmp <<- list(Name = "")
  update_f.df()
  update_plot_obssel()
  p.modified <<- TRUE
}
export_csv_handler <- function(h, ...) { # {{{3
  csv_file <- paste(ftmp$ds$title, "_", ftmp$mkinmod$name, ".csv", sep = "")

  solution_type = ftmp$solution_type
  parms.all <- c(ftmp$bparms.optim, ftmp$bparms.fixed)

  ininames <- c(
    rownames(subset(ftmp$start, type == "state")),
    rownames(subset(ftmp$fixed, type == "state")))
  odeini <- parms.all[ininames]

  # Order initial state variables
  names(odeini) <- sub("_0", "", names(odeini))
  odeini <- odeini[names(ftmp$mkinmod$diffs)]

  xlim = range(ftmp$data$time)
  outtimes <- seq(xlim[1], xlim[2], length.out=200)

  odenames <- c(
    rownames(subset(ftmp$start, type == "deparm")),
    rownames(subset(ftmp$fixed, type == "deparm")))
  odeparms <- parms.all[odenames]

  out <- mkinpredict(ftmp$mkinmod, odeparms, odeini, outtimes,
          solution_type = solution_type, atol = ftmp$atol, rtol = ftmp$rtol)

  write.csv(out, csv_file)
  svalue(sb) <- paste("Wrote model predictions to", file.path(getwd(), csv_file))
}
get_Parameters <- function(stmp, optimised) # {{{3
{
  pars <- rbind(stmp$start[1:2], stmp$fixed)

  pars$fixed <- c(rep(FALSE, length(stmp$start$value)),
                  rep(TRUE, length(stmp$fixed$value)))
  pars$name <- rownames(pars)
  Parameters <- data.frame(Name = pars$name,
                           Type = pars$type,
                           Initial = pars$value,
                           Fixed = pars$fixed,
                           Optimised = as.numeric(NA))
  Parameters <- rbind(subset(Parameters, Type == "state"),
                      subset(Parameters, Type == "deparm"),
                      subset(Parameters, Type == "error"))
  rownames(Parameters) <- Parameters$Name
  if (optimised) {
    Parameters[rownames(stmp$bpar), "Optimised"] <- stmp$bpar[, "Estimate"]
  }
  return(Parameters)
}
show_plot <- function(type) {
  Parameters <- f.gg.parms[,]
  Parameters.de <- subset(Parameters, Type == "deparm", type)
  stateparms <- subset(Parameters, Type == "state")[[type]]
  deparms <- as.numeric(Parameters.de[[type]])
  names(deparms) <- rownames(Parameters.de)
  if (type == "Initial") {
    ftmp <<- suppressWarnings(mkinfit(m.cur,
                                      override(ds.cur$data),
                                      parms.ini = deparms,
                                      state.ini = stateparms,
                                      fixed_parms = names(deparms),
                                      fixed_initials = names(stateparms),
                                      transform_fraction = FALSE, # to be able to fix ff
                                      quiet = TRUE,
                                      control = list(iter.max = 0)))
    ftmp$ds <<- ds.cur
  }
  svalue(plot.ftmp.gi) <<- plot_ftmp_png()
  svalue(plot.ftmp.savefile) <- paste0(ftmp$mkinmod$name, " - ", ftmp$ds$title, ".", plot_format)
  svalue(plot.confint.gi) <<- if (type == "Initial") NA
    else  plot_confint_png()
  svalue(right) <- 4
}
# Widget setup {{{2
# Line 1 with buttons {{{3
f.run.line <- ggroup(cont = f.config)
f.run <- gbutton("<b>Run fit</b>",
                 width = 100,
                 cont = f.run.line,
                 handler = run_fit_handler,
                 ext.args = list(disabled = TRUE))

f.running.line <- ggroup(cont = f.config)
f.running_noconf <- paste("No fit configured. Please select a dataset and a model and",
                          "press the button 'Configure fit' on the left.")
f.running.label <- glabel(f.running_noconf, cont = f.running.line)

# Fit options forms {{{3
f.gg.opts.g <- ggroup(cont = f.config)

# First group {{{4
f.gg.opts.1 <- gformlayout(cont = f.gg.opts.g)
solution_types <- c("auto", "analytical", "eigen", "deSolve")
f.gg.opts.st <- gcombobox(solution_types, selected = 1,
                          label = "solution_type", width = 160,
                          cont = f.gg.opts.1)
f.gg.opts.atol <- gedit(1e-8, label = "atol", width = 20,
                         cont = f.gg.opts.1)
f.gg.opts.rtol <- gedit(1e-10, label = "rtol", width = 20,
                         cont = f.gg.opts.1)
error_models <- c("const", "obs", "tc")
f.gg.opts.error_model <- gcombobox(error_models, selected = 1,
                                   label = "error_model",
                                   width = 160,
                                   cont = f.gg.opts.1)
error_model_algorithms <- c("d_3", "direct", "threestep", "IRLS", "OLS")
f.gg.opts.error_model_algorithm <- gcombobox(error_model_algorithms, selected = 5,
                                   label = "error_model_algorithm",
                                   width = 160,
                                   cont = f.gg.opts.1)
f.gg.opts.maxit <- gedit(200, label = "maxit",
                         width = 20, cont = f.gg.opts.1)

# Second group {{{4
f.gg.opts.2 <- gformlayout(cont = f.gg.opts.g)
f.gg.opts.transform_rates <- gcheckbox("transform_rates",
                         cont = f.gg.opts.2, checked = TRUE)
f.gg.opts.transform_fractions <- gcheckbox("transform_fractions",
                         cont = f.gg.opts.2, checked = TRUE)
weights <- c("manual", "none", "std", "mean")
f.gg.opts.reweight.tol <- gedit(1e-8, label = "reweight.tol",
                                 width = 20, cont = f.gg.opts.2)
f.gg.opts.reweight.max.iter <- gedit(10, label = "reweight.max.iter",
                                 width = 20, cont = f.gg.opts.2)

f.gg.plotopts <- ggroup(cont = f.gg.opts.g, horizontal = FALSE, width = 80)

f.gg.po.format <- gcombobox(plot_formats, selected = 1,
                  cont = f.gg.plotopts, width = 50,
                  handler = function(h, ...) {
                    plot_format <<- svalue(h$obj)
                    svalue(plot.ftmp.savefile) <<- gsub("...$", plot_format,
                                                       svalue(plot.ftmp.savefile))
                  })
plot_format <- svalue(f.gg.po.format)
f.gg.po.legend <- gcheckbox("legend", cont = f.gg.plotopts, checked = TRUE)
f.gg.po.obssel <- gcheckboxgroup("", cont = f.gg.plotopts,
                                 checked = TRUE)
visible(f.gg.po.obssel) <- FALSE
# Parameter table {{{3
f.parameters.line <- ggroup(cont = f.config, horizontal = TRUE)
get_initials_handler <- function(h, ...)
{
  f.i <- svalue(get.initials.gc, index = TRUE)
  fit <- if (f.i == 1) ftmp
    else ws$f[[f.i - 1]]
  got_initials <- c(fit$bparms.fixed, fit$bparms.optim)
  parnames <- f.gg.parms[,"Name"]
  newparnames <- names(got_initials)
  commonparnames <- intersect(parnames, newparnames)
  f.gg.parms[commonparnames, "Initial"] <- got_initials[commonparnames]
}
get.initials.gb <- gbutton("Get starting parameters from", cont = f.parameters.line,
                           handler = get_initials_handler)
get.initials.gc <- gcombobox(paste("Result", f.df$Name), width = 200, cont = f.parameters.line)
show.initial.gb.u <- gbutton("Plot unoptimised",
                             handler = function(h, ...) show_plot("Initial"),
                             cont = f.parameters.line)
tooltip(show.initial.gb.u) <- "Show model with inital parameters shown below"
show.initial.gb.o <- gbutton("Plot optimised", ext.args = list(disabled = TRUE),
                             handler = function(h, ...) show_plot("Optimised"),
                             cont = f.parameters.line)
tooltip(show.initial.gb.o) <- "Show model with optimised parameters shown below"

# Empty parameter table
Parameters <- Parameters.empty <- data.frame(
  Name = "",
  Type = factor("state", levels = c("state", "deparm")),
  Initial = numeric(1),
  Fixed = logical(1),
  Optimised = numeric(1))
# Dataframe with initial and optimised parameters {{{4
f.gg.parms <- gdf(Parameters, cont = f.config, height = 500,
                  name = "Starting parameters",
                  do_add_remove_buttons = FALSE)
size(f.gg.parms) <- list(columnWidths = c(220, 50, 65, 50, 65))

# Do not show fit option widgets when no fit is configured
show_fit_option_widgets <- function(show)
{
  visible(f.gg.opts.g) <- show
  visible(f.parameters.line) <- show
  visible(f.gg.parms) <- show
}
show_fit_option_widgets(FALSE)

# center: Results viewer {{{1
r.viewer  <- gframe("", horizontal = FALSE, cont = center,
                    label = "Result")
# Row with buttons {{{2
r.buttons <- ggroup(cont = r.viewer, horizontal = TRUE)
f.delete <- gbutton("Delete fit", cont = r.buttons,
  handler = delete_fit_handler, ext.args = list(disabled = TRUE))
f.keep <- gbutton("Keep fit", cont = r.buttons, handler = keep_fit_handler)
tooltip(f.keep) <- "Store the optimised model with all settings and the current dataset in the fit list"
f.keep$call_Ext("disable")
f.csv <- gbutton("Export csv", cont = r.buttons, handler = export_csv_handler)
tooltip(f.csv) <- "Save model predictions in a text file as comma separated values for plotting"
# Result name {{{2
r.line.name <- ggroup(cont = r.viewer, horizontal = TRUE)
r.name  <- gedit("", label = "<b>Result name</b>",
                 width = 50, cont = r.line.name)

# Optimised parameter table {{{2
par.df.empty <- data.frame(
  Parameter = character(1),
  Estimate = numeric(1), "Pr(>t)" = numeric(1),
  Lower = numeric(1), Upper = numeric(1), check.names = FALSE)
r.par.gf <- gframe("Optimised parameters", cont = r.viewer,
                   horizontal = FALSE, spacing = 0)
r.parameters <- gtable(par.df.empty, cont = r.par.gf, height = 200,
                       ext.args = list(resizable = TRUE, resizeHandles = 's'))

# Tables with chi2, ff, DT50 {{{2
r.frames <- ggroup(cont = r.viewer, horizontal = TRUE, spacing = 0)

r.frames.chi2 <- gframe("Chi2 errors [%]", cont = r.frames,
                        horizontal = TRUE, spacing = 0)
chi2.df.empty = data.frame(Variable = character(1), Error = character(1),
                           n.opt = character(1), df = character(1),
                           stringsAsFactors = FALSE)
r.frames.chi2.gt <- gtable(chi2.df.empty, cont = r.frames.chi2,
                            width = 180, height = 150)
size(r.frames.chi2.gt) <- list(columnWidths = c(60, 35, 35, 15))

r.frames.ff <- gframe("Formation fractions", cont = r.frames,
                      horizontal = TRUE, spacing = 0)
ff.df.empty = data.frame(Path = character(1), ff = character(1),
                         stringsAsFactors = FALSE)
r.frames.ff.gt <- gtable(ff.df.empty, cont = r.frames.ff,
                         width = 150, height = 150)
size(r.frames.ff.gt) <- list(columnWidths = c(80, 15))

r.frames.distimes <- gframe("Disappearance times", cont = r.frames,
                            horizontal = TRUE, spacing = 0)
distimes.df.empty = data.frame(Variable = character(1), DT50 = character(1),
                               stringsAsFactors = FALSE)
r.frames.distimes.gt <- gtable(distimes.df.empty, cont = r.frames.distimes,
                               width = 150, height = 150)

# Summary {{{2
f.gg.summary <- gframe("Summary", height = 400, use.scrollwindow = TRUE,
                       cont = r.viewer, horizontal = FALSE)
f.gg.summary.topline <- ggroup(cont = f.gg.summary, horizontal = TRUE)
f.gg.summary.filename <- gedit("", width = 40, cont = f.gg.summary.topline)
f.gg.summary.savebutton <-  gbutton("Save summary", cont = f.gg.summary.topline,
                                    handler = function(h, ...) {
                                      filename <- svalue(f.gg.summary.filename)
                                      if (file.exists(filename))
                                      {
                                        gconfirm(paste("File", filename, "exists. Overwrite?"),
                                                 parent = w,
                                                 handler = function(h, ...) {
                                                   capture.output(stmp,  file = filename)
                                                 })
                                      } else {
                                        capture.output(summary(ftmp),  file = filename)
                                      }
                                    })
f.gg.summary.listing <- ghtml("", cont = f.gg.summary)

svalue(center) <- 1
# right: Viewing area {{{1
# Workflow {{{2
workflow.gg <- ggroup(cont = right, label = "Workflow", width = 480,  height = 570,
                      ext.args = list(layout = list(type="vbox", align = "center")))

workflow_url <- "/custom/gmkin_png/workflow/gmkin_workflow_434x569.png"
workflow.gi <- gimage(workflow_url, size = c(434, 569), label = "Workflow", cont = workflow.gg)

# Data editor {{{2
ds.e.gdf <- gdf(ds.cur$data, label = "Data", name = "Kinetic data",
                do_add_remove_buttons = FALSE,
                width = 488, height = 577, cont = right)

# Model Gallery {{{2
m.g.gg <- ggroup(cont = right, label = "Model gallery",
                 ext.args = list(layout = list(type="vbox", align = "center")))

m.g.rows <- list()
m.g.buttonrows <- list()
m.g.fields <- list()
m.g.buttons <- list()
add_gallery_model_handler <- function(h, ...) {
  i_j <- h$action
  ws$add_m(UBA_model_gallery[[i_j[1]]][i_j[2]])
  update_m.df()
  m.i <- nrow(m.df)
  svalue(c.m) <- m.df[m.i, "Name"]
  m.cur <<- ws$m[[m.i]]
  update_m_editor()
  m.delete$call_Ext("enable")
  m.copy$call_Ext("enable")
  if (!is.null(svalue(ds.gtable, index = TRUE))) {
    if (length(svalue(ds.gtable)) > 0) {
      if (!is.na(svalue(ds.gtable))) f.conf$call_Ext("enable")
    }
  }
  svalue(center) <- 3
}
model_gallery_created <- FALSE
m.g.loading <- glabel("Loading the model gallery, please wait...<br />
  If nothing happens, please try switching to the Data tab and back here.", cont = m.g.gg)
create_model_gallery <- function() {
  delete(m.g.gg, m.g.loading)
  for (i in 1:9) {
    m.g.rows[[i]] <<- ggroup(cont = m.g.gg, horizontal = TRUE)
    m.g.buttonrows[[i]] <<- ggroup(cont = m.g.gg, horizontal = TRUE)
    m.g.fields[[i]] <<- list()
    m.g.buttons[[i]] <<- list()
    for (j in 1:4) {
      model <- UBA_model_gallery[[i]][[j]]
      m.url = paste0("/custom/gmkin_png/", gsub(" ", "_", model$name), ".png")
      m.g.fields[[i]][[j]] <<- gimage(m.url, width = 110,
                                     height = if (i == 1) 80 else if (i == 2) 160 else 220,
                                     cont = m.g.rows[[i]])
      m.g.buttons[[i]][[j]] <<- gbutton(model$name, width = 110,
                                       cont = m.g.buttonrows[[i]],
                                       handler = add_gallery_model_handler,
                                       action = c(i, j))
      tooltip(m.g.buttons[[i]][[j]]) <<- model$name
    }
  }
  model_gallery_created <<- TRUE
}
# Plots {{{2
plot.gg <- ggroup(cont = right, label = "Plot", width = 460,
                      ext.args = list(layout = list(type="vbox", align = "center")))

plot_ftmp <- function() {
  if (length(svalue(f.gg.po.obssel)) == 0) {
    gmessage("Please select more than one variable for plotting.")
  } else {
    if(svalue(f.gg.po.obssel)[1] != "") {
      obs_vars_plot = svalue(f.gg.po.obssel)
    } else {
      obs_vars_plot = names(ftmp$mkinmod$spec)
    }
    if(exists("f.gg.po.legend")) {
      plot_legend = svalue(f.gg.po.legend)
    } else {
      plot_legend = TRUE
    }
    plot(ftmp, main = paste(ftmp$mkinmod$name, "-", ftmp$ds$title),
         obs_vars = obs_vars_plot,
         xlab = ifelse(ftmp$ds$time_unit == "", "Time",
                       paste("Time in", ftmp$ds$time_unit)),
         ylab = ifelse(ftmp$ds$unit == "", "Observed",
                       paste("Observed in", ftmp$ds$unit)),
         legend = plot_legend,
         show_residuals = TRUE)
  }
}

plot_ftmp_png <- function() {
  tf <- get_tempfile(ext=".png")
  png(tf, width = 400, height = 400)
  plot_ftmp()
  dev.off()
  return(tf)
}

plot_ftmp_save <- function(filename) {
  switch(plot_format,
         png = png(filename, width = 400, height = 400),
         pdf = pdf(filename),
         emf = devEMF::emf(filename))
  plot_ftmp()
  dev.off()
  svalue(sb) <- paste("Saved plot to", filename, "in working directory", getwd())
}

plot_confint_png <- function() {
  tf <- get_tempfile(ext=".png")
  png(tf, width = 400, height = 400)
  mkinparplot(ftmp)
  dev.off()
  return(tf)
}


plot.ftmp.gi <- gimage(NA, container = plot.gg, width = 400, height = 400)
plot.ftmp.saveline <- ggroup(cont = plot.gg, horizontal = TRUE)
plot.ftmp.savefile <- gedit("", width = 40, cont = plot.ftmp.saveline)
plot.ftmp.savebutton <-  gbutton("Save plot", cont = plot.ftmp.saveline,
                                    handler = function(h, ...) {
                                      filename <- svalue(plot.ftmp.savefile)
                                      if (file.exists(filename))
                                      {
                                        gconfirm(paste("File", filename,
                                                       "exists. Overwrite?"),
                                                 parent = w,
                                                 handler = function(h, ...) {
                                            plot_ftmp_save(filename)
                                          }
                                        )
                                      } else {
                                        plot_ftmp_save(filename)
                                      }
                                    })
plot.space <- ggroup(cont = plot.gg, horizontal = FALSE, height = 8)
plot.confint.label <- glabel("<b>Parameter confidence intervals</b>", cont = plot.gg)
plot.confint.gi <- gimage(NA, container = plot.gg, width = 400, height = 400)
# Manual {{{2
manual_html <- readLines(system.file("GUI/manual.html", package = "gmkin"))

manual.gh <- ghtml(label = "Manuals", paste0("<div class = 'manual' style = 'margin: 20px'>
<style>
.manual h1{
  font-size: 14px;
  line-height: 20px;
}
</style>
", paste(manual_html, collapse = '\n'), "
</div>"), width = 460, cont = right)

# Changes {{{2
gmkin_news <- markdown::markdownToHTML(system.file("NEWS.md",
                                                   package = "gmkin"),
                                       fragment.only = TRUE)

changes.gh <- ghtml(label = "Changes", paste0("<div class = 'news' style = 'margin: 20px'>
<style>
.news h1{
  font-size: 14px;
  line-height: 20px;
}
.news h2{
  font-size: 14px;
  line-height: 20px;
}
.news h3{
  font-size: 12px;
  line-height: 18px;
}
.news ul{
  font-size: 12px;
  line-height: 12px;
}
.news li{
  font-size: 12px;
  line-height: 12px;
}
</style>
", gmkin_news, "
</div>"), width = 460, cont = right)

# Things to do in the end {{{1
# Update meta objects and their depending widgets
svalue(right) <- 1
update_p.df()
# vim: set foldmethod=marker ts=2 sw=2 expandtab: {{{1
