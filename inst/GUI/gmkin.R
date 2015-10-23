# gWidgetsWWW2 GUI for mkin {{{1

# Copyright (C) 2013,2014,2015 Johannes Ranke
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

# Set the GUI title and create the basic widget layout {{{1
# Configuration {{{2
left_width = 250
right_width = 500
save_keybinding = "Ctrl-X"
# Three panel layout {{{2
window_title <- paste0("gmkin ", packageVersion("gmkin"),
                       "- Browser based GUI for kinetic evaluations using mkin")
w  <- gwindow(window_title)
sb <- gstatusbar(paste("Powered by gWidgetsWWW2 (ExtJS, Rook)",
                       "and mkin (FME, deSolve and minpack.lm)",
                       "--- Working directory is", getwd()), cont = w)

bl <- gborderlayout(cont = w,
                    #title = list(center = "Work", east = "Results"),
                    panels = c("center", "west", "east"),
                    collapsible = list(west = FALSE))

bl$set_panel_size("west", left_width)
bl$set_panel_size("east", right_width)

center <- gnotebook(cont = bl, where = "center")
left   <- gvbox(cont = bl, use.scrollwindow = TRUE, where = "west")
right   <- gnotebook(cont = bl, use.scrollwindow = TRUE, where = "east")

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
  else ds.df <<- data.frame(Title = sapply(ws$ds, function(x) x$title))
  ds.gtable[,] <- ds.df
  update_ds_editor()
  ds.delete$call_Ext("disable")  
}
# Update dataframe with models {{{2
update_m.df <- function() {
  if (is.na(ws$m[1])) m.df <<- m.df.empty
  else m.df <<- data.frame(Name = sapply(ws$m, function(x) x$name))
}
# Update dataframe with fits {{{2
update_f.df <- function() {
  f.df <- f.df.empty
  if (!is.na(ftmp[1])) {
    f.df[1, "Name"] <- c("Temporary (not fitted)")
  }
  if (!is.na(ws$f[1])) {
    f.df.ws <- data.frame(Name = sapply(ws$f, function(x) x$name),
                          stringsAsFactors = FALSE)
    f.df <- rbind(f.df, f.df.ws)
  }
  f.df <<- f.df
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
ds.cur <- ds.empty <- mkinds$new(
  title = "", time_unit = "", unit = "",
  data = data.frame(
    name = "parent",
    time = c(0, 1),
    value = c(100, NA),
    override = "NA", err = 1,
    stringsAsFactors = FALSE))
ds.df <- ds.df.empty <- data.frame(Title = "", stringsAsFactors = FALSE)
# Models {{{2
m.df <- m.df.empty <- data.frame(Name = "", stringsAsFactors = FALSE)
# Fits {{{2
f.df <- f.df.empty <- data.frame(Name = "", stringsAsFactors = FALSE)
ftmp <- NA
# left: Explorer tables {{{1
# Frames {{{2
p.gf  <- gframe("Projects", cont = left, horizontal = FALSE)
ds.gf <- gframe("Datasets", cont = left)
m.gf <-  gframe("Models", cont = left)
c.gf <-  gframe("Configuration", cont = left, horizontal = FALSE)
f.gf <-  gframe("Results", cont = left)

# Project explorer {{{2
# Initialize project list from the gmkin package and the current working directory
# The former must be manually amended if additional workspaces should be available
p.gtable <- gtable(p.df, cont = p.gf, width = left_width - 10, height = 120)
size(p.gtable) <- list(columnWidths = c(130, 100))
p.loaded <- NA # The index of the loaded project. We reset the selection to this when the user 
               # does not confirm
p.modified <- TRUE # Keep track of modifications after loading
p.switcher <- function(h, ...) {
  p.cur <- h$row_index # h$row_index for clicked or doubleclick handlers, h$value for change
  project_switched <- FALSE
  switch_project <- function() {
    Name <- p.df[p.cur, "Name"]
    if (p.df[p.cur, "Source"] == "working directory") {
      load(paste0(Name, ".gmkinws"))
      ws <<- ws
    } else {
      ws <<-  get(Name)
    }
    svalue(center) <- 1
    svalue(c.ds) <- empty_conf_labels[1]
    svalue(c.m) <- empty_conf_labels[2]
    update_p_editor(p.cur)
    update_ds.df()
    update_m.df()
    m.gtable[,] <<- m.df
    update_f.df()
    f.gtable[,] <<- f.df
    p.loaded <<- p.cur
    project_switched <- TRUE
    p.gtable$set_index(p.cur)
  }
  if (p.modified) {
    gconfirm("When you switch projects, you loose any unsaved changes. Proceed to switch?",
             handler = function(h, ...) { 
      switch_project()
    })
  } else {
    switch_project()
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
  svalue(center) <- 2
}
ds.gtable <- gtable(ds.df, cont = ds.gf, width = left_width - 10, height = 160)
addHandlerClicked(ds.gtable, ds.switcher)
# Model explorer {{{2
m.switcher <- function(h, ...) {
  m.i <- h$row_index
  svalue(c.m) <- m.df[m.i, "Name"]
  #update_m_editor()
  svalue(center) <- 3
}
m.gtable <- gtable(m.df, cont = m.gf, width = left_width - 10, height = 160)
addHandlerClicked(m.gtable, m.switcher)
# Fit explorer {{{2
f.switcher <- function(h, ...) {
  f.cur <<- h$row_index - 1
  if (f.cur > 0) {
    ftmp <<- ws$f[[ws$f.cur]]
    stmp <<- ws$s[[ws$f.cur]]
    c.ds$call_Ext("setText", 
       paste0("<font color='gray'>", ftmp$ds$title, "</font>"), FALSE)
    c.m$call_Ext("setText", 
       paste0("<font color='gray'>", ftmp$m$name, "</font>"), FALSE)
  }
  #update_f_conf()
  #update_f_results()
  svalue(center) <- 5
}
f.gtable <- gtable(f.df, cont = f.gf, width = left_width - 10, height = 160)
addHandlerClicked(f.gtable, f.switcher)
# Configuration {{{2
empty_conf_labels <- paste0("<font color='gray'>Current ", c("dataset", "model"), "</font>")
c.ds <- glabel(empty_conf_labels[1], cont = c.gf)
c.m <- glabel(empty_conf_labels[2], cont = c.gf)
c.conf <- gbutton("Configure fit", cont = c.gf, handler = function(h, ...) svalue(center) <- 4)
# center: Project editor {{{1
p.editor  <- gframe("", horizontal = FALSE, cont = center, 
                     label = "Project")
# New project {{{2
p.line.buttons <- ggroup(cont = p.editor, horizontal = TRUE)
p.new <- gbutton("New project", cont = p.line.buttons,
  handler = function(h, ...) {
    project_name <- "New project"
    svalue(p.name) <- project_name
    svalue(p.filename) <- file.path(getwd(), paste0(project_name, ".gmkinws"))
    p.delete$call_Ext("disable")
    ws <<- gmkinws$new()
    update_ds.df()
    update_m.df()
    m.gtable[,] <- m.df
    update_f.df()
    f.gtable[,] <- f.df
  })
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
               p.delete$call_Ext("disable")
               update_p.df()
             }
           })
}
p.delete <- gbutton("Delete project", cont = p.line.buttons,
                    handler = p.delete.handler,
                    ext.args = list(disabled = TRUE))
# Project name {{{2
p.line.name <- ggroup(cont = p.editor, horizontal = TRUE)
p.name  <- gedit("New project", label = "<b>Project name</b>",
                 width = 50, cont = p.line.name)
p.save.action <- gaction("Save", parent = w,
  handler = function(h, ...) {
    filename <- paste0(svalue(p.name), ".gmkinws")
    try_to_save <- function (filename) {
      if (!inherits(try(save(ws, file = filename)),
                    "try-error")) {
        svalue(sb) <- paste("Saved project to file", filename,
                            "in working directory", getwd())
        update_p.df()
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
  })
p.save.action$add_keybinding(save_keybinding)
p.save <- gbutton(action = p.save.action, cont = p.line.name)
tooltip(p.save) <- paste("Press", save_keybinding, "to save")

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
}
# Working directory {{{2
p.line.wd <- ggroup(cont = p.editor, horizontal = TRUE)
wd_handler <- function(h, ...) {
 target_wd <- svalue(p.wde)
 wd <- try(setwd(target_wd))
 if (inherits(wd, "try-error")) {
   gmessage(paste("Could not set working directory to", target_wd), parent = w)
 } else {
   svalue(sb) <- paste("Changed working directory to", wd)
   update_p.df()
 }
}
p.wde <- gedit(getwd(), cont = p.line.wd, label = "Working directory", width = 50)
p.wde$add_handler_enter(wd_handler)
p.wdb <- gbutton("Change", cont = p.line.wd, handler = wd_handler)
tooltip(p.wdb) <- "Edit the box on the left and press enter to change"
# File name {{{2
p.line.file <- ggroup(cont = p.editor, horizontal = TRUE)
p.filename.gg  <- ggroup(width = 105, cont = p.line.file) # for spacing
p.filename.label <- glabel("Project file:", cont = p.filename.gg)
p.filename  <- glabel("", cont = p.line.file)
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

p.line.import.dsf <- gframe("Datasets for import", cont = p.line.import.frames, horizontal = FALSE)
p.line.import.dst <- gtable(ds.df.empty, cont = p.line.import.dsf, multiple = TRUE,
                            width = left_width - 10, height = 160,
                            handler = function(h, ...) p.line.import.dsb$call_Ext("enable"))
p.line.import.dsb <- gbutton("Import selected", cont = p.line.import.dsf,
  ext.args = list(disabled = TRUE),
  handler = function(h, ...) {
    i <- svalue(p.line.import.dst, index = TRUE)
    ws$add_ds(ws.import$ds[i])
    update_ds.df()
  }
)

p.line.import.mf <- gframe("Models for import", cont = p.line.import.frames, horizontal = FALSE)
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
  }
)
# center: Dataset editor {{{1
ds.editor <- gframe("", horizontal = FALSE, cont = center, 
                     label = "Dataset")
# Handler functions {{{2
# copy_dataset_handler <- function(h, ...) {
#   ds.old <- ds.cur
#   ds.cur <<- as.character(1 + length(ds))
#   svalue(ds.editor) <- paste("Dataset", ds.cur)
#   ds[[ds.cur]] <<- ds[[ds.old]]
#   update_ds.df()
#   ds.gtable[,] <- ds.df
# }
  
delete_dataset_handler <- function(h, ...) {
  ds.i <- svalue(ds.gtable, index = TRUE)
  ws$delete_ds(ds.i)
  update_ds.df()
}

new_dataset_handler <- function(h, ...) {
  ds.new <- ds.empty
  ds.new$title <- "New dataset"
  ws$add_ds(list(ds.new))
  ds.i <- length(ws$ds)
  ds.cur <<- ws$ds[[ds.i]]
  update_ds.df()
  ds.gtable[,] <- ds.df
  update_ds_editor()
}

# tmptextheader <- character(0)
# load_text_file_with_data <- function(h, ...) {
#   tmptextfile <<- normalizePath(svalue(h$obj), winslash = "/")
#   tmptext <- readLines(tmptextfile, warn = FALSE)
#   tmptextskip <<- 0
#   for (tmptextline in tmptext) {
#     if (grepl(":|#|/", tmptextline)) tmptextskip <<- tmptextskip + 1
#     else break()
#   }
#   svalue(ds.e.up.skip) <- tmptextskip
#   if (svalue(ds.e.up.header)) {
#     tmptextheader <<- strsplit(tmptext[tmptextskip + 1], 
#                              " |\t|;|,")[[1]]
#   }
#   svalue(ds.e.up.wide.time) <- tmptextheader[[1]]
#   svalue(ds.e.up.long.time) <- tmptextheader[[2]]
#   svalue(ds.e.up.text) <- c("<pre>", tmptext, "</pre>")
#   svalue(ds.e.stack) <- 2
# }
#  
# new_ds_from_csv_handler <- function(h, ...) {
#    tmpd <- try(read.table(tmptextfile,
#                           skip = as.numeric(svalue(ds.e.up.skip)), 
#                           dec = svalue(ds.e.up.dec),
#                           sep = switch(svalue(ds.e.up.sep), 
#                                        whitespace = "", 
#                                        ";" = ";",
#                                        "," = ","),
#                           header = svalue(ds.e.up.header),
#                           stringsAsFactors = FALSE))
#   if(svalue(ds.e.up.widelong) == "wide") {
#     tmpdl <- mkin_wide_to_long(tmpd, time = as.character(svalue(ds.e.up.wide.time)))
#   } else {
#     tmpdl <- data.frame(
#       name = tmpd[[svalue(ds.e.up.long.name)]],
#       time = tmpd[[svalue(ds.e.up.long.time)]],
#       value = tmpd[[svalue(ds.e.up.long.value)]])
#     tmpderr <- tmpd[[svalue(ds.e.up.long.err)]]
#     if (!is.null(tmpderr)) tmpdl$err <- tmpderr
#   }
#   if (class(tmpd) != "try-error") {
#     ds.cur <<- as.character(1 + length(ds))
#     ds[[ds.cur]] <<- list(
#                           study_nr = NA,
#                           title = "New import",
#                           sampling_times = sort(unique(tmpdl$time)),
#                           time_unit = "",
#                           observed = unique(tmpdl$name),
#                           unit = "",
#                           replicates = max(aggregate(tmpdl$time,
#                                                        list(tmpdl$time,
#                                                             tmpdl$name),
#                                                      length)$x),
#                           data = tmpdl)
#     ds[[ds.cur]]$data$override <<- as.numeric(NA)
#     if (is.null(ds[[ds.cur]]$data$err)) ds[[ds.cur]]$data$err <<- 1
#     update_ds.df()
#     ds.gtable[,] <- ds.df
#     update_ds_editor()
#   } else {
#     galert("Uploading failed", parent = "w")
#   }
# }
#  
# empty_grid_handler <- function(h, ...) {
#   obs <- strsplit(svalue(ds.e.obs), ", ")[[1]]
#   sampling_times <- strsplit(svalue(ds.e.st), ", ")[[1]]
#   replicates <- as.numeric(svalue(ds.e.rep))
#   new.data = data.frame(
#     name = rep(obs, each = replicates * length(sampling_times)),
#     time = as.numeric(rep(sampling_times, each = replicates, times = length(obs))),
#     value = as.numeric(NA),
#     override = as.numeric(NA),
#     err = 1,
#     stringsAsFactors = FALSE
#   )
#   ds.e.gdf[,] <- new.data
# }

# keep_ds_changes_handler <- function(h, ...) {
#   ds[[ds.cur]]$title <<- svalue(ds.title.ge)
#   ds[[ds.cur]]$study_nr <<- as.numeric(gsub("Study ", "", svalue(ds.study.gc)))
#   update_ds.df()
#   ds.gtable[,] <- ds.df
#   tmpd <- ds.e.gdf[,]
#   ds[[ds.cur]]$data <<- tmpd
#   ds[[ds.cur]]$sampling_times <<- sort(unique(tmpd$time))
#   ds[[ds.cur]]$time_unit <<- svalue(ds.e.stu)
#   ds[[ds.cur]]$observed <<- unique(tmpd$name)
#   ds[[ds.cur]]$unit <<- svalue(ds.e.obu)
#   ds[[ds.cur]]$replicates <<- max(aggregate(tmpd$time, 
#                                             list(tmpd$time, tmpd$name), length)$x)
#   update_ds_editor()
#   observed.all <<- union(observed.all, ds[[ds.cur]]$observed)
#   update_m_editor()
# }
#  
# Widget setup {{{2
# Line 1 {{{3
ds.e.buttons <- ggroup(cont = ds.editor, horizontal = TRUE)
ds.e.new <- gbutton("New dataset", cont = ds.e.buttons, handler = new_dataset_handler)
#gbutton("Copy dataset", cont = ds.e.1, handler = copy_dataset_handler)
ds.delete <- gbutton("Delete dataset", cont = ds.e.buttons, 
  handler = delete_dataset_handler, ext.args = list(disabled = TRUE))
ds.e.2 <- ggroup(cont = ds.editor, horizontal = TRUE)
ds.title.ge <- gedit("", label = "<b>Dataset title</b>", width = 50, cont = ds.e.2)

# # Line 2 {{{3
# ds.e.2 <- ggroup(cont = ds.editor, horizontal = TRUE)
# ds.e.2a <- ggroup(cont = ds.e.2, horizontal = FALSE)

# ds.e.2b <- ggroup(cont = ds.e.2)
# tmptextfile <- "" # Initialize file name for imported data
# tmptextskip <- 0 # Initialize number of lines to be skipped
# tmptexttime <- "V1" # Initialize name of time variable if no header row
# upload_dataset.gf <- gfile(text = "Upload text file", cont = ds.e.2b,
#         handler = load_text_file_with_data)

# gbutton("Keep changes", cont = ds.e.2, handler = keep_ds_changes_handler)

# # Line 3 with forms or upload area {{{3
# ds.e.stack <- gstackwidget(cont = ds.editor)
# # Forms for meta data {{{4
# ds.e.forms <- ggroup(cont = ds.e.stack, horizontal = TRUE)

# ds.e.3a <- gvbox(cont = ds.e.forms)
# ds.e.3a.gfl <- gformlayout(cont = ds.e.3a)
# ds.e.st  <- gedit(paste(ds[[ds.cur]]$sampling_times, collapse = ", "),
#                   width = 40,
#                   label = "Sampling times", 
#                   cont = ds.e.3a.gfl)
# ds.e.stu <- gedit(ds[[ds.cur]]$time_unit, 
#                   width = 20,
#                   label = "Unit", cont = ds.e.3a.gfl)
# ds.e.rep <- gedit(ds[[ds.cur]]$replicates, 
#                   width = 20,
#                   label = "Replicates", cont = ds.e.3a.gfl)

# ds.e.3b <- gvbox(cont = ds.e.forms)
# ds.e.3b.gfl <- gformlayout(cont = ds.e.3b)
# ds.e.obs <- gedit(paste(ds[[ds.cur]]$observed, collapse = ", "),
#                   width = 50,
#                   label = "Observed", cont = ds.e.3b.gfl)
# ds.e.obu <- gedit(ds[[ds.cur]]$unit,
#                   width = 20, label = "Unit", 
#                   cont = ds.e.3b.gfl)
# generate_grid.gb <- gbutton("Generate empty grid for kinetic data", cont = ds.e.3b, 
#         handler = empty_grid_handler)
# tooltip(generate_grid.gb) <- "Overwrites the kinetic data shown below"
# # Data upload area {{{4
# ds.e.upload <- ggroup(cont = ds.e.stack, horizontal = TRUE)
# ds.e.up.text <- ghtml("<pre></pre>", cont = ds.e.upload, width = 400, height = 400)
# ds.e.up.options <- ggroup(cont = ds.e.upload, horizontal = FALSE)
# ds.e.up.import <- gbutton("Import using options specified below", cont = ds.e.up.options,
#                           handler = new_ds_from_csv_handler)
# ds.e.up.skip <- gedit(tmptextskip, label = "Comment lines", cont = ds.e.up.options)
# ds.e.up.header <- gcheckbox(cont = ds.e.up.options, label = "Column names", 
#                             checked = TRUE)
# ds.e.up.sep <- gcombobox(c("whitespace", ";", ","), cont = ds.e.up.options,
#                          selected = 1, label = "Separator")
# ds.e.up.dec <- gcombobox(c(".", ","), cont = ds.e.up.options,
#                          selected = 1, label = "Decimal")
# ds.e.up.widelong <- gradio(c("wide", "long"), horizontal = TRUE, 
#                            label = "Format", cont = ds.e.up.options,
#                            handler = function(h, ...) {
#                              widelong = svalue(h$obj, index = TRUE)
#                              svalue(ds.e.up.wlstack) <- widelong
#                            })
# ds.e.up.wlstack <- gstackwidget(cont = ds.e.up.options)
# ds.e.up.wide <- ggroup(cont = ds.e.up.wlstack, horizontal = FALSE, width = 300)
# ds.e.up.wide.time <- gedit(tmptexttime, cont = ds.e.up.wide, label = "Time column")
# ds.e.up.long <- ggroup(cont = ds.e.up.wlstack, horizontal = FALSE, width = 300)
# ds.e.up.long.name <- gedit("name", cont = ds.e.up.long, label = "Observed variables")
# ds.e.up.long.time <- gedit(tmptexttime, cont = ds.e.up.long, label = "Time column")
# ds.e.up.long.value <- gedit("value", cont = ds.e.up.long, label = "Value column")
# ds.e.up.long.err <- gedit("err", cont = ds.e.up.long, label = "Relative errors")
# svalue(ds.e.up.wlstack) <- 1

# svalue(ds.e.stack) <- 1

# # Kinetic Data {{{3
# ds.e.gdf <- gdf(ds[[ds.cur]]$data, name = "Kinetic data", 
#                 width = 500, height = 700, cont = ds.editor)
# ds.e.gdf$set_column_width(2, 70)

# # Update the dataset editor {{{3
update_ds_editor <- function() {
  svalue(ds.title.ge) <- ds.cur$title
  ds.delete$call_Ext("enable")  
#   svalue(ds.study.gc, index = TRUE) <- ds[[ds.cur]]$study_nr

#   svalue(ds.e.st) <- paste(ds[[ds.cur]]$sampling_times, collapse = ", ")
#   svalue(ds.e.stu) <- ds[[ds.cur]]$time_unit
#   svalue(ds.e.obs) <- paste(ds[[ds.cur]]$observed, collapse = ", ")
#   svalue(ds.e.obu) <- ds[[ds.cur]]$unit
#   svalue(ds.e.rep) <- ds[[ds.cur]]$replicates
#   svalue(ds.e.stack) <- 1
#   ds.e.gdf[,] <- ds[[ds.cur]]$data
}
# center: Model editor {{{1
m.editor  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Model")
# center: Fit configuration {{{1
f.config  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Configuration")
# center: Results viewer {{{1
r.viewer  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Result")
svalue(center) <- 1
# right: Viewing area {{{1
# Workflow {{{2
workflow.gg <- ggroup(cont = right, label = "Workflow", width = 480,  height = 600,
                      ext.args = list(layout = list(type="vbox", align = "center")))

workflow.png <- get_tempfile(ext = ".png")
file.copy(system.file("GUI/gmkin_workflow_434x569.png", package = "gmkin"), workflow.png)
workflow.gi <- gimage(workflow.png, size = c(434, 569), label = "Workflow", cont = workflow.gg)

# Manual {{{2
gmkin_manual <- readLines(system.file("GUI/gmkin_manual.html", package = "gmkin"))
gmb_start <- grep("<body>", gmkin_manual)
gmb_end <- grep("</body>", gmkin_manual)
gmkin_manual_body <- gmkin_manual[gmb_start:gmb_end]

manual.gh <- ghtml(label = "Manual", paste0("<div class = 'manual' style = 'margin: 20px'>
<style>
.manual h1{
  font-size: 14px;
  line-height: 20px;
}
.manual h2{
  font-size: 14px;
  line-height: 20px;
}
.manual h3{
  font-size: 12px;
  line-height: 18px;
}
.manual ul{
  font-size: 12px;
  line-height: 12px;
}
.manual li{
  font-size: 12px;
  line-height: 12px;
}
</style>
", paste(gmkin_manual_body, collapse = '\n'), "
</div>"), width = 460, cont = right)

# Changes {{{2
gmkin_news <- markdownToHTML(system.file("NEWS.md", package = "gmkin"), 
                             fragment.only = TRUE,
                             )

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

svalue(right) <- 1

# Update meta objects and their depending widgets
update_p.df()
# vim: set foldmethod=marker ts=2 sw=2 expandtab: {{{1
