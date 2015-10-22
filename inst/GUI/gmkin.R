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
# Widgets {{{2
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
}
# Update dataframe with datasets {{{2
update_ds.df <- function() {
  ds.df <<- data.frame(Title = sapply(ws$ds, function(x) x$title))
}
# Update dataframe with models {{{2
update_m.df <- function() {
  m.df <<- data.frame(Name = names(ws$m))
}
# Update dataframe with fits {{{2
update_f.df <- function() {
  f.df <- f.df.empty
  if (!is.na(ftmp[1])) {
    f.df[1, "Name"] <- c("Temporary (not fitted)")
  }
  if (!is.na(ws$f)) {
    f.df.ws <- data.frame(Name = names(ws$f), stringsAsFactors = FALSE)
    f.df <- rbind(f.df, f.df.ws)
  }
  f.df <<- f.df
}
# Generate the initial workspace {{{1
ws <- gmkinws$new()
ws.import <- NA
# Initialise meta data objects so assignments within functions using <<- will {{{2
# update them in the right environment.
# Also create initial versions of meta data in order to be able to clear the workspace
p.df <- p.df.package <- data.frame(Name = c("FOCUS_2006", "FOCUS_2006_Z"),
                                   Source = rep("gmkin package", 2), stringsAsFactors = FALSE)

update_p.df()
ds.df <- ds.df.empty <- data.frame(Title = "", stringsAsFactors = FALSE)
m.df <- m.df.empty <- data.frame(Name = "", stringsAsFactors = FALSE)
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
p.switcher <- function(h, ...) {
  p.cur <- h$row_index
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
  ds.gtable[,] <<- ds.df
  update_m.df()
  m.gtable[,] <<- m.df
  update_f.df()
  f.gtable[,] <<- f.df
}
addHandlerClicked(p.gtable, p.switcher)
# Dataset explorer {{{2
ds.switcher <- function(h, ...) {
  ws$ds.cur <<- h$row_index
  svalue(c.ds) <- ds.df[ws$ds.cur, "Title"]
  #update_ds_editor()
  svalue(center) <- 2
}
ds.gtable <- gtable(ds.df, cont = ds.gf, width = left_width - 10, height = 160)
addHandlerClicked(ds.gtable, ds.switcher)
# Model explorer {{{2
m.switcher <- function(h, ...) {
  ws$m.cur <<- h$row_index
  svalue(c.m) <- m.df[ws$m.cur, "Name"]
  #update_m_editor()
  svalue(center) <- 3
}
m.gtable <- gtable(m.df, cont = m.gf, width = left_width - 10, height = 160)
addHandlerClicked(m.gtable, m.switcher)
# Fit explorer {{{2
f.switcher <- function(h, ...) {
  ws$f.cur <<- h$row_index - 1
  if (ws$f.cur > 0) {
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
   p.gtable[,] <- p.df
   p.line.import.p[,] <- c("", p.df$Name)
 }
}
p.wde <- gedit(getwd(), cont = p.line.wd, label = "Working directory", width = 50)
p.wde$add_handler_enter(wd_handler)
p.wdb <- gbutton("Change", cont = p.line.wd, handler = wd_handler)
tooltip(p.wdb) <- "Edit the box on the left and press enter to change"
# Project name {{{2
p.line.name <- ggroup(cont = p.editor, horizontal = TRUE)
p.name  <- gedit("New project", label = "Project name",
                 width = 50, cont = p.line.name)
p.save <- gaction("Save", parent = w,
                  handler = function(h, ...) {
                    filename <- paste0(svalue(p.name), ".gmkinws")
                    try_to_save <- function (filename) {
                      if (!inherits(try(save(ws, file = filename)),
                                    "try-error")) {
                        svalue(sb) <- paste("Saved project to file", filename,
                                            "in working directory", getwd())
                        update_p.df()
                        p.gtable[,] <- p.df
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
p.save.button <- gbutton(action = p.save, cont = p.line.name)
p.save$add_keybinding(save_keybinding)
tooltip(p.save.button) <- paste("Press", save_keybinding, "to save")

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
# File name {{{2
p.line.file <- ggroup(cont = p.editor, horizontal = TRUE)
p.filename.gg  <- ggroup(width = 400, cont = p.line.file)
p.filename  <- glabel("", cont = p.filename.gg)
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
               p.gtable[,] <- p.df
             }
           })
}
p.delete <- gbutton("Delete", cont = p.line.file,
                    handler = p.delete.handler)
p.delete$call_Ext("disable")
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
    p.line.import.mt[,] <- data.frame(Name = names(ws.import$m),
                                      stringsAsFactors = FALSE)
  })
p.line.import.frames <- ggroup(cont = p.editor, horizontal = TRUE)

p.line.import.dsf <- gframe("Datasets for import", cont = p.line.import.frames, horizontal = FALSE)
p.line.import.dst <- gtable(ds.df.empty, cont = p.line.import.dsf, multiple = TRUE,
                            width = left_width - 10, height = 160)
p.line.import.dsb <- gbutton("Import selected", cont = p.line.import.dsf,
  handler = function(h, ...) {
    i <- svalue(p.line.import.dst, index = TRUE)
    ws$ds <<- append(ws$ds, ws.import$ds[i])
    update_ds.df()
    ds.gtable[,] <- ds.df
  }
)

p.line.import.mf <- gframe("Models for import", cont = p.line.import.frames, horizontal = FALSE)
p.line.import.mt <- gtable(m.df.empty, cont = p.line.import.mf, multiple = TRUE,
                            width = left_width - 10, height = 160)
p.line.import.mb <- gbutton("Import selected", cont = p.line.import.mf,
  handler = function(h, ...) {
    i <- svalue(p.line.import.mt, index = TRUE)
    ws$m <<- append(ws$m, ws.import$m[i])
    update_m.df()
    m.gtable[,] <- m.df
  }
)


# center: Dataset editor {{{1
ds.editor <- gframe("", horizontal = FALSE, cont = center, 
                     label = "Dataset editor")
m.editor  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Model editor")
f.config  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Fit configuration")
r.viewer  <- gframe("", horizontal = FALSE, cont = center, 
                    label = "Result viewer")
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


# vim: set foldmethod=marker ts=2 sw=2 expandtab: {{{1
