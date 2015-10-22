# Copyright (C) 2015 Johannes Ranke
# Contact: jranke@uni-bremen.de

# This file is part of the R package gmkin

# mkin is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>

#' A workspace class for gmkin
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom mkin mkinws
#' @export
#' @format An \code{\link{R6Class}} generator object.
#' @field observed Names of the observed variables in the datasets, named
#'   by the names used in the models contained in field m
#' @field ds A list of datasets compatible with mkinfit (long format)
#' @field ds.cur Index of the currently selected dataset
#' @field m A list of mkinmod models
#' @field m.cur Index of the currently selected model
#' @field f A list of mkinfit objects
#' @field f.cur Index of the currently selected fit
#' @field s The summaries of the mkinfit objects in field f

gmkinws <- R6Class("gmkinws", 
  public = list(
    observed = NULL,
    ds = list(),
    ds.cur = NULL,
    m = list(),
    m.cur = NULL,
    f = list(),
    f.cur = NULL,
    s = NA,

    initialize = function(ds, m, f, ds.cur = NA, m.cur = NA, f.cur = NA) {

      ## Datasets
      if (!missing(ds)) {
        self$check_ds(ds)
        self$ds = ds
        self$ds.cur = ds.cur

        # Collect names of observed variables
        self$observed <- unique(sapply(ds, function(x) x$observed))
      }

      ## Models
      if (!missing(m)) {
        self$check_m(m)
        self$m <- m
      }
      self$m.cur = m.cur

      ## Fits
      if (!missing(f)) {
        self$f <- f
      }
      self$f.cur = f.cur

      invisible(self)
    },

    check_ds = function(ds) {
      errmsg <- "ds must be a list of mkinds objects"
      if (!is.list(ds)) stop(errmsg)
      lapply(ds, function(x) {
        if (!is(x, "mkinds"))
          stop(errmsg)
        }
      )
    },

    add_ds = function(ds) {
      self$check_ds(ds)
      common_names = intersect(names(self$ds), names(ds))
      if (length(common_names) > 0) stop("Dataset name(s) ", paste(common_names, collapse = ", "), " already used.")
      else append(self$ds, ds)

      # Update names of observed variables
      observed <- unique(sapply(ds, function(x) x$observed))
      self$observed <- union(self$observed, observed)

      invisible(self)
    },

    check_m = function(m) {
      errmsg <- "m must be a list of mkinmod objects"
      if (!is.list(m)) stop(errmsg)
      lapply(m, function(x) {
        if (!is(x, "mkinmod"))
          stop(errmsg)
        }
      )
    },

    add_m = function(m) {
      self$check_m(m)
      common_names = intersect(names(self$m), names(m))
      if (length(common_names) > 0) stop("Model name(s) ", paste(common_names, collapse = ", "), " already used.")
      else self$m = c(self$m, m)
      invisible(self)
    }
  )   
)

#' @export
print.gmkinws <- function(x, ...) {
  cat("<gmkinws> workspace object\n")
  cat("\nDatasets:\n")
  print(x$ds)
  cat("\nModels:\n")
  print(x$m)
  cat("Current selections:\n")
  cat("Dataset ", x$ds.cur, ", Model ", x$m.cur, ", Fit ", x$f.cur, "\n", sep = "")
  cat("\nFits:\n")
  print(names(x$f))
}
