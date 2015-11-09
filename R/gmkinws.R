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

#' A workspace class for gmkin
#'
#' Datasets, models and fits are stored in lists.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom plyr compact
#' @export
#' @format An \code{\link{R6Class}} generator object.
#' @field observed Names of the observed variables in the datasets, named
#'   by the names used in the models contained in field m
#' @field ds A list of datasets compatible with mkinfit (long format)
#' @field m A list of mkinmod models
#' @field f A list of mkinfit objects

gmkinws <- R6Class("gmkinws", 
  public = list(
    observed = NULL,
    ds = list(),
    m = list(),
    ftmp = list(Name = ""),
    f = list(),

    initialize = function(ds, m, f) {

      ## Datasets
      if (!missing(ds)) {
        self$check_ds(ds)
        self$ds = plyr::compact(ds)

        self$update_observed()
      }

      ## Models
      if (!missing(m)) {
        self$check_m(m)
        self$m <- plyr::compact(m)
      }

      ## Fits
      if (!missing(f)) {
        self$f <- plyr::compact(f)
      }

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
      if (is.na(self$ds[1])) self$ds <- plyr::compact(ds)
      else self$ds <- append(self$ds, plyr::compact(ds))

      self$update_observed()

      invisible(self)
    },

    update_observed = function() {
      observed_ds <- if (is.na(self$ds[1])) NULL
        else na.omit(unique(unlist(sapply(self$ds, function(x) x$observed))))

      observed_m <- if (is.na(self$m[1])) NULL
        else na.omit(unique(unlist(sapply(self$m, function(x) names(x$spec)))))

      self$observed = union(observed_ds, observed_m)
    },

    delete_ds = function(i) {
      if (any(sapply(self$ds[i], is.null))) 
        stop("Could not delete dataset(s) ", paste(i, collapse = ", "))

      self$ds <- self$ds[-i] 
      if (length(self$ds) == 0) self$ds <- NA
      self$update_observed()
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
      if (is.na(self$m[1])) self$m <- plyr::compact(m)
      else self$m = append(self$m, plyr::compact(m))
      
      self$update_observed()

      invisible(self)
    },

    delete_m = function(i) {
      if (any(sapply(self$m[i], is.null))) 
        stop("Could not delete model(s) ", paste(i, collapse = ", "))

      self$m <- self$m[-i] 
      if (length(self$m) == 0) self$m <- NA
      invisible(self)
    },

    add_f = function(f) {
      if (is.na(self$f[1])) self$f <- plyr::compact(f)
      else self$f = append(self$f, plyr::compact(f))
      
      invisible(self)
    },

    delete_f = function(i) {
      if (any(sapply(self$f[i], is.null))) 
        stop("Could not delete fit(s) ", paste(i, collapse = ", "))

      self$f <- self$f[-i] 
      if (length(self$f) == 0) self$f <- NA
      invisible(self)
    },

    clear_compiled = function() {
      for (i in seq_along(self$m)) {
        self$m[[i]]$cf <- NULL
      }
      self$ftmp$mkinmod$cf <- NULL
      for (i in seq_along(self$f)) {
        self$f[[i]]$mkinmod$cf <- NULL
      }
      invisible(self)
    }
  )   
)

#' @export
print.gmkinws <- function(x, ...) {
  cat("<gmkinws> workspace object\n")
  cat("Observed variables:\n")
  print(x$observed)
  cat("\nDatasets:\n")
  print(x$ds)
  cat("\nModels:\n")
  print(x$m)
  cat("\nNames of fits:\n")
  if (is.na(x$f[1])) print(NA)
  else print(sapply(x$f, function(x) x$name))
}
