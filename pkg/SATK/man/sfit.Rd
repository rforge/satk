\name{sfit}
\alias{sfit}
\title{Create a scale_fit object from a dataframe or matrix object}
\usage{
  sfit(formula, x, stype = NA, data.cols, start,
    eff.check = TRUE, ...)
}
\arguments{
  \item{formula}{a formula for to fit}

  \item{x}{a data.frame or matrix object containing data
  for model. A data.frame should the data in columns named
  \code{N},for "scaled objects" (e.g. users, cpus), and
  \code{X_N}, for measured throughput. The default for a
  matrix object is to put scaled objects in the matrix's
  column 1 (i.e. \code{x[,1]}) and throughput in column 2
  \code{x[,2]}}

  \item{data.cols}{a vector indicating which columns in x
  has scaled object number and throughput data.
  \code{data.cols[[1]]} is scaled objects, and
  \code{data.col[[2]]} is measured throughput. It can be a
  numeric vector containing column numbers or a character
  vector containing the column names.}

  \item{start}{vector with initial guess for parameters}

  \item{stype}{character string to set the sfit_type
  attribute to.}

  \item{eff.check}{toggle check for over-acheivers}

  \item{...}{additional arguments to pass to function}
}
\value{
  sfit object
}
\description{
  Extends the native \code{nls} object to specialize it for
  fitting scaling models Allows multiple models to be
  stored in one workspace.
}

