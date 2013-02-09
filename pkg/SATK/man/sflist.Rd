\name{sflist}
\alias{sflist}
\alias{sflist.data.frame}
\alias{sflist.sfit}
\title{sflist: sflist_list constructor}
\usage{
  sflist(x, ...)

  \method{sflist}{data.frame} (x, fits = .fits(), ...,
    starts, algorithms, fits.params, fit.names = NULL)

  \method{sflist}{sfit} (..., fit.names = NULL)
}
\arguments{
  \item{x}{a data.frame or matrix object containing data
  for model. A data.frame should the data in columns named
  \code{N},for "scaled objects" (e.g. users, cpus), and
  \code{X_N}, for measured throughput. The default for a
  matrix object is to put scaled objects in the matrix's
  column 1 (i.e. \code{x[,1]}) and throughput in column 2
  \code{x[,2]}}

  \item{fits}{a list that specifics which sfit objects to
  include in the list}

  \item{...}{additional arguments to pass to each of the
  sfit objects}

  \item{starts}{a named list of start parameter for the nls
  in the sfit object}

  \item{algorithms}{a named list of alogirthms to use in
  the nls in the sfit object}

  \item{fits.params}{a named list of named lists of fit
  parameters}

  \item{fit.names}{names to give the fit list}
}
\value{
  sflist object
}
\description{
  sflist_list constructors

  Extends the R \code{list} object to specializeit to
  groups of sfit objects that have all be fitted from the
  same data source
}
\details{
  Details of sflist
}
\examples{
data(USLcalc)
sl1<-sflist(USLcalc)

data(SSUSLcalc)
sl2<-sflist(SSUSLcalc,data.cols=c("V1","V2"),starts=c(usl=c(alpha=.1,beta=.001)))
}

