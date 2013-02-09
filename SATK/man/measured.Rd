\name{measured}
\alias{measured}
\alias{measured.sfit}
\alias{measured.sflist}
\title{Dump measured data}
\usage{
  measured(object, ...)

  \method{measured}{sfit} (object, drop.norm = TRUE, ...)

  \method{measured}{sflist} (object, drop.norm = TRUE, ...)
}
\arguments{
  \item{object}{a sfit object}

  \item{drop.norm}{logical indicating whether to print Norm
  column}

  \item{...}{additional arguments}
}
\value{
  data.frame
}
\description{
  Dump the scale_fit model data from nls environment
}
\examples{
data(USLcalc)
sl1<-sflist(USLcalc)
measured(sl1)
}

