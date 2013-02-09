\name{build_fits_frame}
\alias{build_fits_frame}
\title{Create data frame from sflit_list object}
\usage{
  build_fits_frame(object, keep.measured = TRUE)
}
\arguments{
  \item{object}{a sfit_list}

  \item{keep.measured}{a logical indicate to include
  measured}
}
\value{
  data.frame
}
\description{
  builds a data frame containing fitted data from each of
  the sfit objects in the sfit_list plus the original
  measured data.
}
\examples{
data(USLcalc)
sl1<-sflist(USLcalc)
build_fits_frame(sl1)
}

