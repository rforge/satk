\name{plot.sflist}
\alias{plot.sflist}
\title{Draw a graph of all the objects plus measured data in sfit_list}
\usage{
  \method{plot}{sflist} (x, xlab = "N",
    ylab = "Throughput X(N)", ..., plot.lin_scale = F)
}
\arguments{
  \item{x}{a sfit_list}

  \item{xlab}{a label for the x-axis}

  \item{ylab}{a label for the y-axis}

  \item{plot.lin_scale}{add a line showing linear scaling}

  \item{...}{additional arguments to pass to each of the
  sfit objects}
}
\value{
  plot
}
\description{
  Draws a graph of all the objects plus measured data in x
  in the sfit_list plus the original measured data.
}
\examples{
library(lattice)
data(USLcalc)
sl1<-sflist(USLcalc)
plot(sl1)
}

