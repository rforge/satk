\name{plot.sfit}
\alias{plot.sfit}
\title{Plot sfit object}
\usage{
  \method{plot}{sfit} (x, lty = "solid", type = "l",
    lwd = 1, ylab = "Throughput X(N)",
    xlab = "Virtual users", plot.legend = TRUE,
    legend.params = NULL, plot.measured = TRUE,
    points.params = NULL, plot.title = TRUE,
    title.params = NULL, plot.lin_scale = FALSE,
    lin_scale.params = NULL, ...)
}
\arguments{
  \item{x}{an sfit object}

  \item{lty}{line type see plot for all options}

  \item{type}{what type of plot should be drawn (see plot)}

  \item{lwd}{the line width}

  \item{xlab}{a label for the x axis, defaults to a
  description of x.}

  \item{ylab}{a label for the x axis, defaults to a
  description of y.}

  \item{plot.legend}{toggle drawing the legend (default
  TRUE)}

  \item{legend.params}{supply your own parameters to the
  legend function}

  \item{plot.measured}{toggle adding measured points to the
  graph (default TRUE)}

  \item{points.params}{supply your own parameters to the
  points function}

  \item{plot.lin_scale}{toggle adding linear scaling line
  (default TRUE)}

  \item{lin_scale.params}{supply your own parameters to the
  abline function that draws the linear scale line}

  \item{plot.title}{toggle plot the stand title (default
  TRUE)}

  \item{title.params}{supply your own parameters to the
  title function}

  \item{...}{additional arguments to pass to function}
}
\description{
  Plot sfit object
}

