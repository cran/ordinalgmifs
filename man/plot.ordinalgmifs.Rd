\name{plot.ordinalgmifs}
\alias{plot.ordinalgmifs}
\title{
Plot solution path for ordinal GMIFS fitted model.
}
\description{
This function plots either the coefficient path, the AIC, or the log-likelihood for a fitted \code{ordinalgmifs} object.
}
\usage{
\method{plot}{ordinalgmifs}(x, type = "trace", ...)
}
\arguments{
  \item{x}{
an \code{ordinalgmifs} object.
}
  \item{type}{
default is \code{"trace"} which plots the coefficient path for the fitted object. Also available are \code{"AIC"}, \code{"BIC"}, and \code{"logLik"}.
}
  \item{\dots}{other arguments.}
}
\author{
Kellie J. Archer
}
\seealso{
See Also \code{\link{coef.ordinalgmifs}}, \code{\link{summary.ordinalgmifs}}, \code{\link{ordinalgmifs}}, \code{\link{predict.ordinalgmifs}}
}
\examples{
\donttest{
data(hccframe)
# To minimize processing time, MPO_E302_R is coerced into the model and only a subset of 
# two CpG sites (DDIT3_P1313_R and HDAC9_P137_R) are included as penalized covariates
# in this demonstration.
hcc.fit<-ordinal.gmifs(group~MPO_E302_R, x=c("DDIT3_P1313_R", "HDAC9_P137_R"), 
	data=hccframe)
plot(hcc.fit)
}
}
\keyword{ methods }