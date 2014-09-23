\name{ordinalgmifs-package}
\alias{ordinalgmifs-package}
\alias{ordinalgmifs}
\docType{package}
\title{
Ordinal Response Regression for High-Dimensional Data
}
\description{
This package provides a function, ordinal.gmifs, for fitting cumulative link, adjacent category, forward and backward continuation ratio, and stereotype ordinal response models when the number of parameters exceeds the sample size, using the the generalized monotone incremental forward stagewise method.
}
\details{
\tabular{ll}{
Package: \tab ordinalgmifs\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-07\cr
License: \tab What license is it under?\cr
}
This package contains generic methods (coef, plot, predict, print, summary) that can be invoked for an object fitted using ordinal.gmifs. 
}
\author{
Kellie J. Archer, Jiayi Hou, Qing Zhou, Kyle Ferber, John G. Layne, Amanda Gentry

Maintainer: Kellie J. Archer <kjarcher@vcu.edu>
}
\references{
Hastie T., Taylor J., Tibshirani R., and Walther G. (2007) Forward stagewise regression and the monotone lasso. \emph{Electronic Journal of Statistics}, 1, 1-29.
}
\keyword{ package }
\keyword{ classif }
\seealso{
For models where no predictor is penalized see \code{\link[VGAM:VGAM]{vglm}} 
}
\examples{
\donttest{
data(hccframe)
# To minimize processing time, MPO_E302_R is coerced into the model and only a subset of 
# two CpG sites (DDIT3_P1313_R and HDAC9_P137_R) are included as penalized covariates
# in this demonstration.
hcc.fit<-ordinal.gmifs(group~MPO_E302_R, x=c("DDIT3_P1313_R", "HDAC9_P137_R"), 
	data=hccframe)
coef(hcc.fit)
summary(hcc.fit)
phat<-predict(hcc.fit)
table(phat$class, hccframe$group)
}
}
