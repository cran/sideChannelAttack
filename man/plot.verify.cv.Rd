% $Id: plot.verify.cv.Rd 24 2009-09-24 12:36:55Z edd $
\name{plot.verify.cv}
\alias{plot.verify.cv}
\title{plot.verify.cv}
\description{
  Plot the quality of an attack of a \code{verify.cv} object.
}
\usage{
\method{plot}{verify.cv}(x, ...)
}
\arguments{
  \item{x}{An objet of class 'verify.cv'.}
\item{...}{Currently ignored.}
}
\value{
  This function plots the quality of an attack of a \code{verify.cv} object.
  }
\details{
  The \code{verify.cv} function estimates the quality of the attack with a k-cross-validation approach and this function plots it.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{
#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]
newIndice = c(sample(1:128,15),sample(129:256,15))
traces = traces[newIndice,]
key = key[newIndice]

#model checking
attack=verify.cv(model=dpa1,filter=filter.mRMR,X=traces,Y=key,nbreVarX=c(14:15),k=3)
plot(attack)
}
