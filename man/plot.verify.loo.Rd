% $Id: plot.verify.loo.Rd 24 2009-09-24 12:36:55Z edd $
\name{plot.verify.loo}
\alias{plot.verify.loo}
\title{plot.verify.loo}
\description{
  Plot the quality of an attack of a \code{verify.loo} object.
}
\usage{
\method{plot}{verify.loo}(x, ...)
}
\arguments{
  \item{x}{An objet of class 'verify.loo'.}
\item{...}{Currently ignored.}
}
\value{
  This function plots the quality of an attack of a \code{verify.loo} object.
  }
\details{
  The \code{verify.loo} function estimates the quality of the attack with a hold-out approach and this function plots it.
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
attack=verify.loo(model=dpa1,filter=filter.mRMR,X=traces,Y=key,nbreVarX=c(14:15))
plot(attack)
}
