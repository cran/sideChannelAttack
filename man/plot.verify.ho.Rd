% $Id: plot.verify.ho.Rd 24 2009-09-24 12:36:55Z edd $
\name{plot.verify.ho}
\alias{plot.verify.ho}
\title{plot.verify.ho}
\description{
  Plot the quality of an attack of a \code{verify.ho} object.
}
\usage{
\method{plot}{verify.ho}(x, ...)
}
\arguments{
  \item{x}{An objet of class 'verify.ho'.}
\item{...}{Currently ignored.}
}
\value{
  This function plots the quality of an attack of a \code{verify.ho} object.
  }
\details{
  The \code{verify.ho} function estimates the quality of the attack with a hold-out approach and this function plots it.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{
#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]
newIndice = sample(1:256)
traces = traces[newIndice,]
key = key[newIndice]+1

#model checking 
attack=verify.ho(model=gaussian,filter=filter.mRMR,Xlearn=traces[1:128,],Ylearn=key[1:128],Xval=traces[129:256,],Yval=key[129:256],nbreVarX=c(1:15))
plot(attack)
}
