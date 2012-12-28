% $Id: filter.MAX.Rd 24 2009-09-24 12:36:55Z edd $
\name{filter.MAX}
\alias{filter.MAX}
\alias{filter.MAX.default}
\alias{predict.filter.MAX}
\title{filter.MAX}
\description{
  The \code{filter.MAX} function returns the maximum values of a physical measure.
}
\usage{
filter.MAX(nbreVarX_,...)
}
\arguments{
  \item{nbreVarX_}{The number of variables which represents each physical measures after the reduction.}
  \item{...}{Currently ignored.}
}
\value{
  The \code{filter.MAX} function returns an object which can be used with the \code{predict} function to reduce each physical measure. This physical measure can be the same or another one than contained in \eqn{X}{X}.

The value of this function is an object of class \code{filter.MAX}, which is a list with the following components:

	\item{nbreVarX}{number of component to get after the reduction of a physical measure.}
  }
\details{
  The \code{filter.MAX} function is the feature selection MAX. It converts a set of physical measures to another one with less components. 
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=filter.MAX(nbreVarX_=2)

#model prediction
predict(attack,t(traces[1,]))
}
