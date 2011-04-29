% $Id: filter.RegressionTreeFilter.Rd 24 2009-09-24 12:36:55Z edd $
\name{filter.RegressionTreeFilter}
\alias{filter.RegressionTreeFilter}
\alias{filter.RegressionTreeFilter.default}
\alias{predict.filter.RegressionTreeFilter}
\title{filter.RegressionTreeFilter}
\description{
  The \code{filter.RegressionTreeFilter} function implements the feature selection RegressionTreeFilter to a set of physical measures.
}
\usage{
filter.RegressionTreeFilter(X,nbreVarX_,...)
}
\arguments{
 \item{X}{A matrix where each row is a physical measure.}
 \item{nbreVarX_}{The number of variables which represents each physical measure after the reduction by the RegressionTreeFilter.}
  \item{...}{Currently ignored.}
}
\value{
  The \code{filter.RegressionTreeFilter} function returns an object which can be used with the \code{predict} function to reduce each physical measure. This side channel can be the same or an other one than contained in \eqn{X}{X}.

The value of this function is an object of class \code{filter.RegressionTreeFilter}, which is a list with the following components:

	\item{nbreVarX}{number of variable to get after the projection in the new basis.}
  }
\details{
  The \code{filter.RegressionTreeFilter} function is the feature selection RegressionTreeFilter. It returns an object which can be used with the \code{predict} function to convert a set of physical measures to another one with less variables. 
}
\references{
Pierre Geurts. 2001. Pattern Extraction for Time Series Classification. In Proceedings of the 5th European Conference on Principles of Data Mining and Knowledge Discovery (PKDD '01), Luc De Raedt and Arno Siebes (Eds.). Springer-Verlag, London, UK, 115-127.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=filter.RegressionTreeFilter(X=traces[-1,],nbreVarX_=2)

#model prediction
predict(attack,t(traces[1,]))
}
