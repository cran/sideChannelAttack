% $Id: filter.mRMR.Rd 24 2009-09-24 12:36:55Z edd $
\name{filter.mRMR}
\alias{filter.mRMR}
\alias{filter.mRMR.default}
\alias{predict.filter.mRMR}
\title{filter.mRMR}
\description{
  The \code{filter.mRMR} function applies the feature selection mRMR to a set of physical measures.
}
\usage{
filter.mRMR(X,Y,nbreVarX_,...)
}
\arguments{
  \item{X}{A matrix where each row is a physical measure.}
  \item{Y}{A binary vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{y}{y} is the key for the \eqn{i^{th}}{i_th} physical measure in the matrix \eqn{x}{x}.
 }
 \item{nbreVarX_}{Number of component to get after the reduction by the mRMR of a physical measure.}
\item{...}{Currently ignored.}
}
\value{
  The \code{filter.mRMR} function returns an object which can use with the \code{predict} function to reduce each physical measure. This physical measure can be the same or an other one than contained in \eqn{X}{X}.

The value of this function is an object of class \code{filter.mRMR}, which is a list with the following components:

	\item{filter}{sorted list of the best variables returned by the mRMR algorithm.}
  }
\details{
  The \code{filter.mRMR} function is the feature selection mRMR. It returns an object which can be used with the \code{predict} function to convert a set of physical measures to another one with less variables. 
}
\references{
H. Peng & F. Long & C. Ding, (2005), "Feature Selection based on Mutual Infor- mation : Criteria of Max-Dependency, Max-Relevance, and Min-Redundancy", IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol 27, No 8, pp 1226-1238.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=filter.mRMR(X=traces[-1,],Y=key[-1],nbreVarX_=2)

#model prediction
predict(attack,t(traces[1,]))
}
