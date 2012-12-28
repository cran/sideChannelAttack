% $Id: gaussian.Rd 24 2009-09-24 12:36:55Z edd $
\name{gaussian}
\alias{gaussian}
\alias{gaussian. default}
\alias{predict.gaussian}
\title{Template Attack}
\description{
  The \code{gaussian} function applies a template attack (TA) to a set of traces in order to find the key used by the cryptographic device.
}
\usage{
gaussian(x, y,...)
}
\arguments{
  \item{x}{A matrix where each row is a trace.}
  \item{y}{A vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{y}{y} is the key for the \eqn{i^{th}}{i_th} trace in the matrix \eqn{x}{x}.}
  \item{...}{Currently ignored.}
}
\value{
  The \code{gaussian} function returns an object which can be used with the \code{predict} function to estimate the value of the key knowing a trace.

The value of this function is an object of class \code{gaussian}, which is a list with the following components:

	\item{mean}{a list of arithmetic means, one for each possible key.}
	\item{cov}{a list of covariance matrices, one for each possible key.}
	\item{detCov}{a list of determinants of each covariance matrice.}
  }
\details{
  The \code{gaussian} function is an example of TA. It estimates the conditional probability of the trace for each key and then returns the key which maximizes this probability. It extracts all possible informations available in each trace and is hence the strongest form of side channel attack possible in an information theoretic sense that relies on a parametric Gaussian estimation approach. 
}
\references{
S. Chari & J. R. Rao & P. Rohatgi, (2002), "Template Attacks" , in CHES, volume 2523 of LNCS, pages 13-28. Springer.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]+1

#feature selection
filter = filter.mRMR(traces,key,14)
traces = predict(filter,traces)

#model creation
attack=gaussian(traces[-1,],factor(key[-1]))

#model prediction
predict(attack,traces[1,])
}
