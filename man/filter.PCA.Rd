% $Id: filter.PCA.Rd 24 2009-09-24 12:36:55Z edd $
\name{filter.PCA}
\alias{filter.PCA}
\alias{filter.PCA.default}
\alias{predict.filter.PCA}
\title{filter.PCA}
\description{
  The \code{filter.PCA} function applies the feature selection Principal Component Analysis (PCA) to a set of physical measures.
}
\usage{
filter.PCA(X,nbreVarX_,...)
}
\arguments{
  \item{X}{A matrix where each row is a physical measures.}
  \item{nbreVarX_}{The number of variables which represents each physical measures after the reduction by the PCA.}
  \item{...}{Currently ignored.}
}
\value{
  The \code{filter.PCA} function returns an object which can be used with the \code{predict} function to reduce each physical measure. This physical measure can be the same or another one than contained in \eqn{X}{X}.

The value of this function is an object of class \code{filter.PCA}, which is a list with the following components:

	\item{mod}{a model of PCA.}
	\item{nbreVarX}{number of component to get after the projection by the PCA of a physical measure.}
  }
\details{
  The \code{filter.PCA} function is the feature selection PCA. It converts a set of physical measures to another one with less components. 
}
\references{
K. Pearson, (1901), "On Lines and Planes of Closest Fit to Systems of Points in Space", Philosophical Magazine 2 (6), pp. 559-572.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=filter.PCA(X=traces[-1,],nbreVarX_=2)

#model prediction
predict(attack,t(traces[1,]))
}
