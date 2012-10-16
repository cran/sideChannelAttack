% $Id: verify.ho.Rd 24 2009-09-24 12:36:55Z edd $
\name{verify.ho}
\alias{verify.ho}
\alias{verify.ho.default}
\title{verify.ho}
\description{
  The \code{verify.ho} function allows to estimate the quality of a model (an attack) with a hold-out approach.
}
\usage{
verify.ho(model, filter, Xlearn, Ylearn, Xval, Yval, nbreVarX,...)
}
\arguments{
  \item{model}{A model such as randomForest, gaussian, svm, etc.}
  \item{filter}{A feature selection such as filter.PCA, filter.mRMR, etc.}
  \item{Xlearn}{A matrix where each row is a physical measure for the training set.}
  \item{Ylearn}{A vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{Ylearn}{Ylearn} is the key for the \eqn{i^{th}}{i_th} physical measure in the matrix \eqn{Xlearn}{Xlearn}.}
  \item{Xval}{A matrix where each row is a physical measure for the validation set.}
  \item{Yval}{A vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{Yval}{Yval} is the key for the \eqn{i^{th}}{i_th} physical measure in the matrix \eqn{Xval}{Xval}.}
 \item{nbreVarX}{The number of variables which represents each physical measure after the reduction by the feature selection.}
\item{...}{Currently ignored.}
}
\value{
  The \code{verify.ho} function returns an object which can be used with the \code{plot} function to plot the quality of the model.

The value of this function is an object of class \code{verify.ho}, which is a list with the following components:

	\item{TP}{number of true positive}
	\item{TN}{number of true negative}
	\item{FN}{number of false negative}
	\item{FP}{number of false positive}
	\item{dim}{the number of variables which represents each physical measure after the reduction by the feature selection.}
  }
\details{
  The \code{verify.ho} function estimates the quality of the attack with a hold-out approach. It cuts randomly the set of observations in two subsets. The first one is retained as the training and the second is used for the validation phase where the quality of the model is estimatedby counting the number of time where model predicts correctly the key.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{
#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]
newIndice = sample(1:256)
traces = traces[newIndice,]
key = key[newIndice]

#model checking
attack=verify.ho(model=gaussian,filter=filter.mRMR,Xlearn=traces[1:128,],Ylearn=key[1:128],Xval=traces[128:256,],Yval=key[128:256],nbreVarX=c(1:15))
plot(attack)
}
