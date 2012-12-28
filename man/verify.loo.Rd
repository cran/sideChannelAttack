% $Id: verify.loo.Rd 24 2009-09-24 12:36:55Z edd $
\name{verify.loo}
\alias{verify.loo}
\alias{verify.loo.default}
\title{verify.loo}
\description{
  The \code{verify.loo} function allows to estimate the quality of a model (an attack) with a leave-one-out approach.
}
\usage{
verify.loo(model, filter, X, Y, nbreVarX, param.model=list(), param.fs=list(), ...)
}
\arguments{
  \item{model}{A model such as randomForest, gaussian, svm, etc.}
  \item{filter}{A feature selection such as filter.PCA, filter.mRMR, etc.}
  \item{X}{A matrix where each row is a physical measure.}
  \item{Y}{A vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{y}{y} is the key for the \eqn{i^{th}}{i_th} physical measure in the matrix \eqn{x}{x}.}
  \item{nbreVarX}{The number of variables which represents each physical measure after the reduction by the feature selection.}
  \item{param.model}{A list of parameters to insert into the model.}
  \item{param.fs}{A list of parameters to insert into the feature selection algorithm.}
\item{...}{Currently ignored.}
}
\value{
  The \code{verify.loo} function returns an object which can be used with the \code{plot} function to plot the quality of the model.

	\item{TP}{number of true positive}
	\item{TN}{number of true negative}
	\item{FN}{number of false negative}
	\item{FP}{number of false positive}
	\item{dim}{the number of variables which represents each physical measure after the reduction by the feature selection.}
  }
\details{
  The \code{verify.loo} function estimates the quality of the attack with a leave-one-out approach. It is executed in \eqn{N}{N} rounds. Each round uses \eqn{N-1}{N-1} traces to learn a model and the remaining trace to assess the generalization accuracy of the model. This is repeated until every trace has been used for testing purposes. The best model is the one that maximizes the value returned by leave-one-out.

The advantages of this estimation is its accuracy but the estimation process is expensive in a computational point of view.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}} 

\examples{
#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]
newIndice = c(sample(1:128,15),sample(129:256,15))
traces = traces[newIndice,]
key = key[newIndice]+1

#model checking
attack=verify.loo(model=dpa1,filter=filter.mRMR,X=traces,Y=key,nbreVarX=c(5))
plot(attack)
}
