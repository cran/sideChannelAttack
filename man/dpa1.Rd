% $Id: dpa1.Rd 24 2009-09-24 12:36:55Z edd $
\name{dpa1}
\alias{dpa1}
\alias{predict.dpa1}
\title{DPA}
\description{
  The \code{dpa1} function applies a Differential Power Analysis (DPA) to a set of traces in order to find the key used by the cryptographic device.
}
\usage{
dpa1(x, y,...)
}
\arguments{
  \item{x}{A matrix where each row is a trace.}
  \item{y}{A binary vector where the \eqn{i^{th}}{i_th} element of the vector \eqn{y}{y} is the key for the \eqn{i^{th}}{i_th} trace in the matrix \eqn{x}{x}.}
  \item{...}{Currently ignored.}
}
\value{
  The \code{dpa1} function returns an object which knowing a trace can be used with the \code{predict} function that estimates the value of the key.
The value of this function is an object of class \code{dpa1}, which is a list with the following components:

	\item{mean}{two arithmetics means, one for each possible key}
  }
\details{
  The \code{dpa1} function is an example of DPA. It calculates the average \eqn{X_{i}}{X_i} of traces for each key \eqn{Y_{i}}{Y_i}.
  Then, to estimate the key from a trace \eqn{T}, it returns the key \eqn{Y_{i}}{Y_i} which maximizes the equation \eqn{\arg\max_{Y_{i}}(cor(T,X_{i})) }{argmax{Y_i}(cor(T,X_i))}
}
\references{
P. C. Kocher & J. Jaffe & B. Jun, (1999), "Differential Power Analysis: Leaking Secrets", In Proc. Crypto '99, Springer-Verlag, LNCS 1666, pages 388-397.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{
#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=dpa1(traces[-1,],factor(key[-1]))

#model prediction
predict(attack, traces[1,])
}
