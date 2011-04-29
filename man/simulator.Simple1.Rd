% $Id: simulator.Simple1.Rd 24 2009-09-24 12:36:55Z edd $
\name{simulator.Simple1}
\alias{simulator.Simple1}
\title{simulator.Simple1}
\description{
  The \code{simulator.Simple1} function simulates an unprotected cryptographic device implementing a bloc cipher.
}
\usage{
simulator.Simple1(message, key, noise=0)
}
\arguments{
  \item{message}{A matrix where each row is a binary plaintext of 6 components. Each component has the value 1 or 0.}
  \item{key}{A vector representing the binary secret key of 6 components. Each component has 1 or 0.}
  \item{noise}{A positive integer to represent noise on traces returned by this function. Higher is its value, more there are noise on data.}
}
\value{
  The \code{simulator.Simple1} function returns a trace which represents the power consumption, in volts, during an encryption by a cryptographic device.
  }
\details{
The \code{simulator.Simple1} function allows to simulate a cryptographic device implementing a bloc cipher. Its goal is to facilitate and to accelerate the collect of data and to compare attacks with the same dataset.
With a key and messages, it returns a trace which is the power consumption, in volts, during an encryption by the cryptographic device.

The algorithm, implemented in the \code{simulator.Simple1} function, is detailed below.
First, it applies the \code{xor} function between a \eqn{message_{i}}{message_i} and the \eqn{key}{key}. The result is inserted in a S-Box which is a nonlinear function that takes 6 components and returns 4 components. 

Then it calculates the hamming distance between the result and the previous result of the S-Box or the value null when encrypting the first message.

The result of the hamming distance is a point in the trace.
}
\references{
E. Peeters & F-X Standaert & N Donckers & J-J Quisquater, (2005), "Improved Higher-Order Side-Channel Attacks with FPGA Experiments", CHES, pp. 309-323.
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{
n=100
clee = c(round(runif(6)))
inp=matrix(round(runif(6*n)),ncol=6)

#creating a trace without noise through the simulator
traces=simulator.Simple1(inp,clee)
}
