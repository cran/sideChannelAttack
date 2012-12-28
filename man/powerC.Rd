\name{powerC}
\docType{data}
\alias{powerC}
\title{Power Consumption Data}
\description{
  This is a set of traces from a cryptographic device Xilinx Spartan XC3s5000 running at a frequency around 33 MHz and collected by an oscilloscope Agile infiniium DSO80204B 2Ghz 40GSa/s. The cryptographic device implemented the bloc cipher `Triple Data Encryption Algorithm' and the traces have been reduced with the feature selection \code{mRMR} in order to obtain only 300 components from 20001 initially.
}
\usage{
data(powerC)
}
\format{
  \eqn{powerC}{powerC} is a matrix with 256 rows and 301 columns. From the first to the \eqn{300^{th}}{300_th} component of \eqn{powerC}{powerC}, each row is a trace. The secret to retrieve by these traces is a bit of the secret key used by the cryptographic device. The value of this bit is given by the \eqn{301^{th}}{301_th} component of \eqn{powerC}{powerC}.
}
\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]+1

#model creation
attack=dpa1(traces[-1,],factor(key[-1]))

#model prediction
predict(attack,traces[1,])
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}
