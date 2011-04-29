% $Id: filter.NULL.Rd 24 2009-09-24 12:36:55Z edd $
\name{filter.NULL}
\alias{filter.NULL}
\alias{filter.NULL.default}
\alias{predict.filter.NULL}
\title{filter.NULL}
\description{
  The \code{filter.NULL} function applies the feature selection NULL to a set of physical measures. In other words, it returns all this data.
}
\usage{
filter.NULL(...)
}
\arguments{
  \item{...}{Currently ignored.}
}
\value{
  The \code{filter.NULL} function returns an object which can be used with the \code{predict} function to reduce each physical measure. This data can be the same or an other one than contained in \eqn{X}{X}.

The value of this function is an object of class \code{filter.NULL}, which is a list without element.
  }
\details{
  The \code{filter.NULL} function is the feature selection NULL. It returns an object which can be used with the \code{predict} function to convert a set of physical measures to another one. But in this particular case, it returns all the data without reduction. 
}
\author{Liran Lerman \email{llerman@ulb.ac.be} & Gianluca Bontempi \email{gbonte@ulb.ac.be@ulb.ac.be} & Olivier Markowitch \email{olivier.markowitch@ulb.ac.be}}

\examples{

#data collection
data(powerC)
traces = powerC[,-301]
key = powerC[,301]

#model creation
attack=filter.NULL()

#model prediction
predict(attack,t(traces[1,]))
}
