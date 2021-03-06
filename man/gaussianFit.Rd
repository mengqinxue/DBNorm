% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNorm_Fitting.R
\name{gaussianFit}
\alias{gaussianFit}
\title{fitting a distribution by gaussian curve fitting}
\usage{
gaussianFit(DBdata)
}
\arguments{
\item{DBdata}{input distribution dataset}
}
\value{
a gaussian curve fitting function
}
\description{
fitting a distribution by gaussian curve fitting
}
\details{
The function fits distributions by gaussian curve fitting and
returns a gaussian curve fitting function.
}
\examples{
# Calculating the gaussian curve fitting function of DArray1's distribution
DBdata1 = gaussianFit(DBdata1)

}
\author{
Qinxue Meng, Paul Kennedy
}
\seealso{
\code{optim}
}

