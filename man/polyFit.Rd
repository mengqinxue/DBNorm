% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNorm_Fitting.R
\name{polyFit}
\alias{polyFit}
\title{fitting a distribution by polynomial curve fitting}
\usage{
polyFit(DBdata, n)
}
\arguments{
\item{DBdata}{input distribution dataset}

\item{n}{the degree of polynomial functions}
}
\value{
a polynomial curve fitting function
}
\description{
fitting a distribution by polynomial curve fitting
}
\details{
The function fits distributions by polynomial curve fitting and
returns a polynomial curve fitting function.
}
\examples{
# Calculating the polynomial curve fitting function of DArray1's distribution
DBdata1 = polyFit(DBdata1, 3)

}
\author{
Qinxue Meng, Paul Kennedy
}
\seealso{
\code{lm}
}

