% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sm-algorithm.R
\name{r2b}
\alias{r2b}
\title{computing a norm value for the segment from point k1 to k2-1}
\usage{
r2b(k1, k2, x, y)
}
\arguments{
\item{k1}{[INTEGER] start point}

\item{k2}{[INTEGER] end point+1}

\item{x}{[REAL(?)] input x-axis array (predictor)}

\item{y}{[REAL(?)] input y-axis array (response)}
}
\value{
A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
}
\description{
A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
}
\examples{
ni <- c( 1, 201, 402 )
i <- 1
k1 <- ni[i]
k2 <- ni[i+1]


r2b(k1, k2, y=t11$temper, x=t11$depth)
}
\keyword{internal}
