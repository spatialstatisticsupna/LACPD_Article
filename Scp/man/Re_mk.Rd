\name{Re_mk}
\alias{Re_mk}
\title{
Locally change-point detection using Mann-Kendall
}
\description{
Locally change-point detection using Mann-Kendall.
}
\usage{
Re_mk(x,m=1,k=2,blow=0.1,bup=(1-blow),leave=FALSE,adjust=FALSE,history=FALSE,...)
}
\arguments{
\item{x}{
a numeric vector
}
\item{m}{
number of times to sub-sample
}
\item{k}{
single number or numeric vector indicating the number of points in each side of target point. See details
}
\item{blow}{
fraction of observations (0-1) at the beginning of the time-seres not cosidered for change detection
}
\item{bup}{
similar to \code{blow}, but for the end of the time series. Default is 1-\code{blow}
}
\item{leave}{
if \code{TRUE}, the function uses the leave-one-out technique when looking for changes.
}
\item{adjust}{
if \code{TRUE}, p-value will be adjusted by methods in \link[stats]{p.adjust}
}
\item{history}{
if \code{TRUE}, it maintains the stepwise results when \code{k} is a vector
}
\item{...}{
arguments passed to \link[stats]{p.adjust}
}
}
\details{
This technique uses the Mann-Kendall trend detection method to locally look for potential change-points in the numerical vector \code{x}.

In the tails of \code{x}, since there is not enough data points before/after the target points, the methods uses sub-sampling. To moderate the effect of sub-sampling, the sub-sampling process is repeated \code{m} times.

Assume the length of \code{x} is n. The argument \code{k} is used to set the number of points in the sides of target point when looking for change-points. n/\code{k}  is the number of points in each side of the target point. For instance, if n=300 and \code{k=10}, this means we consider 30 points before and after when locally detecting changes.

If \code{leave=TRUE}, the function removes the target point when checking for possible changes at the target point.

If \code{adjust=TRUE}, methods susch as "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
"fdr", "none" can be passed through function, e.g. method="BH".

If \code{k} is a vector of numbers, then the function returns an adaptive result based on the different \code{k}s.

}
\references{
}
\author{
Mehdi Moradi \email{m2.moradi@yahoo.com}
}
\value{
cp: the index of the most probable change point in the time series.

z: the z statistics of Mann-Kendall trend test at cp.

magnitude: the magnitude of change at cp.

p.value: the corresponding p.value of z.

s: the time-periods that the function has looked for potential changes.

Attributes:

attr(,"zs"): retrieves the obtained z statistics at the time-periods the function has looked for potential changes.

attr(,"ps"): retrieves the obtained p-values at the time-periods the function has looked for potential changes.

attr(,"mags"): retrieves the magnitude of change at the time-periods the function has looked for potential changes.
}
\seealso{
}
\examples{
x <- rnorm(300)
Z <- Re_mk(x,m=50,k=10)
plot(Z$s,attr(Z,"zs"),type="l",ylab = "Z",xlab = "time")
plot(Z$s,attr(Z,"mags"),type="l",ylab = "Z",xlab = "time")
}
