\name{Re_tfpwmk}
\alias{Re_tfpwmk}
\title{
Locally change-point detection using the tfpwmk Approach
}
\description{
Locally change-point detection using the tfpwmk Approach.
}
\usage{
Re_tfpwmk(x,m=1,k=2,blow=0.1,bup=(1-blow),leave=FALSE, adjust= FALSE,history=FALSE,...)
}
\arguments{
\item{x}{
a numeric vector
}
\item{m}{
number of times we sub-sample
}
\item{k}{
proportional to the number of points that we check in each side of target point, it can be either a signle number or a vector of numbers. see details
}
\item{blow}{
used to find the first time-period to be checked for change-point. the function does not look for changes at time-periods earlier than blow*n that n is the length of x
}
\item{bup}{
similar to blow. default is 1-blow
}
\item{leave}{
if TRUE the function uses the leave-one-out technique when looking for changes.
}
\item{adjust}{if TRUE p-value will be adjusted by methods in \link[stats]{p.adjust}
}
\item{history}{
if TRUE, it maintains the stepwise results when k is a vector
}
\item{...}{
arguments passed to \link[stats]{p.adjust}
}

}
\details{
This technique uses the tfpwmk trend detection method to locally look for potential change-points in the numerical vector x.

In the tails of x, since there is not enough data points before/after the target points, the methods uses sub-sampling, in order to moderate the effect of sub-sampling this process will be repeated in the tails some m times.

Assume the length of x is n. The argument k is used to set the number of points in the sides of target point when looking for change-points. n/k  is the number of points in each side we consider. For instance, if n=300 and k=10, this means we consider 30 points before and 30 after when locally detecting changes.

If leave=TRUE, the function removes the target point when checking for possiblity of change at the target point.

If adjust=TRUE, methods susch as "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
"fdr", "none" can be passed through function, e.g. method="BH".

If k is a vector of numbers, then the function returns an adaptive result based on the different given ks.

}


\references{
}
\author{
Mehdi Moradi \email{m2.moradi@yahoo.com}
}

\value{
cp: the most probable change point.

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
Z <- Re_tfpwmk(x,m=50,k=10)
plot(Z$s,attr(Z,"zs"),type="l",ylab = "Z",xlab = "time")
plot(Z$s,attr(Z,"mags"),type="l",ylab = "Z",xlab = "time")
}
