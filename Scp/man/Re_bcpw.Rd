\name{Re_bcpw}
\alias{Re_bcpw}
\title{
Locally change-point detection using the bcpw Approach
}
\description{
Locally change-point detection using the bcpw Approach.
}
\usage{
Re_bcpw(x,m=1,k=2,blow=0.1,bup=(1-blow),leave=FALSE, adjust= FALSE,history=FALSE,...)
}
\arguments{
\item{x}{
a numeric vector
}
\item{m}{
number of times to sub-sample
}
\item{k}{
proportional to the number of points that we check in each side of target point, it can be either a signle number or a vector of numbers. see details
}
\item{blow}{
used to find the first time-period to be checked for change-point. the function does not look for changes at time-periods earlier than blow*n, n being the length of x
}
\item{bup}{
similar to blow used for the right end of x. default is 1-blow
}
\item{leave}{
if TRUE the function uses the leave-one-out technique when looking for changes
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
This technique accommodates the \link[modifiedmk]{bcpw} trend detection method in the LACPD procedure of Moradi et al. (2020) to look for potential change-points in the numerical vector x.

In the tails of x, since there are not enough data points before/after the target points, the method uses sub-sampling, wherein it moderates the effect of sub-sampling by iteration.

Assume the length of x is n. The argument k is used to set the number of data in the sides of the target point when looking for change-points. n/k  is the number of points on each side we consider. For instance, if n=300 and k=10, this means we consider 30 observations before and 30 after when locally detecting changes.

Since changes usually do not happen at the very beginning/end of time-period, so it can be avoided to look for changes at the very beginning/end of data. Argument blow=0.1 means the procedure does not look for changes in the first/last 10 percent of data.

If leave=TRUE, the function removes the target point when checking for the possibility of change at the target point.

If adjust=TRUE, methods susch as "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
"fdr", "none" can be passed through function, e.g. method="BY".

If k is a vector of numbers, then the function returns a result based on adaptive sliding windows which is an average result obtained from different windows.
}



\author{
Mehdi Moradi \email{m2.moradi@yahoo.com}
}

\value{
cp: the most probable change-point

z: the z statistics

magnitude: the magnitude of change

p.value: the corresponding p.value

s: the time-periods that the function has looked for potential changes.

Attributes:

attr(,"zs"): retrieves the obtained z statistics at the moments when the function has looked for potential changes.

attr(,"ps"): retrieves the obtained p-values at the moments when the function has looked for potential changes.


attr(,"mags"): retrieves the magnitude of change at the moments when the function has looked for potential changes.
}

\seealso{
\link[modifiedmk]{bcpw}
}

\references{
Moradi, M., Montesino-SanMartin, M., Ugarte, M. D., and Militino, A. F. (2020). Locally adaptive change-point detection with applications to remote sensing and land use changes.
}

\examples{
x <- rnorm(100)
Z <- Re_bcpw(x,m=50,k=c(2:3))
plot(Z$s,attr(Z,"zs"),type="l",ylab = "Z",xlab = "time")
plot(Z$s,attr(Z,"mags"),type="l",ylab = "Z",xlab = "time")
}
