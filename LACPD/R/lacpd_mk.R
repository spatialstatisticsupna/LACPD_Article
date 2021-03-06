#' Locally change-point detection using the Mann-Kendall test
#'
#' Locally change-point detection after accommodating the Mann-Kendall test in LACPD procedure
#'
#'
#'
#' @param x a numeric vector
#' @param m number of times to sub-sample
#' @param k single number or numeric vector proportional to the number of points on each side of the target point. See details
#' @param blow fraction of observations (0-1) at the beginning of the time-series not considered for change detection
#' @param bup similar to \code{blow}, but for the end of the time series. Default is 1-\code{blow}
#' @param leave if \code{TRUE}, the function uses the leave-one-out technique when looking for changes
#' @param adjust if \code{TRUE}, p-value will be adjusted by methods in \link[stats]{p.adjust}
#' @param history if \code{TRUE}, it maintains the stepwise results when \code{k} is a vector
#' @param ... arguments passed to \link[stats]{p.adjust}
#'
#'
#' @details
#' This technique accommodates the \link[trend]{mk.test} trend detection method in the LACPD procedure of Moradi et al. (2020) to look for potential change-points in the numerical vector \code{x}.
#'
#' In the tails of \code{x}, since there are not enough data points before/after the target points, the method uses sub-sampling, wherein it moderates the effect of sub-sampling by iteration.
#'
#' Assume the length of \code{x} is n. The argument \code{k} is used to set the number of data in the sides of the target point when looking for change-points. n/\code{k}  is the number of points on each side we consider. For instance, if n=300 and k=10, this means we consider 30 observations before and 30 after when locally detecting changes.
#'
#' If \code{leave=TRUE}, the function removes the target point when checking for possible changes at the target point.
#'
#' If \code{adjust=TRUE}, methods susch as "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" can be passed through function, e.g. method="BY".
#'
#' If \code{k} is a vector of numbers, then the function returns a result based on adaptive sliding windows which is an average result obtained from different windows.
#'
#'
#'
#' @return
#' cp: the index of the most probable change point in the time series
#'
#' z: the z statistics of LACPD
#'
#' magnitude: the magnitude of change
#'
#' p.value: the corresponding p.value
#'
#' s: the time-periods that the function has looked for potential changes
#'
#'
#' Attributes:
#'
#' attr(,"zs"): retrieves the obtained z statistics at the time-periods the function has looked for potential changes
#'
#' attr(,"ps"): retrieves the obtained p-values at the time-periods the function has looked for potential changes
#'
#' attr(,"mags"): retrieves the magnitude of change at the time-periods the function has looked for potential changes
#'
#'
#' if \code{history=TRUE} and \code{k} is a numeric vector, then the following attributes can also be retrieved.
#'
#' attr(,"history"): a dataframe containing the results (cp, magnitude, Z, and p.value) based on different adaptive sliding windows which are permutations of the given \code{k}
#'
#' attr(,"allzs"): a list which retrieves the obtained z statistics at the time-periods the function has looked for potential changes based on different adaptive sliding windows which are permutations of the given \code{k}
#'
#' attr(,"allmags"): a list which retrieves the magnitude of change at the time-periods the function has looked for potential changes based on different adaptive sliding windows which are permutations of the given \code{k}
#'
#' attr(,"allps"): a list which retrieves the obtained p-values at the time-periods the function has looked for potential changes based on different adaptive sliding windows which are permutations of the given \code{k}
#'
#' @author Mehdi Moradi \email{m2.moradi@yahoo.com}, Manuel Montesino-SanMartin \email{manuel.montesino@unavarra.es}
#'
#' @references
#' Moradi, M., Montesino-SanMartin, M., Ugarte, M. D., and Militino, A. F. (2020). Locally adaptive change-point detection with applications to remote sensing and land use changes.
#'
#' @seealso
#' \link[trend]{mk.test}, \link[stats]{p.adjust}
#'
#' @examples
#' x <- rnorm(50)
#' Z <- lacpd_mk(x,m=10,k=3)
#' plot(Z$s,attr(Z,"zs"),type="l",ylab = "Z",xlab = "time")
#' plot(Z$s,attr(Z,"mags"),type="l",ylab = "Z",xlab = "time")


#' @import trend
#' @import stats
#' @export
lacpd_mk <- function(x,m=1,k=2,blow=0.1,bup=(1-blow),leave=FALSE,adjust=FALSE,history=FALSE,...){

  if(anyNA(x)) stop("there is NA in your data")

  n <- length(x)

  a <- round(blow*n)
  b <- round(bup*n)
  s <- seq(a,b,by=1)

  ############### if k contains several values

  if(length(k)>1){

    out.list.z <- list()
    out.list.mag <- list()

    if(history){ hist1 <- matrix(ncol = 4,nrow = length(k)) }

    for (i in 1:length(k)) {
      out.pre <- lacpd_mk(x=x,m=m,k=k[i],blow=blow,bup=(1-blow),leave=leave,adjust=adjust,...)

      out.list.z[[i]] <- attr(out.pre,"zs")
      out.list.mag[[i]] <- attr(out.pre,"mags")

      if(history){hist1[i,] <- c(out.pre$cp,out.pre$mag,out.pre$z,out.pre$p)}

    }
    out.list.z <- do.call(rbind,out.list.z)
    zs <- colMeans(out.list.z)

    out.list.mag <- do.call(rbind,out.list.mag)
    mags <- colMeans(out.list.mag)

    if(history){

      out.hist.zs <- list()
      out.hist.ps <- list()
      out.hist.mags <- list()
      out.hist <- list()

      rn1 <- c()
      for (i in 1:nrow(hist1)) {
        rn1[i] <- paste(k[i])
      }
      colnames(hist1) <- c("change-point","magnitude","Z","p.value")
      rownames(hist1) <- rn1

      for (h in 1:(nrow(out.list.z)-1)) {

        allzs <- list()
        allmags <- list()
        allps <- list()
        cphist <- c()
        maghist <- c()
        zhist <- c()
        phist <- c()

        for (i in (h+1):nrow(out.list.z)){

          allzs[[i-h]] <- colMeans(out.list.z[h:i,])
          allmags[[i-h]] <- colMeans(out.list.mag[h:i,])

          cphist[i-h] <- s[which.max(allzs[[i-h]])]
          maghist[i-h] <- allmags[[i-h]][which.max(allzs[[i-h]])]
          zhist[i-h] <- max(allzs[[i-h]])

          if(adjust){
            allps[[i-h]] <- p.adjust(unlist(lapply(X=1:length(allzs[[i-h]]), function(j){
              2 * min(0.5, pnorm(abs(allzs[[i-h]][j]),lower.tail = FALSE))
            })),...)
          }else{
            allps[[i-h]] <- unlist(lapply(X=1:length(allzs[[i-h]]), function(j){
              2 * min(0.5, pnorm(abs(allzs[[i-h]][j]),lower.tail = FALSE))
            }))
          }

          phist[i-h] <- allps[[i-h]][which.max(allzs[[i-h]])]

        }
        hist <- cbind(cphist,maghist,zhist,phist)
        rn <- c()
        for (i in 1:nrow(hist)) {
          rn[i] <- paste(k[h],"-",k[i+h])
        }
        colnames(hist) <- c("change-point","magnitude","Z","p.value")
        rownames(hist) <- rn

        names(allps) <- names(allmags) <- names(allzs) <- rn


        out.hist[[h]] <- hist
        out.hist.zs[[h]] <- allzs
        out.hist.mags[[h]] <- allmags
        out.hist.ps[[h]] <- allps

      }

      out.hist.zs <- unlist(out.hist.zs, recursive=FALSE)
      out.hist.ps <- unlist(out.hist.ps, recursive=FALSE)
      out.hist.mags <- unlist(out.hist.mags, recursive=FALSE)
      out.hist <- do.call(rbind,out.hist)

    }
    cp <- s[which.max(zs)]
    mag <- mags[which.max(zs)]
    z <- max(zs)

    if(history){ out.hist <- rbind(hist1,out.hist)}

    if(adjust){
      ps <- p.adjust(unlist(lapply(X=1:length(zs), function(j){
        2 * min(0.5, pnorm(abs(zs[j]),lower.tail = FALSE))
      })),...)
    }else{
      ps <- unlist(lapply(X=1:length(zs), function(j){
        2 * min(0.5, pnorm(abs(zs[j]),lower.tail = FALSE))
      }))
    }

    p <- ps[which.max(zs)]


    out.final <- list(s=s,
                      cp=cp,
                      z=z,
                      mag=mag,
                      p=p)

    class(out.final) <- c("list","lacpd.mk")
    attr(out.final,"zs") <- zs
    attr(out.final,"mags") <- mags
    attr(out.final,"ps") <- ps

    if(history){
      attr(out.final,"history") <- out.hist
      attr(out.final,"allzs") <- out.hist.zs
      attr(out.final,"allmags") <- out.hist.mags
      attr(out.final,"allps") <- out.hist.ps
    }

    return(out.final)
   }



  low <- ceiling(n/k)
  up <- floor(n-(n/k))
  if(low < a) low <- a #stop(" n/k should be larger than blow*n where n is the length of x ")
  if(up > b)  up <- b

  low1 <- which(s==low)
  up1 <- which(s==up)



  ############### if k>2, then subsampling is done on the first and last part of x

  if(k==2){

    ################################################ lower part

    out.low <- lapply(X=1:m, function(i){

      zs <- numeric()
      mag <- numeric()

      for (j in 1:(low1)){

        z <- c(sample(x[1:(s[j]-1)],size = (n-s[j]+1),replace = T),
               x,
               sample(x[(s[j]+1):n],size =(s[j]),replace = T ))

        znew <- z[ceiling(((n*(k-1))/k)+1):(n+(n/k)+1)]

        if(leave) {
          znew <- znew[-(floor(length(znew)/2)+1)]
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[((zlen/2)+1):zlen]) - mean(znew[1:(zlen/2)]))
        }else{
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[(floor(zlen/2)+1):zlen]) - mean(znew[1:floor(zlen/2)]))
        }

        mk <- mk.test(znew)
        zs[j] <- abs(as.numeric(mk$statistic))

      }

      return(list(zs=zs,mag=mag))

    })

    out.z.low <- lapply(X=1:m, function(i){
      out.low[[i]]$zs
    })
    zs.low <- rowMeans(do.call(cbind,out.z.low))

    out.mag.low <- lapply(X=1:m, function(i){
      out.low[[i]]$mag
    })
    mag.low <- rowMeans(do.call(cbind,out.mag.low))

    ################################################## upper part
    out.up <- lapply(X=1:m, function(i){
      zs <- numeric()
      mag <- numeric()

      for (j in (up1+1):length(s)){

        z <- c(sample(x[1:(s[j]-1)],size = (n-s[j]+1),replace = T),
               x,
               sample(x[(s[j]+1):n],size =(s[j]),replace = T ))

        znew <- z[ceiling(((n*(k-1))/k)+1):(n+(n/k)+1)]

        if(leave) {
          znew <- znew[-(floor(length(znew)/2)+1)]
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[((zlen/2)+1):zlen]) - mean(znew[1:(zlen/2)]))
        }else{
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[(floor(zlen/2)+1):zlen]) - mean(znew[1:floor(zlen/2)]))
        }

        mk <- mk.test(znew)
        zs[j] <- abs(as.numeric(mk$statistic))

      }

      return(list(zs=zs[(up1+1):length(s)],mag=mag[(up1+1):length(s)]))
    })

    out.z.up <- lapply(X=1:m, function(i){
      out.up[[i]]$zs
    })
    zs.up <- rowMeans(do.call(cbind,out.z.up))

    out.mag.up <- lapply(X=1:m, function(i){
      out.up[[i]]$mag
    })
    mag.up <- rowMeans(do.call(cbind,out.mag.up))


    zs <- c(zs.low,zs.up)
    mags <- c(mag.low,mag.up)

  }
  else{ ########## k>2
    ################################################ lower part
    out.low <- lapply(X=1:m, function(i){
      zs <- numeric()
      mag <- numeric()

      for (j in 1:(low1)){

        z <- c(sample(x[1:(s[j]-1)],size = (n-s[j]+1),replace = T),
               x,
               sample(x[(s[j]+1):n],size =(s[j]),replace = T ))
        znew <- z[ceiling(((n*(k-1))/k)+1):(n+(n/k)+1)]
        if(leave) {
          znew <- znew[-(floor(length(znew)/2)+1)]
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[((zlen/2)+1):zlen]) - mean(znew[1:(zlen/2)]))
        }else{
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[(floor(zlen/2)+1):zlen]) - mean(znew[1:floor(zlen/2)]))
        }

        mk <- mk.test(znew)
        zs[j] <- abs(as.numeric(mk$statistic))

      }

      return(list(zs=zs,mag=mag))

    })

    out.z.low <- lapply(X=1:m, function(i){
      out.low[[i]]$zs
    })
    zs.low <- rowMeans(do.call(cbind,out.z.low))

    out.mag.low <- lapply(X=1:m, function(i){
      out.low[[i]]$mag
    })
    mag.low <- rowMeans(do.call(cbind,out.mag.low))

    ################################################ middle part
    zs.middle <-  c()
    mag_m <- numeric()
    for (j in (low1+1):(up1-1)){

      z <- c(sample(x[1:(s[j]-1)],size = (n-s[j]+1),replace = T),
             x,
             sample(x[(s[j]+1):n],size =(s[j]),replace = T ))

      znew <- z[ceiling(((n*(k-1))/k)+1):(n+(n/k)+1)]

      if(leave) {
        znew <- znew[-(floor(length(znew)/2)+1)]
        zlen <- length(znew)
        mag_m[j] <- abs(mean(znew[((zlen/2)+1):zlen]) - mean(znew[1:(zlen/2)]))
      }else{
        zlen <- length(znew)
        mag_m[j] <- abs(mean(znew[(floor(zlen/2)+1):zlen]) - mean(znew[1:floor(zlen/2)]))
      }

      mk <- mk.test(znew)
      zs.middle[j] <- abs(as.numeric(mk$statistic))

    }
    zs.middle <- zs.middle[(low1+1):(up1-1)]
    mag.middle <- mag_m[(low1+1):(up1-1)]
    ################################################ upper part
    out.up <- lapply(X=1:m, function(i){

      zs <- numeric()
      mag <- numeric()

      for (j in up1:length(s)){
        z <- c(sample(x[1:(s[j]-1)],size = (n-s[j]+1),replace = T),
               x,sample(x[(s[j]+1):n],size =(s[j]),replace = T ))

        znew <- z[ceiling(((n*(k-1))/k)+1):(n+(n/k)+1)]

        if(leave) {
          znew <- znew[-(floor(length(znew)/2)+1)]
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[((zlen/2)+1):zlen]) - mean(znew[1:(zlen/2)]))
        }else{
          zlen <- length(znew)
          mag[j] <- abs(mean(znew[(floor(zlen/2)+1):zlen]) - mean(znew[1:floor(zlen/2)]))
        }

        mk <- mk.test(znew)
        zs[j] <- abs(as.numeric(mk$statistic))
      }
      return(list(zs=zs[up1:length(s)],mag=mag[up1:length(s)]))
    })

    out.z.up <- lapply(X=1:m, function(i){
      out.up[[i]]$zs
    })
    zs.up <- rowMeans(do.call(cbind,out.z.up))

    out.mag.up <- lapply(X=1:m, function(i){
      out.up[[i]]$mag
    })
    mag.up <- rowMeans(do.call(cbind,out.mag.up))

    zs <- c(zs.low,zs.middle,zs.up)
    mags <- c(mag.low,mag.middle,mag.up)

  }


  cp <- s[which.max(zs)]
  mag <- mags[which.max(zs)]
  z <- max(zs)

  if(adjust){
    ps <- p.adjust(unlist(lapply(X=1:length(zs), function(j){
      2 * min(0.5, pnorm(abs(zs[j]),lower.tail = FALSE))
    })),...)
  }else{
    ps <- unlist(lapply(X=1:length(zs), function(j){
      2 * min(0.5, pnorm(abs(zs[j]),lower.tail = FALSE))
    }))
  }

  p <- ps[which.max(zs)]


  out.final <- list(s=s,
                    cp=cp,
                    z=z,
                    mag=mag,
                    p=p)

  class(out.final) <- c("list","lacpd.mk")
  attr(out.final,"zs") <- zs
  attr(out.final,"mags") <- mags
  attr(out.final,"ps") <- ps


  return(out.final)
}

#' @export
print.lacpd.mk <- function(x,round=5,...){
  cat("LACPD Mann-Kendall \n");
  cat("cp:", " ", paste0(x$cp), ", Z=",paste0(round(x$z,round)),", magnitude=",paste0(round(x$mag,round)),
      ", p.value=",paste0(round(x$p,round)),"\n");
}
