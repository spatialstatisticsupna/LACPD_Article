#' @export
lacpd_bcpw <- function(x,m=1,k=2,blow=0.1,bup=(1-blow),leave=FALSE,adjust=FALSE,history=FALSE,...){

  if(anyNA(x)) stop("there is NA in your data")

  n <- length(x)

  a <- round(blow*n)
  b <- round(bup*n)
  s <- seq(a,b,by=1)

  if(length(k)>1){

    out.list.z <- list()
    out.list.mag <- list()

    for (i in 1:length(k)) {
      out.pre <- lacpd_bcpw(x=x,m=m,k=k[i],blow=blow,bup=(1-blow),leave=leave,adjust=adjust,...)
      out.list.z[[i]] <- attr(out.pre,"zs")
      out.list.mag[[i]] <- attr(out.pre,"mags")

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

    class(out.final) <- c("list","lacpd.bcpw")

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

        mk <- bcpw(znew)
        zs[j] <- abs(as.vector(mk[1]))

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

        mk <- bcpw(znew)
        zs[j] <- abs(as.vector(mk[1]))

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

        mk <- bcpw(znew)
        zs[j] <- abs(as.vector(mk[1]))

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

      mk <- bcpw(znew)
      zs.middle[j] <- abs(as.vector(mk[1]))

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

        mk <- bcpw(znew)
        zs[j] <- abs(as.vector(mk[1]))
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

  class(out.final) <- c("list","lacpd.bcpw")

  attr(out.final,"zs") <- zs
  attr(out.final,"mags") <- mags
  attr(out.final,"ps") <- ps


  return(out.final)
}

#' @export
}
