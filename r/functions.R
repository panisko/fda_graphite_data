GraphiteToMatrix <- function(data) {
  n <- length(levels(as.factor(data[, 1])))
  l <- vector("list", n)
  macierz <- matrix(ncol = n)
  for (i in 1:n) {
    value <- unique(data[, 1])[i]
    #każdy element wektora to oddzielny pokój
    #vectorvalue
    #przy pierwszym zapisz czas i wartosci
    column.name <- strsplit(as.character(value) , '[.]')[[1]][3]
    print(paste("++Level: ", i, " value: ", column.name))
    if (i == 1) {
      dateTime <- data[data[, 1] == value, 2]
      
      macierz <- cbind(as.numeric(as.POSIXlt(dateTime)))
      
      colnames(macierz)[1] <- "dateTime"
      l[[i]] <- as.numeric(data[data[, 1] == value, 3])
      macierz <- cbind(macierz, l[[i]])
      colnames(macierz)[2] <- paste(column.name)
    }
    else {
      l[[i]] <- data[data[, 1] == value, 3]
      macierz <- cbind(macierz, l[[i]])
      colnames(macierz)[i + 1] <- paste(column.name)
    }
  }
  dateTime <- macierz[, 1]
  rownames(macierz) <- dateTime
  macierz <- (macierz[, -1]) #remove dateTime from data
  return(t(macierz))
}

fca <- function(data = data,
                basis =  "fourier",
                nderiv = 0,
                width = 1100,
                height = 660,
                nbasis = 51,
                metric = "metric.dist",
                optimise = FALSE,
                nharm = 2,
                lambda = 1e6,
                
                ...) {
  call <- list(basis =  basis,
               nderiv = nderiv,
               width = width,
               height = height,
               nbasis = nbasis,
               metric = metric,
               optimise = optimise,
               nharm = nharm,
               lambda = lambda)
  response <- list()
  fdata <- fdata(t(na.omit(t(data))))
  
  if (isTRUE(optimise)) {
    fdata <- fdata(t(na.omit(t(data))))
    optimisedFd <-
      optim.basis(fdata,  type.basis = basis, lambda = 0)
    print(optim$gcv)
    #nbasis <- optim$numbasis.opt
  }
  else {
    optimisedFd = NULL
  }
  
  
  fd <-
    fdata2fd(fdata,
             type.basis = basis,
             nbasis = nbasis ,
             nderiv = nderiv)
  
  mean <- mean.fd(fd)
  
  pca <- pca.fd(fdobj = fd,  nharm = nharm)
  rownames(pca$scores) <- fd$fdnames$reps
  
  dist.matrix <-
    metric.dist(t(fd$coefs), p = 2, method = "minkowski")
  b <- as.dist(dist.matrix)
  hclust <- hclust(b, method = "average")
  
  
  pam <-
    kmeans.fd(
      fdataobj = fdata,
      ncl = c(1:3),
      draw = FALSE,
      cluster.size = 1,
      par.dfunc = NULL
    )
  
  response <-
    list(
      call = call,
      optimisedFd = optimisedFd ,
      fd = fd,
      mean = mean,
      pca = pca,
      hclust = hclust,
      pam = pam
    )
  
  return(response)
}
