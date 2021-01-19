BuildUrl <- function(graphiteHost, startDate, endDate, metrics){
  url <-
    paste(
      graphiteHost,
      "/render?from=01%3A00_",
      startDate,
      "&until=23%3A59_",
      endDate,
      #"&target=home.fibaro.*.grzejnik.termometr.temp&target=home.fibaro.*.*.termostat.targetTemp&tz=UTC&format=csv",
      "&target=home.fibaro.*.grzejnik.termometr.temp&tz=UTC&format=csv",
      sep = ''
    )
  return(url);
}

RemoveEmptyRows <- function(data) {
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  return(data)
}
ReplaceEmptyWithZeros <- function(csv) {
  #this is needed in case metric is empty
  return(csv[csv==""] <- 0)
}
GetData <- function(url, separator) {
  return(
    read.csv2(
      url,
      #na.strings = 0,
      # quote = "\"",
      header = FALSE,
      sep = separator
    )
    )
}

GraphiteToMatrix <- function(data) {
  # data <- ReplaceEmptyWithZeros(data)
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
      l[[i]] <- as.numeric(data[data[, 1] == value, 3])
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
                norder = 4, 
                metric = "metric.lp",
                method = "average",
                optimise = FALSE,
                nharm = 2,
                ncl = 3,
                lambda = 1e6,
                
                ...) {
  call <- list(basis =  basis,
               nderiv = nderiv,
               width = width,
               height = height,
               nbasis = nbasis,
               metric = "metric.lp",
               method = "average",
               optimise = optimise,
               nharm = nharm,
               ncl = 3,
               hclust.mincowski = 2,
               lambda = lambda)
  response <- list()
  data <- data
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
  
  if (metric == "metric.lp"){
    dist.matrix <- metric.lp(fdata) 
  }
  if (metric == "metric.DTW"){
    dist.matrix <- metric.DTW(fdata) 
  }
  if (metric == "metric.hausdorff"){
    dist.matrix <- metric.hausdorff(fdata) 
  }
  if (metric == "semimetric.NPFDA"){
    dist.matrix <- semimetric.NPFDA(fdata) 
  }
  if (metric == "metric.lp"){
    dist.matrix <- metric.lp(fdata) 
  }
  b <- as.dist(dist.matrix)
  hclust <- hclust(b, method = method)
  
  
  pam <-
    kmeans.fd(
      fdataobj = fdata,
      ncl = ncl,
      draw = FALSE,
      cluster.size = 1,
      par.dfunc = NULL
    )
  
  response <-
    list(
      call = call,
      data =data,
      fdata = fdata,
      optimisedFd = optimisedFd ,
      fd = fd,
      mean = mean,
      pca = pca,
      hclust = hclust,
      pam = pam
    )
  
  return(response)
}
