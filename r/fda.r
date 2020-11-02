### Libraries
library('fda')
library(devtools)
devtools::install_github("moviedo5/fda.usc")
library('fda.usc')
### Functions

#'WIP:
#'Converts graphite data into matrix
#'Additionally it splits series names into smaller parts
#'It expects dataframe
#'TODO: Epoch time yes/no
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
  dateTime <- macierz[,1]
  rownames(macierz) <- dateTime
  macierz <- (macierz[,-1 ]) #remove dateTime from data
  return(t(macierz))
}


### Read data
url <- 'https://raw.github.com/panisko/graphite2r/master/resource/20200401_20200501_file.csv'
graphite_data <-  read.csv(url, na.strings = c("NA"), quote = "\"", header = TRUE, sep = ' ')
### Convert data to matrix
data <- GraphiteToMatrix(graphite_data)

### Vars for further processing
basis <- "fourier"
nderiv = 0
width = 1100
height = 660
nbasis = 51
nderiv = 0
optimise = FALSE


### Plot data
png(filename="data.png", width = width, height = height, unit = "px")
matplot(t(data),lty =0.5, cex =0.1, type = "p", pch = 1, col = 1:7, ylab = "Temperatura", xlab = "Czas w formacie epoc", axes = F, )
axis(2)
axis(side=1,at=1:ncol(data), labels = colnames(data))
legend("topleft", legend = rownames(data), col = 1:7, pch = 1)
dev.off()

### Data to functional data
#fdata <- fdata(data)
# fda* libraries don't work correctly with 'NA' values
fdata <- fdata(t(na.omit(t(data))))

### Use of optim.basis gives GCV and optimal number of basis
if(isTRUE(optimise)){
  fdata <- fdata(t(na.omit(t(data))))
  optim <- optim.basis(fdata,  type.basis = basis, lambda = 0)
  # optim <- optim.basis(fdata,  type.basis = basis, lambda = 1e6)
  print(optim$gcv)
  nbasis <- optim$numbasis.opt
}

fd <- fdata2fd( fdata, type.basis = basis, nbasis = 51, nderiv = nderiv)
mean <- mean.fd(fd)

### Plotting functional data
png(filename=paste("fdata_",basis, "_", nbasis,"_", nderiv,".png", sep='') , width = width, height = height, unit = "px")
plot.fd(fd, ylab = "Temperatura", xlab = "Czas w formacie epoc", axes = F)
lines(mean, cex= 7, cex.lab = 10 , col = "white")
axis(2)
axis(side=1,at=1:ncol(data), labels = colnames(data))
legend("topleft", legend = rownames(data), col = 1:7, pch = 1)
dev.off()

### Cluster analysis #1
png(filename="hist.png", width = 550, height = 330, unit = "px")
dist.matrix <- metric.dist(t(fd$coefs),p=2, method = "minkowski")
b <- as.dist(dist.matrix)
c2 <- hclust(b, method = "average")
plot(c2, main = "Dendogram metryka Mińskowskiego p=2" ,sub = "", xlab = "Pokój", ylab = "Miara (nie)podobieństwa")
dev.off()

### PCA
fd.pca <- pca.fd(fdobj = fd,  nharm = 2)
rownames(fd.pca$scores) <- fd$fdnames$reps
png(filename="pca.png", width = 550, height = 330, unit = "px")
plot(
  fd.pca$scores,
  ylab = "PC Score 2",
  xlab = "PC Score 1",
  col = 4,
  cex.lab = 0.5,
  ylim = c(min(fd.pca$scores[, 2] - 1),
           max(fd.pca$scores[, 2] + 1)),
  xlim = c(min(fd.pca$scores[, 1] - 1),
           max(fd.pca$scores[, 1] +
                 1)),
  type = "n"
)
text(fd.pca$scores[,1], fd.pca$scores[,2], labels = rownames(fd.pca$scores), cex = 1)
dev.off()
########

### PAM
fd.kmeans <-
  kmeans.fd(
    fdataobj = fdata,
    ncl = 3,
    metric = "metric.lp",
    draw = FALSE,
    cluster.size = 1,
    par.dfunc = NULL
  )

plot(fd.kmeans$centers)

fd.kmeans <-
  kmeans.fd(
    fdataobj = fdata,
    ncl = c(1:3),
    draw = TRUE,
    cluster.size = 1,
    #dfunc = "LD",
    par.dfunc = NULL
  )



