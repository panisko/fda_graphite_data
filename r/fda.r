### Libraries
library('fda')
library(devtools)
#devtools::install_github("moviedo5/fda.usc")
library('fda.usc')
### Functions


source("./functions.R")

#'WIP:
#'Converts graphite data into matrix
#'Additionally it splits series names into smaller parts
#'It expects dataframe
#'TODO: Epoch time yes/no

### Read data
url <-
  'https://raw.github.com/panisko/graphite2r/master/resource/20200401_20200501_file.csv'

startDate <- "20210115"
endDate <- "20210115"
url <- BuildUrl(graphiteHost = "http://graphite:8080", startDate = startDate, endDate = endDate)

graphiteData <-
  read.csv2(
    url,
#    na.strings = "",
#    quote = "\"",
    header = FALSE,
    sep = ','
  )
rm(data)
### Convert data to matrix
data <- GraphiteToMatrix(graphiteData)
data <- data[rowSums(is.na(data)) != ncol(data), ]
data[is.na(data)] <- 0

is.na(data)
### Vars for further processing
fca <- fca(data)

### Plot data
png(
  filename = "data.png",
  width = fca$call$width,
  height = fca$call$height,
  unit = "px"
)
matplot(
  t(data),
  lty = 0.5,
  cex = 0.1,
  type = "p",
  pch = 1,
  col = 1:ncol(data),
  ylab = "Temperatura",
  xlab = "Czas w formacie epoc",
  axes = F,
  
)
axis(2)
axis(side = 1,
     at = 1:ncol(data),
     labels = colnames(data))
legend("topleft",
       legend = rownames(data),
       col = 1:7,
       pch = 1)
dev.off()


### Plotting functional data
png(
  filename = paste("fdata_", fca$call$basis, "_", fca$call$nbasis , "_", fca$call$nderiv, ".png", sep = '') ,
  width = fca$call$width,
  height = fca$call$height,
  unit = "px"
)
fd <- fca$fd
plot(fd,
            ylab = "Temperatura",
            xlab = "Czas w formacie epoc",
            cex =7,
            axes = F)
lines(fca$mean,
      cex = 14,
      cex.lab = 10 ,
      col = "red")

axis(2)
axis(side = 1,
     at = 1:length(fd$fdnames$time),
     labels = fd$fdnames$time)
legend("topleft",
       legend = fd$fdnames$reps,
       col = 1:length(fd$fdnames$reps),
       pch = 1)
dev.off()

### Cluster analysis #1
png(
  filename =  paste("hist_", fca$call$basis, "_", fca$call$nbasis , "_", fca$call$nderiv, ".png", sep = '') ,
  width = fca$call$width,
  height = fca$call$height,
  unit = "px"
)
plot(
  fca$hclust,
  main = "Dendogram metryka Mińskowskiego p=2" ,
  sub = "",
  xlab = "Pokój",
  ylab = "Miara (nie)podobieństwa"
)
dev.off()

### PCA

png(
  filename =  paste("pca_", fca$call$basis, "_", fca$call$nbasis , "_", fca$call$nderiv, ".png", sep = '') ,
  width = fca$call$width,
  height = fca$call$height,
  unit = "px"
)
pca <- fca$pca
plot(
  pca$scores,
  ylab = "PC Score 2",
  xlab = "PC Score 1",
  col = 4,
  cex.lab = 0.5,
  ylim = c(min(fca$pca$scores[, 2] - 1),
           max(fca$pca$scores[, 2] + 1)),
  xlim = c(min(fca$pca$scores[, 1] - 1),
           max(fca$pca$scores[, 1] +
                 1)),
  type = "n"
)
text(pca$scores[, 1],
     pca$scores[, 2],
     labels = rownames(pca$scores),
     cex = 1)
dev.off()
########

### PAM
# fd.kmeans <-
#   kmeans.fd(
#     fdataobj = fca$fd,
#     ncl = 3,
#     metric = "metric.lp",
#     draw = FALSE,
#     cluster.size = 1,
#     par.dfunc = NULL
#   )
# fd.kmeans <-
#   kmeans.fd(
#     fdataobj = fca$fd,
#     ncl = c(1:3),
#     draw = TRUE,
#     cluster.size = 1,
#     #dfunc = "LD",
#     par.dfunc = NULL
#   )

###PAM 
png(
  filename =  paste("pam_", fca$call$basis, "_", fca$call$nbasis , "_", fca$call$nderiv, ".png", sep = '') ,
  width = fca$call$width,
  height = fca$call$height,
  unit = "px"
)
data <- fca$pam$centers$data

matplot(
  t(data),
  lty = 0.5,
  cex = 0.1,
  #type = "p",
  pch = 1,
  col = 1:ncol(data),
  ylab = "Temperatura",
  xlab = "Czas w formacie epoc",
  axes = F,
)

axis(2)
axis(side = 1,
     at = 1:ncol(data),
     labels = colnames(data))
legend("topleft",
       legend = rownames(data),
       col = 1:ncol(data),
       pch = 1)
dev.off()


plot.pca.fd(fca$pca)
method <- "average"
dist.matrix <-
  metric.lp(fca$fdata)
b <- as.dist(dist.matrix)
hclust <- hclust(b, method = method)
plot(hclust)
plot(metric.lp(fca$fdata))
metric.lp(fca$data[1,], fca$data[2,])
metric.lp(fca$fdata)
metric <- "metric.lp"
met <- `metric`
get(met(fca$fdata))

do.call("metric.lp", fca$fdata)

metric.lp(fca$fdata)

#TESTING
data1 <- read.table(file = 'test.csv', header = TRUE, sep = ',', dec = '.', check.names = FALSE)
colnames(data1) <- as.numeric(colnames(data1))

View(data1)
