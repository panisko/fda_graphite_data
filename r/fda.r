### Libraries
library('fda')
library(devtools)
devtools::install_github("moviedo5/fda.usc")
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
graphite_data <-
  read.csv(
    url,
    na.strings = c("NA"),
    quote = "\"",
    header = TRUE,
    sep = ' '
  )
### Convert data to matrix
data <- GraphiteToMatrix(graphite_data)


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
plot.fd(fca$fd,
            ylab = "Temperatura",
            xlab = "Czas w formacie epoc",
            axes = F)
lines(fca$mean,
      cex = 14,
      cex.lab = 10 ,
      col = "red")
axis(2)
axis(side = 1,
     at = 1:ncol(data),
     labels = colnames(data))
legend("topleft",
       legend = rownames(data),
       col = 1:ncol(data),
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
plot(
  fca$pca$scores,
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
text(fca$pca$scores[, 1],
     fca$pca$scores[, 2],
     labels = rownames(fca$pca$scores),
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

plot(fca$pam$centers)

# fd.kmeans <-
#   kmeans.fd(
#     fdataobj = fca$fd,
#     ncl = c(1:3),
#     draw = TRUE,
#     cluster.size = 1,
#     #dfunc = "LD",
#     par.dfunc = NULL
#   )
