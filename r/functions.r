url <- 'https://raw.github.com/panisko/graphite2r/master/resource/20200401_20200501_file.csv'
data <-  read.csv(url, na.strings = c("NA", "NaN", " "), sep = ' ')
