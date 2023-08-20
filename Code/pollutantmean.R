pollutantmean <- function(directory, pollutant, id=1:332) {
  list_files <- list.files(directory, full.names = T)
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(list_files[i]))
  }
  mean_dat <- mean(dat[,pollutant], na.rm=T)
  mean_dat
}