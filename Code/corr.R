corr <- function(directory, threshold=0) {
  list_files <- list.files(directory, full.names = T)
  list_info <- complete(directory)
  dat <- c()
  for (i in list_info$id) {
    if (list_info$nobs[i] >= threshold) {
      temp_df <- read.csv(list_files[i])
      temp_df2 <- temp_df[complete.cases(temp_df),]
      corre <- cor(temp_df2$sulfate, temp_df2$nitrate)
      dat <- c(dat, corre)
    }
  }
  dat
}