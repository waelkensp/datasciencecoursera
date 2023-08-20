complete <- function(directory, id = 1:332) {
  list_files <- list.files(directory, full.names = T)
  dat <- data.frame()
  for (i in id) {
    temp_df <- read.csv(list_files[i])
    temp_df2 <- temp_df[complete.cases(temp_df),]
    n_line <- nrow(temp_df2)
    info_file <- c(i, n_line)
    dat <- rbind(dat, info_file)
  }
  colnames(dat) <- c("id", "nobs")
  dat
}