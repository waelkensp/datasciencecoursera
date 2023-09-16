path <- file.path(getwd(),"Data","outcome-of-care-measures.csv")
outcome <- read.csv(path, colClasses = "character")
head(outcome)

ncol(outcome)
names(outcome)

outcome[,11] <- as.numeric(outcome[, 11])
hist(outcome[,11])

best <- function(state, outcome) {
  path <- file.path(getwd(),"Data","outcome-of-care-measures.csv")
  df <- read.csv(path, colClasses = "character")
  if (!state %in% df[,7]) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
}



  