getDeathRateColName <- function(outcomeName) {
  validOutcomes <- matrix(nrow = 3, ncol = 2)
  validOutcomes[,1] <- c("heart attack", "heart failure", "pneumonia")
  validOutcomes[,2] <- paste(
    "Hospital.30.Day.Death..Mortality..Rates.from.",
    c("Heart.Attack", "Heart.Failure", "Pneumonia"), 
    sep = ""
  )
  colnames(validOutcomes) <- c("names", "deathRateColNames")
  
  # If outcomeName is not valid, display error message.
  if (!(outcomeName %in% validOutcomes[,"names"])) {
    stop("Invalid outcome!")
  }
  
  # Otherwise, return the corresponding death rate column name from
  # validOutcomes.
  validOutcomes[validOutcomes[,"names"] == outcomeName, "deathRateColNames"]
}


data.frame.numerifyCols <- function(dataFrame, colsToNumerify) {
  for (col in colsToNumerify) {
    dataFrame[, col] <- suppressWarnings(as.numeric(dataFrame[, col]))
    
    # Filter out all rows with a missing value in col.
    dataFrame <- dataFrame[complete.cases(dataFrame[, col]),]
  }
  
  dataFrame
}


path <- file.path(getwd(),"Data","outcome-of-care-measures.csv")
read.outcomes <- function(state = NULL, columns = NULL, make.numeric = NULL) {
  outcomesData <- read.csv(path, 
                           colClasses = "character")
  
  # Validate parameters.
  if (!is.null(state) && !(state %in% outcomesData$State)) {
    stop("Invalid state!")
  }
  if (!is.null(columns) && !is.null(make.numeric) && 
      length(columns) != length(make.numeric)) {
    stop("length(columns) does not equal length(make.numeric)!")
  }
  
  # Apply filters based on inputs.
  if (!is.null(state)) {
    outcomesData <- outcomesData[outcomesData$State == state,]
  }
  if (!is.null(columns)) {
    outcomesData <- outcomesData[, columns]
  }
  if (!is.null(make.numeric)) {
    outcomesData <- data.frame.numerifyCols(outcomesData, columns[make.numeric])
  }
  
  outcomesData
}