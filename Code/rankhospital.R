rankhospital <- function(state, outcome, num) {
  source("auxiliaries.R")
  
  deathRateColName <- getDeathRateColName(outcome)
  targetData <- read.outcomes(state,
                              columns = c(deathRateColName, "Hospital.Name"),
                              make.numeric = c(TRUE, FALSE))
  
  # Use tapply to group hospital names by death rates and sort them in
  # alphabetical order w/in each group. Then, flatten the array returned
  # by tapply, so that each index corresponds to exactly 1 hospital name.
  hospitalsPerDR <- unlist(tapply(targetData$Hospital.Name, 
                                  targetData[,deathRateColName], sort))
  totalNumHospitals <- length(hospitalsPerDR)
  
  targetHospitalName <- NA	# Default
  if (num == "best") {
    targetHospitalName <- hospitalsPerDR[[1]]
  }
  else if (num == "worst") {
    targetHospitalName <- hospitalsPerDR[[totalNumHospitals]]
  }
  else if (num >= 1 && num <= totalNumHospitals) {
    # Can access by num since elements in hospitalsPerDR are organized
    # in order of increasing mortality rate and hospital name.
    targetHospitalName <- hospitalsPerDR[[num]]
  }
  
  targetHospitalName
}
