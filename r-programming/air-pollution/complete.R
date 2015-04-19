complete <- function(directory, id = 1:332) {
  groupOfID <- NULL
  groupOfNobs <- NULL
  for(monitor in id) {
    numberObservedCases <- numberOfCompleteCasesFrom(readLectureOfMonitor(monitor, directory))
    groupOfID <- c(groupOfID, monitor)
    groupOfNobs <- c(groupOfNobs, numberObservedCases)
  }
  data.frame(id = groupOfID, nobs = groupOfNobs)
}

numberOfCompleteCasesFrom <- function(observedCases) {
  isCompleteCondition <- complete.cases(observedCases$nitrate, observedCases$sulfate)
  completeCases <- observedCases$ID[isCompleteCondition]
  length(completeCases)
}

readLectureOfMonitor <- function(monitorID, directory) {
  zeroBefore <- if(monitorID < 10) {
    "00"
  }
  else if(monitorID < 100) {
    "0"
  }
  else {
    ""
  }
  filename <- paste(zeroBefore, monitorID, ".csv", sep = "")
  read.csv(paste(directory, filename, sep = "/"), comment.char = "")
}
