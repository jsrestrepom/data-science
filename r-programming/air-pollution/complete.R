complete <- function(directory, id = 1:332) {
  groupOfID <- NULL
  groupOfNobs <- NULL
  for(monitor in id) {
    numberObservedCases <- completeCasesFromAllObserved(readLectureOfMonitor(monitor, directory))
    if(numberObservedCases > 0) {
      groupOfID <- c(groupOfID, monitor)
      groupOfNobs <- c(groupOfNobs, numberObservedCases)
    }
  }
  data.frame(id = groupOfID, nobs = groupOfNobs)
}

completeCasesFromAllObserved <- function(observedCases) {
  count <- 0
  for(i in row.names(observedCases)) {
    if(isComplete(observedCases[i, ])) {
      count <- count + 1
    }
  }
  count
}

isComplete <- function(observedCase) {
  !is.na(observedCase$sulfate) & !is.na(observedCase$nitrate)
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
