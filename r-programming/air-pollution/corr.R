corr <- function(directory, threshold = 0) {
  result <- NULL
  observations <- complete(directory)
  for(observation in observations$id) {
    if(observations$nobs[observation] > threshold) {
      pollutionData <- readLectureOfMonitor(observation, directory)
      correlation <- cor(pollutionData$sulfate, pollutionData$nitrate, use = "complete.obs")
      result <- c(result, correlation)
    }
  }
  result
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
