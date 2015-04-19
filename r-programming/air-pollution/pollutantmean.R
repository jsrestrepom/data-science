pollutantmean <- function(directory, pollutant, id = 1:332) {
  totalPollutant <- NULL
  for(monitor in id) {
    pollutionData <- readLectureFromMonitor(monitor, directory)
    pollutantSubset <- pollutionData[pollutant]
    cleanPollutantSubset <- pollutantSubset[!is.na(pollutantSubset)]
    totalPollutant <- c(totalPollutant, cleanPollutantSubset)
  }
  mean(totalPollutant)
}

readLectureFromMonitor <- function(monitorID, directory) {
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
