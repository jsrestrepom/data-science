complete <- function(directory, id = 1:332) {
  listOfID <- NULL
  listOfNobs <- NULL
  for(monitor in id) {
    pollutionData <- readLectureFromMonitor(monitor, directory)
    count <- 0
    for(i in row.names(pollutionData)) {
      if(isComplete(pollutionData[i, ])) {
        count <- count + 1
      }
    }
    if(count > 0) {
      listOfID <- c(listOfID, monitor)
      listOfNobs <- c(listOfNobs, count)
    }
  }
  data.frame(id = listOfID, nobs = listOfNobs)
}

isComplete <- function(observedCase) {
  !is.na(observedCase$sulfate) & !is.na(observedCase$nitrate)
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
