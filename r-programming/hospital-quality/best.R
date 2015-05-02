best <- function(state, outcome) {
  data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!isValid.state(state, data.outcome$State)) {
    stop ("invalid state")
  }
  if (!isValid.outcome(outcome)) {
    stop ("invalid outcome")
  }
  data.subset <- split(data.outcome, data.outcome$State)[[state]]
  outcome.index <- data.outcome.column(outcome)
  data.subset[, outcome.index] <- as.numeric(data.subset[, outcome.index])
  outcome.best <- min(data.subset[, outcome.index], na.rm = TRUE)
  rate <- NULL
  for (observation in 1:nrow(data.subset)) {
    if (identical(data.subset[observation, outcome.index], outcome.best)) {
      rate <- c(rate, data.subset[observation, "Hospital.Name"])
    }
  }
  rate
}

isValid.state <- function(state, allStates) {
  if (length(allStates[allStates == state]) > 0) {
    TRUE
  }
  else {
    FALSE
  }
}

isValid.outcome <- function(outcome) {
  outcome.all <- c("heart attack", "heart failure", "pneumonia")
  if (length(outcome.all[outcome.all == outcome]) > 0) {
    TRUE
  }
  else {
    FALSE
  }
}

data.outcome.column <- function(outcome) {
  data.outcome.config <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  data.outcome.config[[outcome]]
}
