rankhospital <- function(state, outcome, num = "best") {
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
  outcome.ranking <- data.subset[order(data.subset[, outcome.index], data.subset$Hospital.Name), ]
  rank.hospital(num, outcome.ranking, outcome.index)
}

rank.hospital <- function(num, outcome.ranking, outcome.index) {
  clean.ranking <- outcome.ranking[, outcome.index][complete.cases(outcome.ranking[, outcome.index])]
  if (num == "best") {
    num <- 1
  }
  if (num == "worst") {
    num <- length(clean.ranking)
  }
  if (num > length(clean.ranking)) {
    NA
  }
  else {
    outcome.ranking$Hospital.Name[num]
  }
}

isValid.state <- function(state, state.all) {
  if (length(state.all[state.all == state]) > 0) {
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
