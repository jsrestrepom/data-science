rankall <- function(outcome, num = "best") {
  data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!isValid.outcome(outcome)) {
    stop ("invalid outcome")
  }
  outcome.index <- data.outcome.column(outcome)
  data.outcome[, outcome.index] <- as.numeric(data.outcome[, outcome.index])
  data.byState <- split(data.outcome, data.outcome$State)

  list.state <- names(data.byState)
  list.ranking <- sapply(data.byState, fooFunction, num, outcome.index)
  
  data.frame(hospital = list.ranking, state = list.state, row.names = list.state)
}

fooFunction <- function(data.subset.state, num, mortality) {
  ranking <- data.subset.state[order(data.subset.state[, mortality], data.subset.state$Hospital.Name), ]
  ranking.clean <- ranking[, mortality][complete.cases(ranking[, mortality])]
  if (num == "best") {
    num <- 1
  }
  if (num == "worst") {
    num <- length(ranking.clean)
  }
  if (num > length(ranking.clean)) {
    NA
  }
  else {
    ranking$Hospital.Name[num]
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
