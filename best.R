best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome_map <- list(
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23
  )
  
  if (!(outcome %in% names(outcome_map))) stop("invalid outcome")
  if (!(state %in% data$State)) stop("invalid state")
  
  col <- outcome_map[[outcome]]
  data[, col] <- suppressWarnings(as.numeric(data[, col]))
  
  subset_data <- data[data$State == state & !is.na(data[, col]), ]
  best_row <- subset_data[order(subset_data[, col], subset_data$Hospital.Name), ][1, ]
  
  return(best_row$Hospital.Name)
}