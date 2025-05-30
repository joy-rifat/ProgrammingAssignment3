rankhospital <- function(state, outcome, num = "best") {
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
  ordered <- subset_data[order(subset_data[, col], subset_data$Hospital.Name), ]
  
  if (num == "best") {
    return(ordered$Hospital.Name[1])
  } else if (num == "worst") {
    return(ordered$Hospital.Name[nrow(ordered)])
  } else if (is.numeric(num) && num <= nrow(ordered)) {
    return(ordered$Hospital.Name[num])
  } else {
    return(NA)
  }
}