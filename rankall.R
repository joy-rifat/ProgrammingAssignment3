rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome_map <- list(
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23
  )
  
  if (!(outcome %in% names(outcome_map))) stop("invalid outcome")
  
  col <- outcome_map[[outcome]]
  data[, col] <- suppressWarnings(as.numeric(data[, col]))
  data_subset <- data[, c(2, 7, col)]
  colnames(data_subset) <- c("hospital", "state", "rate")
  data_subset <- data_subset[!is.na(data_subset$rate), ]
  
  by_state <- split(data_subset, data_subset$state)
  
  get_rank <- function(df) {
    ordered <- df[order(df$rate, df$hospital), ]
    if (num == "best") return(ordered$hospital[1])
    if (num == "worst") return(ordered$hospital[nrow(ordered)])
    if (is.numeric(num) && num <= nrow(ordered)) return(ordered$hospital[num])
    return(NA)
  }
  
  result <- data.frame(
    hospital = sapply(by_state, get_rank),
    state = names(by_state)
  )
  
  rownames(result) <- names(by_state)
  return(result)
}
