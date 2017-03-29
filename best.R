best <- function(state, outcome) {
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
     ## Check that state and outcome are valid
     states <- unique(data$State)
     if (!state %in% states) {
          stop("invalid state")
     }
     
     outcomes <- c("heart attack", "heart failure", "pneumonia")
     if (!outcome %in% outcomes) {
          stop("invalid outcome")
     }
     ## Return hospital name in that state with lowest 30-day death rate
     
     # Deaths by 'heart attack' = Column # 11
     # Deaths by 'heart failure' = Column # 17
     # Deaths by 'pneumonia' = Column # 23
     outcome_column <- if (outcome == "heart attack") {
          11
     } else if (outcome == "heart failure") {
          17
     } else if (outcome == "pneumonia") {
          23
     }
     # Only keep data of state we are interested in
     data <- data[data$State == state, ]
     
     data[, outcome_column] <- as.numeric(data[, outcome_column])
     
     min <- min(data[, outcome_column], na.rm = TRUE)
     min_index <- which(data[, outcome_column] == min)
     
     
     return(data[min_index, 2])
}