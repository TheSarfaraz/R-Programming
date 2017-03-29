rankall <- function(outcome, num = "best") {
     
     ## Reads Outcome Data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character', na.strings = 'Not Available')[,c(2,7,11,17,23)]
     
     ## Check that state and outcome are valid
     states <- unique(data$State)
     
     outcomes <- c("heart attack", "heart failure", "pneumonia")
     if (!outcome %in% outcomes) {
          stop("invalid outcome")
     }
     
     # Remove columns by outcome, only left HospitalName and Deaths by outcome
     if (outcome == "heart attack") {
          data = data[,c(1,2,3)]
     } else if(outcome == "heart failure") {
          data = data[,c(1,2,4)]
     } else if(outcome == "pneumonia") {
          data = data[,c(1,2,5)]
     }
     names(data)[3] = "Deaths"
     
     ## Convert relevant column to numeric from character
     data[, 3] <- as.numeric(data[, 3])

     # Remove rows with NA
     data = data[!is.na(data$Deaths),]
     
     ## We have to compute find rank in each state
     
     ## Return a data frame with the hospital names and the
     ## (abbreviated) state name
     
     splited = split(data, data$State)
     ans = lapply(splited, function(x, num) {
          # Order by Deaths and then HospitalName
          x = x[order(x$Deaths, x$Hospital.Name),]
          
          # Return
          if(class(num) == "character") {
               if(num == "best") {
                    return (x$Hospital.Name[1])
               }
               else if(num == "worst") {
                    return (x$Hospital.Name[nrow(x)])
               }
          }
          else {
               return (x$Hospital.Name[num])
          }
     }, num)
     
     #Return data.frame with format
     return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}