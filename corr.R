corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the 
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations
     ## NOTE: Do not round the result!
     
     
     ## Calculate complete cases from files in directory and note the ID which we need to keep for calculations
     nobs <- complete(directory = directory)
     nobs <- nobs[nobs$nobs > threshold, ]
     
     
     ## Read files names and save in the list, with their full names
     files_list <- list.files(directory, full.names = TRUE)
     
     ## Import data and combine in a data frame
     data <- lapply(files_list, read.csv)
     data <- do.call(rbind, data)
     
     ## Calculate the correlation between sulfate and nitrate
     result <- numeric()
     
     for (i in nobs$id) {
          cur_data <- data[data["ID"] == i, ]
          
          res <- round(cor(x = cur_data$sulfate, y = cur_data$nitrate, use = "complete.obs"), 5)
          
          result <- c(result, res)
     }
     
     return(result)
}