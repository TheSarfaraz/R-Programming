complete <- function(directory = "specdata", id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSv files
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the 
     ## number of complete cases
     
     
     ## Read files names and save in the list, with their full names
     files_list <- list.files(directory, full.names = TRUE)
     
     ## Import data and combine in a data frame
     data <- lapply(files_list[id], read.csv)
     data <- do.call(rbind, data)
     
     ## Count complete cases and put in a new data frame
     result <- data.frame()
     
     for (i in id) {
          obs <- sum(complete.cases(data[data["ID"] == i, ]))
          new <- cbind(i, obs)
          result <- rbind(result, new)
     }
     colnames(result) <- c("id", "nobs")
     return(result)
}