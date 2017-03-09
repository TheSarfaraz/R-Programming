pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
     ## 'directory' is a character vector of lengh 1 indicating
     ## the location of the CSV files
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either 'sulfate' or 'nitrate'.
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
     ## NOTE: Do not round the result!
     
     ## Read files names and save in the list, with their full names
     files_list <- list.files(directory, full.names = TRUE)
     
     ## Import data and combine in a data frame
     data <- lapply(files_list[id], read.csv)
     data <- do.call(rbind, data)
     
     ## Calulate mean
     result <- mean(data[[pollutant]], na.rm = TRUE)
     
     return(result)
}