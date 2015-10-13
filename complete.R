complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  file_list <- list.files(directory, full.names=TRUE)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  complete_data <- data.frame()
  
  final_data <- data.frame()
  
  for (i in id) {                                
    complete_data <- read.csv(file_list[i])
    count_complete_data <- data.frame(id= i,nobs = sum(complete.cases(complete_data)))
    final_data <- rbind(final_data,count_complete_data)
  }
  
  final_data
  ##unique(data.frame(id=complete_data$ID,nobs=sum(complete.cases(complete_data))))
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}