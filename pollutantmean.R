pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  file_list <- list.files(directory, full.names=TRUE)
  
  poludata <- data.frame()
  
  for (i in id) {                                
        poludata <- rbind(poludata, read.csv(file_list[i]))
  }
  
  ## sub_poludata<- poludata[which(poludata[,pollutant])]
  
  ##data <- 
  ##  do.call("rbind", 
  ##          lapply(file_list[c(1:10)], 
  ##                   function(x) 
  ##                  read.csv(paste(x, sep=''), 
  ##                              stringsAsFactors = FALSE)))
 
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ##setwd(home)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ##sum(data$pollutant)
  mean(poludata[,pollutant],na.rm = TRUE)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
}