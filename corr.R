corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  file_list <- list.files(directory)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  cor_vector <- vector()
  
  for(f in 1:length(file_list)){
    data_complete <- read.csv( paste(directory, "/", file_list[f], sep=""))
    data_complete <- data_complete[complete.cases(data_complete),]
    if ( nrow(data_complete) > threshold ) {
      cor_vector <- c(cor_vector, cor(data_complete$sulfate, data_complete$nitrate) ) # append corralations
    }
  }
  
  cor_vector
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}