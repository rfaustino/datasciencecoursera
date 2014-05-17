complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  nobs <- vector(mode="numeric", length=length(id))
  
  for (i in 1:length(id)) {
    
    index <- id[i]
    index <- sprintf("/%03d.csv",index)
    fname <- paste ("./", directory, index, sep="")
    
    data <- read.csv(fname, colClasses = "character")
    
    datacomplete <- data[complete.cases(data),]
    nobs[i] <- nrow(datacomplete)
  }
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  df <- data.frame(id=id, nobs=nobs)
  return (df)
}