pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  for (i in 1:length(id)) {
    
    index <- id[i]
    index <- sprintf("/%03d.csv",index)
    fname <- paste ("./", directory, index, sep="")
    
    data <- read.csv(fname, colClasses = "character")
    dnum <- as.numeric (data[,pollutant])
    dnum <- na.omit(dnum)
    
    if (i == 1){
      meanvect <- dnum
    } else {
      meanvect <- c(meanvect, dnum)
    }
    
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  m <-  mean(meanvect, na.rm = TRUE)
  return (round(m, digits=3))
  
}