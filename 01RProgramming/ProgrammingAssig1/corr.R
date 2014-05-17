corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  source("complete.R")
  # complete_thr <- subset(comp, nobs>threshold)
  # rows <- nrow(complete_thr) 
  
  vcorr <- vector(mode="numeric")

  
  for (i in 1:332) {
    
    comp <- complete ("specdata", i)
    
    if (comp$nobs > threshold) {
      
      index <- sprintf("/%03d.csv",i)
      fname <- paste ("./", directory, index, sep="")
      data <- read.csv(fname, colClasses = "character")
      datacomplete <- data[complete.cases(data),]
      
      #vcorr[i] <- round(cor(as.numeric(datacomplete$sulfate), as.numeric(datacomplete$nitrate)), digits = 5)
      vcorr[i] <- cor(as.numeric(datacomplete$sulfate), as.numeric(datacomplete$nitrate))
    } 
    
  }
  
  vcorr <- na.omit(vcorr)
  
  return (vcorr)
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}