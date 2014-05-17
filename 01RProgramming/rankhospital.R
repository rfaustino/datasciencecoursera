rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that state and outcome are valid
  
  ## Validate State
  #### Fill validation
  statedata <- data[data$State == state, ]  # filters the data frame to corresponding state;
  
  if (nrow(statedata)==0)
    stop("invalid state")
  
  if (outcome == 'heart attack') {
    statedata <- statedata[,c(7,2,11)]
  } else if (outcome == 'heart failure') {
    statedata <- statedata[,c(7,2,17)]
  } else if (outcome == 'pneumonia') {
    statedata <- statedata[,c(7,2,23)]
  } else {
    stop("invalid outcome")
  }
  
  statedata[,3] <- suppressWarnings(as.numeric(statedata[,3]))
  statedata <- na.omit(statedata)
  
  rankStateData <- rank(statedata[,3], na.last=NA, ties.method="first")
  
  if (num=="best") {
    r <- 1
  } else if (num =="worst") {
    r <- length(rankStateData)
  } else if (num <= length(rankStateData) ) {
    r <- num
  } else {
    return(NA)
  }
  
  #auxdata <- statedata
  #auxdata$rank <- rankStateData
  
  ## Return hospital name in that state with lowest 30-day death
  orderStateData <- order(statedata[,3], statedata[,2], na.last=NA)
  
  hospital <- statedata [orderStateData[r],2]
  
  return(hospital)
  
}