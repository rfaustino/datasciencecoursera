rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")  
  
  state <- data$State
  state <- sort(unique(state))
  
  hospital <- rep("", length(state))
  
  for (i in 1:length(state)) {   # for each state
  
      statedata <- data[data$State == state[i], ]  # filters the data frame to a state;
  
      ## Check that outcome are valid
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
        r <- NA
      }

      if (is.na(r)){    
        hospital[i] <- NA
      } else {
        orderStateData <- order(statedata[,3], statedata[,2], na.last=NA)
        hospital[i] <- statedata [orderStateData[r],2]
      }
  
  }
  
  return(data.frame(hospital=hospital, state=state, row.names = state))
  
}