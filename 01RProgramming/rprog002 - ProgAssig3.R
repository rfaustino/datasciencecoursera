## 1 Plot the 30-day mortality rates for heart attack
# read data file to variable outcome
outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

# head and names functions
head(outcome)
names(outcome)

# histogram of the 30-day death rate from heart attack
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

## 2 Finding the best hospital in a state

best <- function(state, outcome) {
  ## Read outcome data
  outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that state and outcome are valid
  
  ## Validate State
  #### Fill validation
  statedata <- data[data$State == state, ]  # filters the data frame to corresponding state;
  
  if (outcome == 'heart attack') {
    statedata <- statedata[,c(7,2,11)]
  } else if (outcome == 'heart failure') {
    statedata <- statedata[,c(7,2,17)]
  } else if (outcome == 'pneumonia') {
    statedata <- statedata[,c(7,2,23)]
  } else {
    stop("invalid outcome")
  }
  
  statedata[,3] <- as.numeric(statedata[,3])
  statedata <- na.omit(statedata)
  
  
  ## Return hospital name in that state with lowest 30-day death
  orderStateData <- order(statedata[,3], na.last=NA)
  hospital <- statedata [orderStateData[1],2]
  #rankStateData <- rank(statedata[,3], na.last=NA, ties.method="first")
  
  ## rate
  
}