#Question 1 and 2

data <- read.csv("getdata-data-ss06hid.csv") 

L1M <- data[complete.cases(data[,"VAL"]),]  #clear missing values in VAL
L1M <- L1M[L1M$VAL == 24,] # Val=24 - the house worth more than 1 Million
nrow(L1M)


#Question 3
install.packages("xlsx")
library("xlsx")
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx", sheetIndex=1, colIndex = colIndex, rowIndex = rowIndex)

#Question 4
install.packages("XML")
library(XML)
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xData <- getURL(fileURL)
doc <- xmlTreeParse(fileURL, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
zipcode <- xpathSApply(rootNode, "//zipcode", xmlValue)
temp <- zipcode[zipcode==21231]
str(temp)


#Question 5
install.packages("data.table")
library("data.table")

fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv" 
DT <- fread (fileURL)


f1 <- function (trial_size){
  for (i in 1:trial_size){
    sapply(split(DT$pwgtp15,DT$SEX),mean)
  }
}
f2 <- function (trial_size){ 
  for (i in 1:trial_size){
    mean(DT$pwgtp15,by=DT$SEX)
  }
}
f3 <- function (trial_size){
  for (i in 1:trial_size){
    tapply(DT$pwgtp15,DT$SEX,mean)
  }
}
f4 <- function (trial_size){ #error
  for (i in 1:trial_size){
    rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
  }
}
f5 <- function (trial_size){
  for (i in 1:trial_size){
    DT[,mean(pwgtp15),by=SEX]
  }
}
f6 <- function (trial_size){
  for (i in 1:trial_size){
    mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
  }
}

system.time(f1(300))
system.time(f2(300)) #don't give the correct result
system.time(f3(300))
system.time(f4(300)) #error
system.time(f5(300))
system.time(f6(300))





