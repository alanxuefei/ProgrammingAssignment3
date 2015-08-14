
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data 
  
  thedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available") 
  ## Check that state and outcome are valid \
  
  validOutcome <- c("Heart Attack","Heart Failure","Pneumonia")
  moutcome<-grepl(pattern = outcome, x = validOutcome, ignore.case = T )
  ##print (validOutcome[moutcome])
  if (sum(moutcome)!=1){
    stop("invalid outcome")
  }
  validState = unique(thedata$State)
  if (!state %in% validState) stop(print("invalid state"))
  
  thecolumn<-paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",validOutcome[moutcome]), sep = "")
  ##  print(thecolumn)
  
  ## Return hospital name in that state with the given rank
  result<-thedata[which(thedata$State==state),]
 
  ##print (result1[[thecolumn]])
  if (num=="worst"){
    result<-result[order(-as.numeric(result[[thecolumn]]),result$Hospital.Name),]
    result<-result[1,]
  }else{
    if (num=="best") num<-as.numeric(1)
    result<-result[order(as.numeric(result[[thecolumn]]),result$Hospital.Name),]
    result<-result[num,]
  }
    

  
  return (result$Hospital.Name)
  
}

##rankhospital("TX", "heart failure", 4)
##rankhospital("MD", "heart attack", "worst")
##rankhospital("MN", "heart attack", 5000)