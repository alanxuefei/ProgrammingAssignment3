 
best <- function(state, outcome) {
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
  
  result1<-thedata[which(thedata$State==state),]
  ##print (result1[[thecolumn]])
  minmumdata<-min(as.numeric(result1[[thecolumn]]), na.rm=TRUE)
  ## print(result1[,c(thecolumn,"Hospital.Name")])
  ## print(minmumdata)
  result2<-result1[which(result1[[thecolumn]]==minmumdata),]
 ## str(result2)
 
  

    result3<-result2[order(result2$Hospital.Name),]
  ##str(result3)
    ## print(result2$Hospital.Name)
    return (result3$Hospital.Name[1])
 
}
## best("NY", "pneumonia")
##  best("TX", "heart attack")
  best("TX", "heart failure")
  ## best("MD", "heart attack")
  ##  best("MD", "pneumonia")
  ##   best("BB", "heart attack")
  ##  best("NY", "hert attack")
 
