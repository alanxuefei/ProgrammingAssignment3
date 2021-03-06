
rankall <- function(outcome, num = "best") {
  ## Read outcome data 
  
  thedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available") 
  ## Check that state and outcome are valid \
  
  validOutcome <- c("Heart Attack","Heart Failure","Pneumonia")
  moutcome<-grepl(pattern = outcome, x = validOutcome, ignore.case = T )
  ##print (validOutcome[moutcome])
  if (sum(moutcome)!=1){
    stop("invalid outcome")
  }
  validState =   sort(unique(thedata$State)) 
   
  thecolumn<-paste("Hospital.30.Day.Death..Mortality..Rates.from.", gsub(" ",".",validOutcome[moutcome]), sep = "")
  ##  print(thecolumn)
  ##finalresult<-c("hospital", "state");
  for (state in validState) {
 
    ## Return hospital name in that state with the given rank
    result<-thedata[which(thedata$State==state),]
    if (num=="worst"){
      result<-result[order(-as.numeric(result[[thecolumn]]),result$Hospital.Name),]
      result<-result[1,]
    }else{
      if (num=="best") num<-as.numeric(1)
        result<-result[order(as.numeric(result[[thecolumn]]),result$Hospital.Name),]
        result<-result[num,]
    }
    
     finalresult<-rbind(finalresult, c(result$Hospital.Name, state) )
  }
  
  data.frame(hospital=finalresult[,1],state=finalresult[,2])
}


 head(rankall("heart attack", 20), 10)
##tail(rankall("pneumonia", "worst"), 3)
##tail(rankall(?"heart failure"), 10)



 