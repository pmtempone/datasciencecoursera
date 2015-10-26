best<-function(state,outcome){
  ##Readoutcomedata
  outcomedata <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ##Checkthatstateandoutcomearevalid
  if (is.na(match(state,outcomedata$State))==TRUE){
    stop("invalid state")
  } else {
      if (outcome=='heart attack'){
        c <- 11
      } else if (outcome == 'heart failure') {
        c <- 17
      } else if (outcome == 'pneumonia') {
        c <- 23
      } else {
        stop("invalid outcome")
      }
  }
  
  ##Returnhospitalnameinthatstatewithlowest30-daydeath
  
  outcomedata_result <- outcomedata[outcomedata$State == state,]
  
  outcomedata_result <- outcomedata_result[which.min(outcomedata_result[,c]),"Hospital.Name"]
  
  ##Handling ties
  
  resultado <- sort(outcomedata_result,na.last = TRUE)
  resultado[1]
  
  ##rate
  
  
}