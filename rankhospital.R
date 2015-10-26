rankhospital<-function(state,outcome,num="best"){
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
  
  ##Returnhospitalnameinthatstatewiththegivenrank
  
  outcomedata_result <- outcomedata[outcomedata$State == state,]
  
  outcomedata_filtrado <- as.numeric(outcomedata_result[,c])
  ##order_outcome <- order(outcomedata_result[c],outcomedata_result$Hospital.Name,na.last = TRUE)
  order_outcome <- rank(outcomedata_filtrado, na.last=NA)
  ##30-daydeathrate
  
 ## resultado <- sort(outcomedata_result,na.last = TRUE)
  
  if (num=="best"){
    return(outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[1]])
  } else if (num=="worst"){
    return(outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[length(order_outcome)]])
  } else if (is.numeric(num)==TRUE) {
    return(outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[num]])
  } else{
    stop("invalid num")
  }
}