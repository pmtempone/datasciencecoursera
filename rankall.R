rankall<-function(outcome,num="best"){
  ##Readoutcomedata
  outcomedata <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  states <- sort(unique(outcomedata$State))
  
  
  ##Checkthatstateandoutcomearevalid
  
  if (outcome=='heart attack'){
      c <- 11
    } else if (outcome == 'heart failure') {
      c <- 17
    } else if (outcome == 'pneumonia') {
      c <- 23
    } else {
      stop("invalid outcome")
    }
 ## create data.frame
  
  rank_list <- as.data.frame(matrix(nrow =length(states),ncol = 2))
  names(rank_list) <- c("hospital","state")
  
  ##Returnhospitalnameinthatstatewiththegivenrank
  for (i in 1:length(states)){
    
    outcomedata_result <- outcomedata[outcomedata$State == states[i],]
    
    outcomedata_filtrado <- as.numeric(outcomedata_result[,c])
  ##order_outcome <- order(outcomedata_result[c],outcomedata_result$Hospital.Name,na.last = TRUE)
    order_outcome <- rank(outcomedata_filtrado, na.last=NA)
  ##30-daydeathrate
  
  ## resultado <- sort(outcomedata_result,na.last = TRUE)
  
    if (num=="best"){
      rank_list[i,] <- c((outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[1]]),states[i])
    } else if (num=="worst"){
      rank_list[i,] <- c((outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[length(order_outcome)]]),states[i])
    } else if (is.numeric(num)==TRUE) {
      rank_list[i,] <- c((outcomedata_result$Hospital.Name[order(outcomedata_filtrado,outcomedata_result$Hospital.Name)[num]]),states[i])
    } else{
      stop("invalid num")
    }
  }
  return(rank_list)
}