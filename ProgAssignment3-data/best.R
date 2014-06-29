best<-function(state,outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  print(outcome)
  out<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital<-read.csv("hospital-data.csv", colClasses = "character")
  out.hospital<-merge(out, hospital, by = "Provider.Number")
  if(outcome=='pneumonia'){
  State=out.hospital[out.hospital[,7]==state,]
  min=min(as.numeric(State[,25]),na.rm=TRUE)
  stamin=State[State[,25]==min,]
  result=stamin[,47]
  print(result)
}else if(outcome=='heart attack'){
  State=out.hospital[out.hospital[,7]==state,]
  min=min(as.numeric(State[,31]),na.rm=TRUE)
  stamin=State[State[,31]==min,]
  result=stamin[,47]
  print(result)
}else if(outcome=='heart failure'){
  State=out.hospital[out.hospital[,7]==state,]
  min=min(as.numeric(State[,37]),na.rm=TRUE)
  stamin=State[State[,37]==min,]
  result=stamin[,47]
  print(result)
}else{
  print("invalid outcome")
}
}
