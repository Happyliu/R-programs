rankhospital<-function(state,outcome,num){
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
      if(num=='worst'){
        min=min(as.numeric(State[,25]),na.rm=TRUE)
        stamin=State[State[,25]==min,]
        result=stamin[,47]
        print(result)
      }else if(num=='best'){
        max=max(as.numeric(State[,25]),na.rm=TRUE)
        stamin=State[State[,25]==max,]
        result=stamin[,47]
        print(result)
      }else{
        stamin=order(State[,25],na.last=TRUE)
        st=State[stamin,]
        a=st[,2]
        if(num<=length(a)){
        a[1:num]
        }else{
          print(NA)
        }
      }
  }else if(outcome=='heart attack'){
    State=out.hospital[out.hospital[,7]==state,]
    if(num=='worst'){
      min=min(as.numeric(State[,31]),na.rm=TRUE)
      stamin=State[State[,25]==min,]
      result=stamin[,47]
      print(result)
    }else if(num=='best'){
      max=max(as.numeric(State[,31]),na.rm=TRUE)
      stamin=State[State[,31]==max,]
      result=stamin[,47]
      print(result)
    }else{
      stamin=order(State[,31],na.last=TRUE)
      st=State[stamin,]
      a=st[,2]
      if(num<=length(a)){
        a[1:num]
      }else{
        print(NA)
      }
    }
  }else if(outcome=='heart failure'){
    State=out.hospital[out.hospital[,7]==state,]
    if(num=='worst'){
      min=min(as.numeric(State[,37]),na.rm=TRUE)
      stamin=State[State[,37]==min,]
      result=stamin[,47]
      print(result)
    }else if(num=='best'){
      max=max(as.numeric(State[,37]),na.rm=TRUE)
      stamin=State[State[,37]==max,]
      result=stamin[,47]
      print(result)
    }else{
      stamin=order(State[,37],na.last=TRUE)
      st=State[stamin,]
      a=st[,2]
      if(num<=length(a)){
        a[1:num]
      }else{
        print(NA)
      }
    }
  }else{
    print("invalid outcome")
  }
}