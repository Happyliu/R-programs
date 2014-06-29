getmonitor<-function(id,directory,summary=FALSE){
  a<-formatC(id, width = 3, format = "d", flag = "0") 
  c<-toString(a)
  print(c)
  b<-directory
  sum<-summary
  print(b)
  str1="/Users/liuzhao/Documents/R/assignments/"
  str0="/"
  str2=".csv"
  str3=paste(str1,b,str0,c,str2,sep="")
  if(sum==1){
  x<-read.csv(file=str3,header=T)
  summary(x)
  }else{
    read.csv(file=str3,nrows=6)
  }
}