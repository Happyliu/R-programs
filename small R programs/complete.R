complete<-function(directory,id=1:332){
  dir<-directory
  Id<-list()
  Nobs<-list()
  for(i in id){
    i1<-formatC(i,width=3,format="d",flag="0")
    i2<-toString(i1)
    print(i2)
    str1="/Users/liuzhao/Documents/R/assignments/"
    str0="/"
    str2=".csv"
    str3=paste(str1,dir,str0,i2,str2,sep="")
    x<-read.csv(file=str3,header=T)
    z<-x[,2]
    test<-z[!is.na(z)]
    y<-length(test)
    print(y)
   Id<-append(Id,i)
   Nobs<-append(Nobs,y)
  }
  x3<-cbind(Id,Nobs)
  print(x3)
}