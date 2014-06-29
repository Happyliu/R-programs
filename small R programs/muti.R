 muti<-function(v,w){
   pri<-list()
   prj<-list()
   for (i in v){
     i1<-i*4806
     pri<-append(pri,i1)
     }
   for (j in w){
     j1<-j*435
     prj<append(prj,j1)
   }
 x1<-cbind(pri,prj)
   print(x1)
 }