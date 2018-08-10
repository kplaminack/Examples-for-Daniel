
complete <- function(directory,id=1:332) {

  path <- getwd()
files_full <- list.files(path,full.names=TRUE)

dat<-data.frame()
output<-c()
for (i in id) {
dat<-read.csv(files_full[i])
var1<-i
var2<-sum(complete.cases(dat))
df<-data.frame("id"=var1,"nobs"=var2) 
output<-rbind(output,df)  


  }
output
 }