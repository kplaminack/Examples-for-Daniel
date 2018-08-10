corr <- function(directory,threshold=0) {

path <- getwd()
result<-c()
for(i in 1:332) {
 if (i < 10) {
      data1 <- read.csv(paste(path,"/00", i,".csv", sep = ""), header = TRUE)
    }
    else if (i < 100) {
      data1 <- read.csv(paste(path,"/0", i,".csv", sep = ""), header = TRUE)

    }
    else {
      data1 <- read.csv(paste(path,"/", i,".csv", sep = ""), header = TRUE)

    }
completedata<-data1[complete.cases(data1),]
if(nrow(completedata)>threshold){
result<-c(result,cor(completedata[,"sulfate"],completedata["nitrate"]))
}
}
return(result)
}


cr <- corr("specdata", 150)
head(cr,6)







