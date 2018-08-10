rankall <- function(outcome,num="best") {

data<-read.csv("outcome-of-care-measures.csv",
 na.strings="Not Available", stringsAsFactors=FALSE)

library(plyr)

mydata<-data.frame(data$Hospital.Name, data$State, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
names(mydata) <- c("hospital", "State", "DRheartattack", "DRheartfailure","DRpneumonia")


if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {


if(outcome=="heart attack")                                                            {
s<-data.frame(mydata$hospital, mydata$State, mydata$DRheartattack)
names(s)<-c("hospital", "State", "DRheartattack")

arrangedData<-arrange(s,State,DRheartattack,hospital)
completeData<-na.omit(arrangedData)
splitData<-split(completeData,completeData$State)

if (num=="best")                                                 {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[1]                                                      })
                                                                  }
else if (num=="worst")                                           {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[nrow(splitData)]                              })
                                                                           }
else
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[num]                                                    })

unlisted_values<-unlist(results)
statename<-names(results)

output<-data.frame(hospital=unlisted_values,state=statename,row.names=statename)
                                                                                        }

else if(outcome=="heart failure")                                                       {
s<-data.frame(mydata$hospital, mydata$State, mydata$DRheartfail)
names(s)<-c("hospital", "State", "DRheartfail")

arrangedData<-arrange(s,State,DRheartfail,hospital)
completeData<-na.omit(arrangedData)
splitData<-split(completeData,completeData$State)

if (num=="best")                                                 {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[1]                                                      })
                                                                  }
else if (num=="worst")                                           {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[nrow(splitData)]                              })
                                                                           }
else
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[num]                                                    })

unlisted_values<-unlist(results)
statename<-names(results)

output<-data.frame(hospital=unlisted_values,state=statename,row.names=statename)
                                                                                        }

else 
s<-data.frame(mydata$hospital, mydata$State, mydata$DRpneumonia)
names(s)<-c("hospital", "State", "DRpneumonia")

arrangedData<-arrange(s,State,DRpneumonia,hospital)
completeData<-na.omit(arrangedData)
splitData<-split(completeData,completeData$State)

if (num=="best")                                                 {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[1]                                                      })
                                                                  }
else if (num=="worst")                                           {
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[nrow(splitData)]                              })
                                                                           }
else
results<-lapply(splitData,function(splitData)                              {
splitData$hospital[num]                                                    })

unlisted_values<-unlist(results)
statename<-names(results)

output<-data.frame(hospital=unlisted_values,state=statename,row.names=statename)
                                                                                        }
}

