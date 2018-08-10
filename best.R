best <- function(state, outcome) {

data<-read.csv("outcome-of-care-measures.csv",
 na.strings="Not Available", stringsAsFactors=FALSE)

library(plyr)

mydata<-data.frame(data$Hospital.Name, data$State, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
names(mydata) <- c("hospital", "State", "DRheartattack", "DRheartfailure","DRpneumonia")

if(!state %in% mydata[, "State"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {

statedata<-subset(mydata, State == state)



if(outcome=="heart attack"){

arrangedData <- arrange(statedata,DRheartattack,hospital)}

else if (outcome=="heart failure"){

arrangedData <- arrange(statedata,DRheartfailure,hospital)}

else {arrangedData <- arrange(statedata,DRpneumonia,hospital)}
return(arrangedData[1,1])
}}
