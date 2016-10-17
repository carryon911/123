#!!!!??è??!!!!!!
#è«‹å?ˆè¨­å®šå·¥ä½œç›®???(setwd)
rm(list = ls())
load("futxts.rdata")
decision<-read.table("playground.csv",sep=",",
                     header = TRUE,stringsAsFactors = FALSE)
#çµç?—æ—¥?•¶å¤©éƒ¨ä½å¼·?ˆ¶çµæ??
#?‰¾?‡ºç¶“æ­·??„ç?ç?—æ—¥
library(quantmod)
begin<-as.Date(decision$TradingDate[1])
end<-as.Date(decision$TradingDate[nrow(decision)])
selected_futxts<-futxts[paste0(begin,"/",end)]

cdtemp<-data.frame(time(selected_futxts),coredata(selected_futxts),stringsAsFactors = FALSE)
names(cdtemp)<-names(decision)
cdtemp$TradingDate<-as.character(cdtemp$TradingDate)

selindex<-logical(nrow(cdtemp))
for (i in 1:(nrow(cdtemp)-1)){
  if(cdtemp[i,"TradingDate"] == cdtemp[i+1,"TradingDate"]){
    selindex[i]<-TRUE    
  }
}

cdtemp<-cdtemp[selindex,]
cdtemp$Position<-0
cdtemp$PositionQ<-0
report<-rbind(cdtemp,decision)
report["TradingDate"]<-as.Date(unlist(report["TradingDate"]))
rm(cdtemp,decision,futxts,selected_futxts,begin,end,selindex,i)
#??ˆä½µ??’å?ï?Œç?ç?—æ—¥?•¶å¤©æ?ƒæ?‰å…©ç­†è?‡æ?™ï?Œç?æ?…è?‡æ?™å?—è?åœ¨?–°è¿‘æ?Ÿæ?Ÿè²¨???

Orderbydate <- order(report$TradingDate)
report<-report[Orderbydate,]
rm(Orderbydate)


#å¢å? ä¸¦ä¸”è?ˆç?—è?ç?Ÿæ?„ä?TBasis,ABasis,Income,cumIncome,Lmax,Lmin,Smax,Smin
for(i in 3:ncol(report)){
  report[,i]<-as.numeric(unlist(report[,i]))
}
#TBasis,ABasis
TBasis<-report["C2HL"]*report["PositionQ"]
names(TBasis)<-"TBasis"
for(i in 2: nrow(report)){
  if(report[i,"Position"]==report[i-1,"Position"] && 
     report[i,"PositionQ"]==report[i-1,"PositionQ"]){
    TBasis[i,"TBasis"]<-TBasis[i-1,"TBasis"]  
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] < report[i-1,"PositionQ"]
           ){
    TBasis[i,"TBasis"]<-TBasis[i,"TBasis"]*report[i,"PositionQ"]/report[i-1,"PositionQ"]
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] > report[i-1,"PositionQ"]){
    TBasis[i,"TBasis"]<-TBasis[i-1,"TBasis"]+report[i,"C2HL"]*(report[i,"PositionQ"]-report[i-1,"PositionQ"])
  }else if(report[i,"Position"] != report[i-1,"Position"]){
    TBasis[i,"TBasis"]<-report[i,"C2HL"]*report[i,"PositionQ"]
  }
}
ABasis<-TBasis[,"TBasis"]/report[,"PositionQ"]
ABasis[is.nan(ABasis)]<-0
report<-cbind(report,TBasis,ABasis)
rm(TBasis,ABasis)

#Income
Income<-numeric(NROW(report))
names(Income)<-"Income"
for(i in 2:nrow(report)){
  if(report[i,"Position"]==report[i-1,"Position"] && 
     report[i,"PositionQ"]==report[i-1,"PositionQ"]){
    Income[i]<-0
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] < report[i-1,"PositionQ"]
  ){
    Income[i]<-(report[i,"C2HL"]-report[i-1,"ABasis"])*(report[i,"PositionQ"] - report[i-1,"PositionQ"])*report[i,"Position"]
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] > report[i-1,"PositionQ"]){
    Income[i]<-0
  }else if((report[i,"Position"]==1 && report[i-1,"Position"]==-1)|
           (report[i,"Position"]==-1 && report[i-1,"Position"]==1)){
    Income[i]<-(report[i,"C2HL"]-report[i-1,"ABasis"])*report[i-1,"PositionQ"]*report[i-1,"Position"]
  }else if((report[i,"Position"]==0 && report[i-1,"Position"]==1)|
           (report[i,"Position"]==0 && report[i-1,"Position"]==-1)){
    Income[i]<-(report[i,"C2HL"]-report[i-1,"ABasis"])*report[i-1,"PositionQ"]*report[i-1,"Position"]
}
}
acumIncome <- Income
for (i in 2:nrow(report)) {
  acumIncome[i]<-acumIncome[i-1]+Income[i]  
}

report<-cbind(report,Income,acumIncome)
rm(Income,acumIncome)

#Lmax,Lmin,Smax,Smin
Lmax<-numeric(NROW(report))
Lmin<-numeric(NROW(report))
Smax<-numeric(NROW(report))
Smin<-numeric(NROW(report))
Lmax[1]<-NA
Lmin[1]<-NA
Smax[1]<-NA
Smin[1]<-NA
for (i in 2:NROW(report)) {
  Lmax[i] <- (report[i,"High"]-report[i-1,"ABasis"])*(report[i-1,"PositionQ"])
  Lmin[i] <- (report[i,"Low"]-report[i-1,"ABasis"])*(report[i-1,"PositionQ"]) 
  Smax[i] <- (report[i-1,"ABasis"]-report[i,"Low"])*(report[i-1,"PositionQ"])
  Smin[i] <- (report[i-1,"ABasis"]-report[i,"High"])*(report[i-1,"PositionQ"]) 
}
report<-cbind(report,Lmax,Lmin,Smax,Smin)
rm(Lmax,Lmin,Smax,Smin)
#è¼¸å‡º? ±è¡¨report.csv
write.table(report,file = "report(C2HL).csv",sep = ",",row.names = FALSE)