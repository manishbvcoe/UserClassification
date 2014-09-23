library(stringi)
library(data.table)
library(hash)
library(date)

TAPIN<-read.csv("CSVFiles/TAPINSepToDec.csv",sep=";",header=FALSE,colClasses=c(rep("character",9),"numeric",rep("character",10)))

## Replace Country code by country name in the TAPIN records
TADIGCodes<-read.csv("CSVFiles/ListTADIGCodes.csv",header=TRUE,strip.white=TRUE)
TADIGCodes<-TADIGCodes[,c(2,4)]
TADIGCodes$CODE<-as.character(TADIGCodes$CODE)
TADIGCodes<-TADIGCodes[TADIGCodes$CODE %in% unique(TAPIN$V2),]
h<-hash(keys=TADIGCodes$CODE,values=TADIGCodes$COUNTRIES)
i=1
Temp<-TAPIN$V2
for (code in TADIGCodes$CODE)
{
print(i)
Temp[Temp==code]<-as.character(values(h,keys=code))
i<-i+1
}
TAPIN$V2 <- Temp
rm(code,h,i,TADIGCodes,Temp)


## 
TAPIN<-TAPIN[,c(1,2,5,7,8,9,10,11,12,13,14,15)]
#Remove the August entries from TAPIN
temp=which(substr(TAPIN[,3], 5,6)=="08")
TAPIN=TAPIN[-temp,]

TAPIN= TAPIN[TAPIN$V2=="FRANCE", ]

setnames(TAPIN,c("V1","V2","V5","V7","V8","V9","V10"),c("EventType","Country","TimeStamp","IMSI","MSISDN","IMEI","Duration"))

TravelledUser <- unique(TAPIN$IMSI) 

TAPIN_MO <- TAPIN[TAPIN$EventType=="mo",]
TAPIN_MT <- TAPIN[TAPIN$EventType=="mt",]
#TAPIN_MO <- TAPIN_MO[TAPIN_MO$Duration != 0 , ]
#TAPIN_MT <- TAPIN_MT[TAPIN_MT$Duration != 0 , ]


#number of called 
TAPIN_MO_Feature <- as.data.frame(table(TAPIN_MO$IMSI))
colnames(TAPIN_MO_Feature) <- c("IMSI","NMO_TAPIN")



#number of call perday 
Temp <- as.data.table(TAPIN)
TempNOOfDay <- Temp[,length(unique(substr(TimeStamp,1,8))),by=IMSI]
setnames(TempNOOfDay,"V1","NOFDay_TAPIN")



#Temp <- unique(TAPIN_MO[,c("IMSI" , "V11")])
#TAPIN_MT <- merge(TAPIN_MT , Temp , by = c("IMSI", "V11"))


#number of call taken
TAPIN_MT_Feature <- as.data.frame(table(TAPIN_MT$IMSI))
colnames(TAPIN_MT_Feature) <- c("IMSI","NMT_TAPIN")


#sum of duration of MO call
Temp<- TAPIN_MO[c("IMSI","Duration")]
Temp$Duration <- as.numeric(Temp$Duration)
Temp <- data.table(Temp)
Temp <- Temp[, sum(Duration,na.rm=TRUE),by= IMSI]
setnames(Temp,"V1","SumOfDuration_MO_TAPIN")
TAPIN_MO_Feature <- merge(TAPIN_MO_Feature,Temp,by="IMSI",all.x ="TRUE")


#sum of duration of MT call
Temp<- TAPIN_MT[c("IMSI","Duration")]
Temp$Duration <- as.numeric(Temp$Duration)
Temp <- data.table(Temp)
Temp <- Temp[, sum(Duration,na.rm=TRUE),by= IMSI]
setnames(Temp,"V1","SumOfDuration_MT_TAPIN")
TAPIN_MT_Feature <- merge(TAPIN_MT_Feature,Temp,by="IMSI",all.x ="TRUE")

# median of call duration IMSI wise    
Temp<- TAPIN_MO[c("IMSI","Duration")]
Temp$Duration <- as.numeric(Temp$Duration)
Temp <- data.table(Temp)
Temp <- Temp[, median(Duration,na.rm=TRUE),by= IMSI]
setnames(Temp,"V1","medianOfDuration_MO_TAPIN")
TAPIN_MO_Feature <- merge(TAPIN_MO_Feature,Temp,by="IMSI",all.x ="TRUE")

        
         
TAPIN_MO_Fr <- TAPIN_MO[substr(TAPIN_MO$V11,1,2)=="33",]  
TAPIN_MT_Fr <- TAPIN_MT[substr(TAPIN_MT$V11,1,2)=="33",] 
#TAPIN_MO_Dom <- TAPIN_MO[(substr(TAPIN_MO$V11,4,5)=="04")|(substr(TAPIN_MO$V11,4,5)=="07"),]
TAPIN_MO_Dom <- TAPIN_MO[(substr(TAPIN_MO$V11,1,3)=="241"),]


# number of unique number called in france
TempDataTable <- data.table(TAPIN_MO_Fr[c("EventType","IMSI","V11")])
setnames(TempDataTable,"V11","Called_NO")
Temp <- as.data.frame(TempDataTable[,length(unique(Called_NO)),by = list(IMSI)]) 
setnames(Temp,"V1","UniNOCalled_MO_fr_TAPIN")
TAPIN_MO_Feature <- merge(TAPIN_MO_Feature,Temp,by="IMSI",all.x ="TRUE")


# number of unique number called in Domestic
TempDataTable <- data.table(TAPIN_MO_Dom[c("EventType","IMSI","V11")])
setnames(TempDataTable,"V11","Called_NO")
Temp <- as.data.frame(TempDataTable[,length(unique(Called_NO)),by = list(IMSI)]) 
setnames(Temp,"V1","UniNOCalled_MO_Dom_TAPIN")
TAPIN_MO_Feature <- merge(TAPIN_MO_Feature,Temp,by="IMSI",all.x ="TRUE")

#french no in office hour by total call to french no  
Temp = data.table(TAPIN_MO_Fr$IMSI, substr(TAPIN_MO_Fr$TimeStamp, 1, 8),substr(TAPIN_MO_Fr$TimeStamp, 9, 14))
Temp= setnames(Temp,c("V1","V2","V3"),c("IMSI", "Date","Time"))
Temp=cbind(Temp, substr(Temp$Date, 1, 4), substr(Temp$Date, 5, 6), substr(Temp$Date, 7, 8))
setnames(Temp,c("V2","V3","V4"),c("Year", "Month", "Date"))
Temp=as.data.frame(Temp)
Temp=Temp[, -c(2)]
Temp=cbind(Temp, paste(Temp[, "Year"], Temp[, "Month"], Temp[, "Date"], sep="-"))
colnames(Temp)=c("IMSI", "Time","Year", "Month", "Date", "FullDate")
Temp = Temp[, -c(3, 4, 5)]
Temp$weekday = weekdays(as.Date(Temp$FullDate , format = "%Y-%m-%d"))
Temp$hour = as.numeric(substr(Temp$Time , 1,2))
Temp$is_weekend <- Temp$weekday == "Sunday" | Temp$weekday == "Saturday"
Temp$is_FamilyHour <- Temp$hour >19 | Temp$hour <9
Temp$is_BusinessHour <- Temp$hour < 18 & Temp$hour  >= 9
Temp<- as.data.table(Temp)

Tmp5 <- as.data.frame(Temp[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(Time) , by = IMSI])
setnames(Tmp5,"V1","Businesstimes")
Tmp6 <- as.data.frame(Temp[,length(Time) , by = IMSI])
setnames(Tmp6,"V1","Alltimes")

Temp <- merge(Tmp5 , Tmp6 , by = "IMSI" ,all.x ="TRUE")
Temp$BbyAllMo_Fr_TAPIN <- Temp$Businesstimes/Temp$Alltimes
Temp <-Temp[c("IMSI","BbyAllMo_Fr_TAPIN")]
TAPIN_MO_Feature <- merge(TAPIN_MO_Feature,Temp,by="IMSI",all.x ="TRUE")
#All<-merge(TAPIN_MO_Feature,TAPIN_MT_Feature,by="IMSI",all.x=T,all.y=T)

#NMO/NMT
TAPIN_Feature <- merge(TAPIN_MO_Feature,TAPIN_MT_Feature,by="IMSI",all.x=T,all.y=T)
TAPIN_Feature[,c(2,3,5,6,8,9)][is.na(TAPIN_Feature[,c(2,3,5,6,8,9)])] <- 0
TAPIN_Feature$NMOByNMT_TAPIN <- TAPIN_Feature$NMO_TAPIN/TAPIN_Feature$NMT_TAPIN

#TAPIN_Feature[,c(10)][is.infinite(TAPIN_Feature[,c(10)])] <- TAPIN_Feature$NMO_TAPIN
TAPIN_Feature[,c(10)][is.infinite(TAPIN_Feature[,c(10)])] <-TAPIN_Feature[,c(2)][is.infinite(TAPIN_Feature[,c(10)])]
TAPIN_Feature <- merge(TAPIN_Feature,TempNOOfDay,by="IMSI",all.x=T)
TAPIN_Feature$MOperDay_TAPIN<- TAPIN_Feature$NMO_TAPIN/TAPIN_Feature$NOFDay_TAPIN
TAPIN_Feature = TAPIN_Feature[, -c(11)]
#rm(TAPIN_MO_Feature,TAPIN_MT_Feature, TAPIN_MO, TAPIN_MT)


#UniqNocalled by Total number called
Temp <- as.data.table(TAPIN_MO)
TempUniqN <- Temp[,length(unique(V11)),by=IMSI]
setnames(TempUniqN,"V1","UniNcalled_TAPIN")
Temp <- TAPIN_Feature[,c("IMSI", "NMO_TAPIN")]
Temp <- merge(Temp,TempUniqN,by="IMSI")
Temp$UNR_TAPIN <- Temp$UniNcalled_TAPIN/Temp$NMO_TAPIN 
Temp <- Temp[c("IMSI","UNR_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)


#UniqNocalled by Total number called in Business Hour 
Temp = data.table(TAPIN_MO$IMSI, TAPIN_MO$V11, substr(TAPIN_MO$TimeStamp, 1, 8),substr(TAPIN_MO$TimeStamp, 9, 14))
Temp= setnames(Temp,c("V1","V2","V3","V4"),c("IMSI","CalledNO", "Date","Time"))
Temp=cbind(Temp, substr(Temp$Date, 1, 4), substr(Temp$Date, 5, 6), substr(Temp$Date, 7, 8))
setnames(Temp,c("V2","V3","V4"),c("Year", "Month", "Date"))
Temp=as.data.frame(Temp)
Temp=Temp[, -c(3)]
Temp=cbind(Temp, paste(Temp[, "Year"], Temp[, "Month"], Temp[, "Date"], sep="-"))
colnames(Temp)=c("IMSI", "CalledNO","Time","Year", "Month", "Date", "FullDate")
Temp = Temp[, -c(4, 5,6)]
Temp$weekday = weekdays(as.Date(Temp$FullDate , format = "%Y-%m-%d"))
Temp$hour = as.numeric(substr(Temp$Time , 1,2))
Temp$is_weekend <- Temp$weekday == "Sunday" | Temp$weekday == "Saturday"
Temp$is_FamilyHour <- Temp$hour >19 | Temp$hour <9
Temp$is_BusinessHour <- Temp$hour < 18 & Temp$hour  >= 9

Temp<- as.data.table(Temp)
Tmp5 <- as.data.frame(Temp[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(Time) , by = IMSI])
setnames(Tmp5,"V1","NOfCallInBHour")
Tmp6 <- as.data.frame(Temp[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(unique(CalledNO)) , by = IMSI])
setnames(Tmp6,"V1","UniqNumberCalledInBHour")

#FbyAllNMO
Temp7 <- as.data.frame(Temp[is_FamilyHour =="TRUE" | (weekday == "Sunday" ) ,length(Time) ,by =IMSI])
setnames(Temp7,"V1","NOfCallInFHour")
Temp <- TAPIN_Feature[,c("IMSI", "NMO_TAPIN")]
Temp7 <- merge(Temp, Temp7,by="IMSI")
Temp7$FbyAllNMO_TAPIN <-Temp7$NOfCallInFHour/Temp7$NMO_TAPIN  
Temp7 <- Temp7[c("IMSI","FbyAllNMO_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp7,by="IMSI",all.x=T)

Temp <- merge(Tmp5, Tmp6,by="IMSI")
Temp$UNRB_TAPIN <- Temp$UniqNumberCalledInBHour/Temp$NOfCallInBHour
Temp <- Temp[c("IMSI","UNRB_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)



# BbyAllMt 
Temp = data.table(TAPIN_MT$IMSI, TAPIN_MT$V11, substr(TAPIN_MT$TimeStamp, 1, 8),substr(TAPIN_MT$TimeStamp, 9, 14))
Temp= setnames(Temp,c("V1","V2","V3","V4"),c("IMSI","CallerNO", "Date","Time"))
Temp=cbind(Temp, substr(Temp$Date, 1, 4), substr(Temp$Date, 5, 6), substr(Temp$Date, 7, 8))
setnames(Temp,c("V2","V3","V4"),c("Year", "Month", "Date"))
Temp=as.data.frame(Temp)
Temp=Temp[, -c(3)]
Temp=cbind(Temp, paste(Temp[, "Year"], Temp[, "Month"], Temp[, "Date"], sep="-"))
colnames(Temp)=c("IMSI", "CallerNO","Time","Year", "Month", "Date", "FullDate")
Temp = Temp[, -c(4, 5,6)]
Temp$weekday = weekdays(as.Date(Temp$FullDate , format = "%Y-%m-%d"))
Temp$hour = as.numeric(substr(Temp$Time , 1,2))
Temp$is_weekend <- Temp$weekday == "Sunday" | Temp$weekday == "Saturday"
Temp$is_FamilyHour <- Temp$hour >19 | Temp$hour <9
Temp$is_BusinessHour <- Temp$hour < 18 & Temp$hour  >= 9

Temp<- as.data.table(Temp)
Tmp5 <- as.data.frame(Temp[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(Time) , by = IMSI])
setnames(Tmp5,"V1","NOfCallInBHour")

Temp <- TAPIN_Feature[,c("IMSI", "NMT_TAPIN")]
Temp <- merge(Temp, Tmp5,by="IMSI")
Temp$BbyAllMt_TAPIN <- Temp$NOfCallInBHour/Temp$NMT_TAPIN 
Temp <- Temp[c("IMSI","BbyAllMt_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)

#Median Duration France
Temp <- data.table(TAPIN_MO_Fr)
Temp1 <- Temp[,median(Duration,na.rm=T),by=IMSI]
setnames(Temp1,"V1","MedianDuration_Fr_TAPIN")
TAPIN_Feature <- merge(TAPIN_Feature,Temp1,by="IMSI",all.x=T)

#Median Duration Dom
Temp <- data.table(TAPIN_MO_Dom)
Temp1 <- Temp[,median(Duration,na.rm=T),by=IMSI]
setnames(Temp1,"V1","MedianDuration_Dom_TAPIN")
TAPIN_Feature <- merge(TAPIN_Feature,Temp1,by="IMSI",all.x=T)


#UNR Fr
Temp <- as.data.table(TAPIN_MO_Fr)
TempUniqN <- Temp[,length(unique(V11)),by=IMSI]
setnames(TempUniqN,"V1","UniNcalled_Fr_TAPIN")
Temp<- as.data.frame(table(TAPIN_MO_Fr$IMSI))
colnames(Temp) <- c("IMSI","NMO_Fr_TAPIN")
Temp <- merge(Temp,TempUniqN,by="IMSI")
Temp$UNR_Fr_TAPIN <- Temp$UniNcalled_Fr_TAPIN/Temp$NMO_Fr_TAPIN 
Temp <- Temp[c("IMSI","UNR_Fr_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)


#NMO_Fr_perDay
Temp<- as.data.frame(table(TAPIN_MO_Fr$IMSI))
colnames(Temp) <- c("IMSI","NMO_Fr_TAPIN")
Temp <- merge(Temp,TempNOOfDay,by="IMSI",all.y=T)
Temp[,c(2)] [is.na(Temp[,(2)])] <- 0
Temp$NMO_FrPerDay_TAPIN <- Temp$NMO_Fr_TAPIN/Temp$NOFDay_TAPIN
Temp <- Temp[c("IMSI","NMO_FrPerDay_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)



#NMOFr/NMTFr
Temp<- as.data.frame(table(TAPIN_MO_Fr$IMSI))
colnames(Temp) <- c("IMSI","NMO_Fr_TAPIN")
Temp1<- as.data.frame(table(TAPIN_MT_Fr$IMSI))
colnames(Temp1) <- c("IMSI","NMT_Fr_TAPIN")
Temp <- merge(Temp, Temp1,by="IMSI",all.y=T,all.x=T)
Temp[,c(2)] [is.na(Temp[,(2)])] <- 0
Temp[,c(3)] [is.na(Temp[,(3)])] <- 0
Temp$NMO_FrbyNMTFr_TAPIN <- Temp$NMO_Fr_TAPIN/Temp$NMT_Fr_TAPIN
Temp[,c(4)] [is.infinite(Temp[,(4)])] <- Temp[,c(2)] [is.infinite(Temp[,(4)])]
Temp <- Temp[c("IMSI","NMO_FrbyNMTFr_TAPIN")]
TAPIN_Feature <- merge(TAPIN_Feature,Temp,by="IMSI",all.x=T)


#Number of Domestic call Duration Dom
Temp <- data.table(TAPIN_MO_Dom)
Temp1 <- Temp[,length(EventType),by=IMSI]
setnames(Temp1,"V1","NMO_Dom_TAPIN")
TAPIN_Feature <- merge(TAPIN_Feature,Temp1,by="IMSI",all.x=T)

