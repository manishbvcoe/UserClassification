library(data.table)
#Read MT and MO Data extracted from MSCCDR which is call logs of users from gabon to france made in GABON
Mt_Fr <- read.csv("CSVFiles/MtFr.csv",sep=";",header=F,na.strings= c( "NA", " ", ""),colClasses = c(rep("character" , 11)))
Mt_Fr <- na.omit(Mt_Fr)
colnames(Mt_Fr)<-(c("Type","FrNo","GabNo","IMSI","IMEI","Date","Time","Duration"))
# including 0 duration calls
#Mt_Fr <- Mt_Fr[Mt_Fr$Duration != 0 , ]


Mo_Fr <- read.csv("CSVFiles/MoFr.csv",sep=";",header=F,na.strings= c( "NA", " ", ""),colClasses = c(rep("character" , 12)))
Mo_Fr <- na.omit(Mo_Fr)
colnames(Mo_Fr)<-(c("Type","GabNo","IMSI","IMEI","FrNo","Date","Time","Duration"))

#Mo_Fr <- Mo_Fr[Mo_Fr$Duration != 0 , ]

BeginDate = "2013-07-31"
Mo_Fr$DaysFromBegining <- as.numeric(as.Date(Mo_Fr$Date , format = "%Y-%m-%d") - as.Date(BeginDate , format = "%Y-%m-%d"))
Mo_Fr$Week <- ceiling(Mo_Fr$DaysFromBegining/7)
Mo_Fr$weekday = weekdays(as.Date(Mo_Fr$Date , format = "%Y-%m-%d"))
Mo_Fr$hour = as.numeric(substr(Mo_Fr$Time , 1,2))
Mo_Fr$is_weekend <- Mo_Fr$weekday == "Sunday" | Mo_Fr$weekday == "Saturday"
Mo_Fr$is_FamilyHour <- Mo_Fr$hour >19 | Mo_Fr$hour <9
Mo_Fr$is_BusinessHour <- Mo_Fr$hour < 18 & Mo_Fr$hour  >= 9

Mo_Fr <- data.table(Mo_Fr)
Tmp1 <- Mo_Fr[ , length(Type) , by =list( IMSI,FrNo)]
Tmp1 <- as.data.frame(Tmp1[V1 > 1 , length(unique(FrNo)) , by = IMSI])
setnames(Tmp1,"V1","UniqFrMo")
Tmp2 <- Mo_Fr[ ,length(Type) , by = list(IMSI , FrNo)]
setnames(Tmp2,"V1","IMSINoFreq") 
Tmp3 <- as.data.frame(Mo_Fr[ , length(Type) , by = IMSI])
setnames(Tmp3,"V1","IMSIFreqMo")
Tmp4 <- Mo_Fr[ , length(Type) , by = list(IMSI , Week)]
setnames(Tmp4,"V1","MoIMSIWeek")
Tmp5 <- as.data.frame(Mo_Fr[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(Type) , by = IMSI])
setnames(Tmp5,"V1","MoBusinesstimes")

# per percentage of call greater than 120
Temp120Sec <- as.data.frame(Mo_Fr[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") & as.numeric(Duration)  >= 120 ,length(Type) , by = IMSI])
setnames(Temp120Sec,"V1","MoBusinesstimesatLeast2min")
Temp120Sec <- merge(Temp120Sec, Tmp5,by="IMSI",all.y=T)
Temp120Sec[2][is.na(Temp120Sec[2])] <- 0
Temp120Sec$PerCallGrtEq120sec <-  Temp120Sec$MoBusinesstimesatLeast2min/Temp120Sec$MoBusinesstimes
Temp120Sec <- Temp120Sec[,c("IMSI","PerCallGrtEq120sec")]


Tmp6 <- as.data.frame(Mo_Fr[is_FamilyHour =="TRUE" |weekday == "Sunday"  ,length(Type) , by = IMSI])
setnames(Tmp6,"V1","MoFamilytimes")
Tmp7 <- as.data.frame(Mo_Fr[ , median(as.numeric(Duration)) , by = IMSI])
setnames(Tmp7,"V1","MoMedianDuration")
Tmp8 <-as.data.frame(Mo_Fr[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,sum(as.numeric(Duration)) , by = IMSI])
setnames(Tmp8,"V1","MoBusinessDuration")
Tmp9 <- as.data.frame(Mo_Fr[is_FamilyHour =="TRUE" |weekday == "Sunday"  ,sum(as.numeric(Duration)) , by = IMSI])
setnames(Tmp9,"V1","MoFamilyDuration")
F_Mat_Mo <- Tmp3[Tmp3$IMSIFreqMo > 25,]
F_Mat_Mo <- merge(F_Mat_Mo, Tmp1 ,by ="IMSI" , all.x ="TRUE")
rm(Tmp1 , Tmp3)
F_Mat_Mo <- merge(F_Mat_Mo , Tmp5 , by = "IMSI" ,all.x ="TRUE")
F_Mat_Mo <- merge(F_Mat_Mo , Tmp6 , by = "IMSI" ,all.x ="TRUE")
F_Mat_Mo <- merge(F_Mat_Mo , Tmp8 , by = "IMSI" ,all.x ="TRUE")
F_Mat_Mo <- merge(F_Mat_Mo , Tmp9 , by = "IMSI" ,all.x ="TRUE")
rm(Tmp5,Tmp6,Tmp8,Tmp9)
Tmp4 <- Tmp4[ ,stdv_mo_week := sd(MoIMSIWeek) , by = IMSI]
Tmp4 <- Tmp4[ ,mean_mo_week := mean(MoIMSIWeek) , by = IMSI]
Tmp <- as.data.frame(Tmp4)
Tmp <- unique(Tmp[,c("IMSI","stdv_mo_week", "mean_mo_week")])
Tmp$coef_var_mo_week <- Tmp$stdv_mo_week/Tmp$mean_mo_week
Tmp <- unique(Tmp[,c(1,4)])
F_Mat_Mo <- merge(F_Mat_Mo , Tmp , by = "IMSI" ,all.x ="TRUE")
F_Mat_Mo <- merge(F_Mat_Mo , Tmp7 , by = "IMSI" ,all.x ="TRUE")
rm(Tmp7,Tmp4,Tmp)
setkey(Tmp2,IMSI,IMSINoFreq)
Tmp2 <- Tmp2[,CSum := cumsum(IMSINoFreq) , by = IMSI]
Tmp2 <- Tmp2[,Sum := sum(IMSINoFreq) , by = IMSI]	
Tmp2$CSumRatio <- Tmp2$CSum/Tmp2$Sum
Tmp2$is_CUG <- as.numeric(Tmp2$CSumRatio > .4 & Tmp2$IMSINoFreq > 2)
Tmp2 <-Tmp2[,sum(is_CUG),by = IMSI] 
setnames(Tmp2,"V1","CUGMO")
F_Mat_Mo <- merge(F_Mat_Mo , Tmp2 , by = "IMSI" , all.x = "TRUE")
rm(Tmp2)
Mo_Fr <- data.frame(Mo_Fr)
Mo_Fr$FrNoFormatted <- substring(Mo_Fr$FrNo , 3)
Tmp <- unique(Mo_Fr[,c("IMSI" , "FrNoFormatted")])
#MT Code
Mt_Fr$FrNoFormatted <- substring(Mt_Fr$FrNo , 3)
#Take only those numbers who have made atleast 1 call to france
Mt_Fr <- merge(Mt_Fr , Tmp , by = c("IMSI", "FrNoFormatted"))
Mt_Fr$DaysFromBegining <- as.numeric(as.Date(Mt_Fr$Date , format = "%Y-%m-%d") - as.Date(BeginDate , format = "%Y-%m-%d"))
Mt_Fr$Week <- ceiling(Mt_Fr$DaysFromBegining/7)
Mt_Fr$weekday = weekdays(as.Date(Mt_Fr$Date , format = "%Y-%m-%d"))
Mt_Fr$hour = as.numeric(substr(Mt_Fr$Time , 1,2))
Mt_Fr$is_weekend <- Mt_Fr$weekday == "Sunday" | Mt_Fr$weekday == "Saturday"
Mt_Fr$is_FamilyHour <- Mt_Fr$hour >19 | Mt_Fr$hour <9
Mt_Fr$is_BusinessHour <- Mt_Fr$hour < 18 & Mt_Fr$hour  >= 9
Mt_Fr <- data.table(Mt_Fr)
Tmp1 <- as.data.frame(Mt_Fr[ , length(unique(FrNo)) , by = IMSI])
setnames(Tmp1,"V1","UniqFrMt")
Tmp2 <- Mt_Fr[ ,length(Type) , by = list(IMSI , FrNo)]
setnames(Tmp2,"V1","IMSINoFreq") 
Tmp3 <- as.data.frame(Mt_Fr[ , length(Type) , by = IMSI])
setnames(Tmp3,"V1","IMSIFreqMt")
Tmp4 <- Mt_Fr[ , length(Type) , by = list(IMSI , Week)]
setnames(Tmp4,"V1","MtIMSIWeek")
Tmp5 <- as.data.frame(Mt_Fr[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,length(Type) , by = IMSI])
setnames(Tmp5,"V1","MtBusinesstimes")
Tmp6 <- as.data.frame(Mt_Fr[is_FamilyHour =="TRUE" |weekday == "Sunday"  ,length(Type) , by = IMSI])
setnames(Tmp6,"V1","MtFamilytimes")
Tmp7 <- as.data.frame(Mt_Fr[ , median(as.numeric(Duration)) , by = IMSI])
setnames(Tmp7,"V1","MtMedianDuration")
Tmp8 <-as.data.frame(Mt_Fr[is_BusinessHour =="TRUE" & (weekday != "Sunday" & weekday != "Saturday") ,sum(as.numeric(Duration)) , by = IMSI])
setnames(Tmp8,"V1","MtBusinessDuration")
Tmp9 <- as.data.frame(Mt_Fr[is_FamilyHour =="TRUE" |weekday == "Sunday"  ,sum(as.numeric(Duration)) , by = IMSI])
setnames(Tmp9,"V1","MtFamilyDuration")
F_Mat_Mt <- Tmp3[Tmp3$IMSIFreqMt > 1,]
F_Mat_Mt <- merge(F_Mat_Mt, Tmp1 ,by ="IMSI" , all.x ="TRUE")

F_Mat_Mt <- merge(F_Mat_Mt , Tmp5 , by = "IMSI" ,all.x ="TRUE")
F_Mat_Mt <- merge(F_Mat_Mt , Tmp6 , by = "IMSI" ,all.x ="TRUE")


Tmp4 <- Tmp4[ ,stdv_Mt_week := sd(MtIMSIWeek) , by = IMSI]
Tmp4 <- Tmp4[ ,mean_Mt_week := mean(MtIMSIWeek) , by = IMSI]
Tmp <- as.data.frame(Tmp4)
Tmp <- unique(Tmp[,c("IMSI","stdv_Mt_week", "mean_Mt_week")])

#Tmp[is.na(Tmp)] <- 0
Tmp$coef_var_Mt_week <- Tmp$stdv_Mt_week/Tmp$mean_Mt_week
Tmp <- unique(Tmp[,c(1,4)])
F_Mat_Mt <- merge(F_Mat_Mt , Tmp , by = "IMSI" ,all.x ="TRUE")

F_Mat_Mt <- merge(F_Mat_Mt , Tmp7 , by = "IMSI" ,all.x ="TRUE")


setkey(Tmp2,IMSI,IMSINoFreq)
Tmp2 <- Tmp2[,CSum := cumsum(IMSINoFreq) , by = IMSI]
Tmp2 <- Tmp2[,Sum := sum(IMSINoFreq) , by = IMSI]	
Tmp2$CSumRatio <- Tmp2$CSum/Tmp2$Sum
Tmp2$is_CUG <- as.numeric(Tmp2$CSumRatio > .4 & Tmp2$IMSINoFreq > 2)
Tmp2 <-Tmp2[,sum(is_CUG),by = IMSI] 
setnames(Tmp2,"V1","CUGMt")
F_Mat_Mt <- merge(F_Mat_Mt , Tmp2 , by = "IMSI" , all.x = "TRUE")
F_Mat_Mt <- merge(F_Mat_Mt , Tmp8 , by = "IMSI" , all.x = "TRUE")
F_Mat_Mt <- merge(F_Mat_Mt , Tmp9 , by = "IMSI" , all.x = "TRUE")
final_mat <- merge(F_Mat_Mo , F_Mat_Mt , by = "IMSI" , all.x = "TRUE")

rm(Tmp7,Tmp2,Tmp1,Tmp3,Tmp6,Tmp5,Tmp,Tmp4,Tmp8,Tmp9)
#don't convert NAs to anything
final_mat$BbyFMo <- final_mat$MoBusinesstimes/final_mat$MoFamilytimes
final_mat$BbyFMt <- final_mat$MtBusinesstimes/final_mat$MtFamilytimes

final_mat$BbyFMoDuration  <- final_mat$MoBusinessDuration/final_mat$MoFamilyDuration
final_mat$BbyFMtDuration <- final_mat$MtBusinessDuration/final_mat$MtFamilyDuration

final_mat$CUGbyAll <- final_mat$CUGMO/final_mat$UniqFrMo
final_mat$MobyMt <- final_mat$IMSIFreqMo/final_mat$IMSIFreqMt

#UNR
final_mat$UNR <- final_mat$UniqFrMo/final_mat$IMSIFreqMo

#BbyAllMt
final_mat$BbyAllMt <- final_mat$MtBusinesstimes/final_mat$IMSIFreqMt

#FbyAllMt
final_mat$FbyAllMt <- final_mat$MtFamilytimes/final_mat$IMSIFreqMt

#FbyAllMO
final_mat$FbyAllMO <- final_mat$MoFamilytimes/final_mat$IMSIFreqMo

final_mat<- merge(final_mat,Temp120Sec,by="IMSI",all.x=T)

#percentage of Zero duration call
Mo_Fr <- data.table(Mo_Fr)
Tmp <- Mo_Fr[Duration==0, length(Type) , by =IMSI]
setnames(Tmp,"V1","NMOZeroDuration")
Mt_Fr <- data.table(Mt_Fr)
Tmp1 <- Mt_Fr[ Duration==0, length(Type) , by =IMSI]
setnames(Tmp1,"V1","NMTZeroDuration")
TMP <- merge(Tmp, Tmp1,by="IMSI",All.x=T,All.y=T)
TMP[,c(2,3)][is.na(TMP[,c(2,3)])] <- 0
TMP$NoOfZeroDurationCall=TMP$NMOZeroDuration+ TMP$NMTZeroDuration
TMP <- as.data.frame(TMP)
TMP <- TMP[,c(1,4)]
final_mat<- merge(final_mat, TMP,by="IMSI",all.x=T)
final_mat[, c(31)][is.na(final_mat[, c(31)])] <- 0
final_mat$PersOfZeroDurationCall <- final_mat$NoOfZeroDurationCall/(final_mat$IMSIFreqMo+final_mat$IMSIFreqMt)

rm(BeginDate , F_Mat_Mo , F_Mat_Mt , Mo_Fr , Mt_Fr )






