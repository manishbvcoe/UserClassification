library(data.table)


source("MSCCDR_features.r")
source("feature_extraction_TAPIN.r")
print("number of user call greater >=25 extracted user")
dim(final_mat)

print("Number of travelled user to france")
length(TravelledUser)


print("Number of users having MO call >=25 and Travel to France")
dim(TAPIN_Feature)

TargetUser <- merge(final_mat,TAPIN_Feature,by="IMSI")
print(dim(TargetUser))

#reading travel duration and number of trip and travel days of users
TravelHistory <- read.csv("CSVFiles/userTravelInfo.csv")
TravelHistory <- TravelHistory[,-c(1)]
print("users having detail travel history")
dim(TravelHistory)
TargetUser <- merge(TargetUser, TravelHistory,by="IMSI",all.x=T)

#rading data from DB_PPC
source("feature_extraction _DBPPC.r")
TargetUser <- merge(TargetUser, DB_PPC_feature,by="IMSI",all.x=T)

MO_GABON <- read.csv("CSVFiles/MO_GABON",sep=";",header=F,na.strings= c( "NA", " ", ""),colClasses = c(rep("character" , 12)))
colnames(MO_GABON)<-(c("Type","GabNo","IMSI","IMEI","FrNo","Date","Time","Duration"))
Temp <- data.table(MO_GABON)
Temp1 <- Temp[,length(unique(Date)),by="IMSI"]
setnames(Temp1,"V1","NoOfDaysMSCCDR")
TargetUser <- merge(TargetUser,Temp1,by="IMSI",all.x=T)
TargetUser$MOFreqPerDay <- TargetUser$IMSIFreqMo/TargetUser$NoOfDaysMSCCDR



