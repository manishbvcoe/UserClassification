library(data.table)

DB_PPPC <- read.csv("CSVFiles/DB_PPPC_Final.csv")
DB_PPPC <- DB_PPPC[c("IMSI", "Country","NMO","NMT")]

DB_PPC_Table <- data.table(DB_PPPC)

DB_PPC_feature <- as.data.frame(unique(DB_PPPC$IMSI))
setnames(DB_PPC_feature,c("unique(DB_PPPC$IMSI)"),c("IMSI"))

#number of called country
Temp <- DB_PPC_Table[NMO!=0 & Country!="Gabon" ,length(unique(Country)), by="IMSI"]
setnames(Temp,c("V1"),c("NCountry_Called"))
DB_PPC_feature <- merge(DB_PPC_feature, Temp,by="IMSI",all.x=T)
#Gabon_Users[,c(4)][is.na(Gabon_Users[,c(4)])] <- 0

#NMO out of country
Temp <- DB_PPC_Table[Country!="Gabon" ,sum(NMO), by="IMSI"]
setnames(Temp,c("V1"),c("NMO_NonDom_Called"))
DB_PPC_feature <- merge(DB_PPC_feature, Temp,by="IMSI",all.x=T)

#NMO in country
Temp <- DB_PPC_Table[Country=="Gabon" ,sum(NMO), by="IMSI"]
setnames(Temp,c("V1"),c("NMO_Dom_Called"))
DB_PPC_feature <- merge(DB_PPC_feature, Temp,by="IMSI",all.x=T)

#Nmo in france
Temp <- DB_PPC_Table[Country=="France" ,sum(NMO), by="IMSI"]
setnames(Temp,c("V1"),c("NMO_France_Called"))
DB_PPC_feature <- merge(DB_PPC_feature, Temp,by="IMSI",all.x=T)

DB_PPC_feature[,c(2,3,4,5)][is.na(DB_PPC_feature[,c(2,3,4,5)])] <- 0

#ratio of  NMO out side country and NMO country and
DB_PPC_feature$NMO_NonDombyDom <- DB_PPC_feature$NMO_NonDom_Called/DB_PPC_feature$NMO_Dom_Called

#ratio of NMO france and NMO in side country
DB_PPC_feature$NMO_FrbyDom <- DB_PPC_feature$NMO_France_Called/DB_PPC_feature$NMO_Dom_Called

DB_PPC_feature[,c(6)][is.infinite(DB_PPC_feature[,c(6)])] <- DB_PPC_feature[,c(3)][is.infinite(DB_PPC_feature[,c(6)])]
DB_PPC_feature[,c(7)][is.infinite(DB_PPC_feature[,c(7)])] <- DB_PPC_feature[,c(5)][is.infinite(DB_PPC_feature[,c(7)])]

#NMO NON France Abroad
Temp <- DB_PPC_Table[Country!="Gabon" & Country!="France" ,sum(NMO), by="IMSI"]
setnames(Temp,c("V1"),c("NMO_NonFranceAbroad"))
DB_PPC_feature <- merge(DB_PPC_feature, Temp,by="IMSI",all.x=T)