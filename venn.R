Temp <- TargetUser[(!is.na(TargetUser$NCountry_Called)) ,]
Business <- Temp[Temp$NCountry_Called >=6 , ]
dim(Business)
dim(Business)

Temp <- TargetUser[TargetUser$CUGMO >=8 , ]
dim(Temp)
Business <- union(Business$IMSI,Temp$IMSI)
length(Business)

Temp <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
Temp <- Temp[Temp$UniqFrMo >=20 , ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)


Temp <- TargetUser[(!is.na(TargetUser$UNR)) ,]
Temp <- Temp[Temp$UNR > .3, ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

Temp <- (TargetUser[(!is.na(TargetUser$BbyAllMt)) ,])
Temp <- Temp[Temp$BbyAllMt > .8  ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

Temp <- TargetUser[(!is.na(TargetUser$MobyMt)) ,]
Temp <- Temp[Temp$MobyMt > .3 & Temp$MobyMt < 2, ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

Temp <- TargetUser[(!is.na(TargetUser$PerCallGrtEq120sec)) ,]
Temp <- Temp[Temp$PerCallGrtEq120sec >=.75 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

Temp <- TargetUser[(!is.na(TargetUser$NumTrips)) ,]
Temp <- Temp[Temp$NumTrips >=4 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)


Temp <- TargetUser[(!is.na(TargetUser$NMO_FrPerDay_TAPIN)) ,]
Temp <- Temp[Temp$NMO_FrPerDay_TAPIN>3, ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

Temp <- TargetUser[(!is.na(TargetUser$BbyAllMo_Fr_TAPIN)) ,]
Temp<- Temp[Temp$BbyAllMo_Fr_TAPIN > .9 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)

































Temp <- TargetUser[TargetUser$CUGMO <=1  , ]
dim(Temp)
Family <- TargetUser[TargetUser$CUGMO <=1  , ]
#Family <- union(Family$IMSI,Temp$IMSI)
length(Family)

Temp <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
Temp <- Temp[Temp$UniqFrMo <=4 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)

Temp <- TargetUser[(!is.na(TargetUser$MobyMt)) ,]
Temp <- Temp[Temp$MobyMt < .3 | Temp$MobyMt > 20, ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)


Temp <- TargetUser[(!is.na(TargetUser$PerCallGrtEq120sec)) ,]
Temp <- Temp[Temp$PerCallGrtEq120sec < .1 ,]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)

Temp <- TargetUser[(!is.na(TargetUser$CUGbyAll)) ,]
Temp <- Temp[Temp$CUGbyAll > .6, ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)




Temp <- TargetUser[(!is.na(TargetUser$FbyAllMo)) ,]
Temp <- Temp[Temp$FbyAllMo > .80, ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)

Temp <- TargetUser[(!is.na(TargetUser$FbyAllMt)) ,]
Temp <- Temp[Temp$FbyAllMt > .75, ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)




intersect3 <- function(a,b,c){
	return(length(intersect(intersect(a,b),c)))
}

intersect4 <- function(a,b,c,d){
	return(length(intersect(intersect(a,b),intersect(c,d))))
}

intersect5 <- function(a,b,c,d,e){
	return(length(intersect(intersect(intersect(a,b),intersect(c,d)),e)))
}

Temp1 <- TargetUser[(!is.na(TargetUser$NCountry_Called)) ,]
Temp1 <- Temp1[Temp1$NCountry_Called >=6 , ]

Temp2 <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
Temp2 <- Temp2[Temp2$UniqFrMo >=20 , ]


Temp3 <- (TargetUser[(!is.na(TargetUser$BbyAllMt)) ,])
Temp3 <- Temp3[Temp3$BbyAllMt > .8  ,]



Temp4 <- TargetUser[(!is.na(TargetUser$MobyMt)) ,]
Temp4 <- Temp4[Temp4$MobyMt > .3 & Temp4$MobyMt < 2, ]

Temp5 <- TargetUser[(!is.na(TargetUser$BbyAllMo_Fr_TAPIN)) ,]
Temp5<- Temp5[Temp5$BbyAllMo_Fr_TAPIN > .9 ,]






















