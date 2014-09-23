B <- TargetUser[ TargetUser$NCountry_Called >=5 | TargetUser$CUGMO >=5 | TargetUser$UniqFrMo >=10 | TargetUser$UNR > .3 | TargetUser$BbyAllMt> .8  | (TargetUser$MobyMt > .2 & TargetUser$MobyMt < 3) | TargetUser$NumTrips >=2 | TargetUser$StayDuration <= 6 | TargetUser$NMO_Dom_Called >=2 , ]


-------------------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$NCountry_Called)) ,]
> summary(Temp$NCountry_Called)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.000   2.000   3.123   4.000  22.000 
> dim(Temp[Temp$NCountry_Called >=5 , ])
[1] 929  57
> dim(Temp[Temp$NCountry_Called >=5 , ])
[1] 929  57
> dim(Temp[Temp$NCountry_Called >=6 , ])
[1] 649  57
> dim(Temp[Temp$NCountry_Called >=7 , ])
[1] 453  57
> dim(Temp[Temp$NCountry_Called >=8 , ])
[1] 304  57

------------------------------------------------------------------------------------
> summary(TargetUser$CUGMO)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   2.000   2.732   3.000  17.000 
  
> Temp <- TargetUser[TargetUser$CUGMO >=5 , ]
> dim(Temp)
[1] 488  57
> dim(TargetUser[TargetUser$CUGMO >=5 , ])
[1] 488  57
> dim(TargetUser[TargetUser$CUGMO >=6 , ])
[1] 284  57
> dim(TargetUser[TargetUser$CUGMO >=7 , ])
[1] 182  57
> dim(TargetUser[TargetUser$CUGMO >=8 , ])
[1] 106  57

#na not
--------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
> summary(Temp$UniqFrMo)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0     5.0     9.0    11.7    14.0   110.0 
>  dim(Temp[Temp$UniqFrMo >=10 , ])
[1] 1771   59
> dim(Temp[Temp$UniqFrMo >=11 , ])
[1] 1563   59
> dim(Temp[Temp$UniqFrMo >=12 , ])
[1] 1348   59
> dim(Temp[Temp$UniqFrMo >=15 , ])
[1] 973  59
> dim(Temp[Temp$UniqFrMo >=19 , ])
[1] 636  59

#na not
------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$UNR)) ,]
> summary(Temp$UNR)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.002296 0.056210 0.097560 0.109700 0.150000 0.451600 

> dim(Temp[Temp$UNR > .3, ])
[1] 38 59
> dim(Temp[Temp$UNR > .4, ])
[1]  4 59
> dim(Temp[Temp$UNR >= .2, ])
[1] 441  59
> dim(Temp[Temp$UNR >= .1, ])
[1] 1940   59

#na not need
------------------------------------------------------------------------------------
Temp <- (TargetUser[(!is.na(TargetUser$BbyAllMt)) ,])
> summary(Temp$BbyAllMt)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02174 0.34880 0.50000 0.49780 0.62500 1.00000 
> dim(Temp[Temp$BbyAllMt > .7  ,])
[1] 516  59
> dim(Temp[Temp$BbyAllMt > .8  ,])
[1] 253  57
> dim(Temp[Temp$BbyAllMt > .9  ,])
[1] 152  57
-------------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$MobyMt)) ,]
> summary(Temp$MobyMt)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  0.2042   3.4740   6.5000  11.3600  12.8000 353.0000 
> dim(Temp[Temp$MobyMt > .2 & Temp$MobyMt < 3, ])
[1] 715  59
> dim(Temp[Temp$MobyMt > .2 & Temp$MobyMt < 2, ])
[1] 347  59
> dim(Temp[Temp$MobyMt > .3 & Temp$MobyMt < 2, ])
[1] 344  59

----------------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$PerCallGrtEq120sec)) ,]
> summary(Temp$PerCallGrtEq120sec)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.2069  0.3226  0.3279  0.4324  1.0000 

> dim(Temp[Temp$PerCallGrtEq120sec >=.75 ,])
[1] 96 59



--------------------------------------------------------------------------------------------------
TAPIN
-------------------------------------------------------------------------------------------------


Temp <- TargetUser[(!is.na(TargetUser$NumTrips)) ,]
> summary(TargetUser$NumTrips)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    1.00    1.00    1.38    1.00   11.00 

> dim(Temp[Temp$NumTrips >=2 ,])
[1] 974  59
> dim(Temp[Temp$NumTrips >=3 ,])
[1] 319  59
> dim(Temp[Temp$NumTrips >=4 ,])
[1] 126  59
#na not
----------------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$StayDuration)) ,]
> summary(Temp$StayDuration)
   Min. 1st Qu.  Median    Mean 3rd Q
   u.    Max. 
   1.00    5.50   12.00   16.94   22.00  150.00 
   
  
   
> dim(Temp[Temp$StayDuration <= 6,])
[1] 1164   59
> dim(Temp[Temp$StayDuration <= 4,])
[1] 781  59
> dim(Temp[Temp$StayDuration <= 2,])
[1] 454  59
> dim(Temp[Temp$StayDuration <= 1,])
[1] 300  59
------------------------------------------------------------------------------------------------
NMO_Dom_TAPIN

Temp <- TargetUser[(!is.na(TargetUser$NMO_Dom_TAPIN)) ,]
> summary(Temp$NMO_Dom_TAPIN)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    8.00   66.97   31.00 5902.00 
> dim(Temp[Temp$NMO_Dom_TAPIN > 2 ,])
[1] 1981   59
> dim(Temp[Temp$NMO_Dom_TAPIN > 31 ,])



------------------------------------------------------------------------------------------------

Temp <- TargetUser[(!is.na(TargetUser$NMO_FrPerDay_TAPIN)) ,]
> summary(Temp$NMO_FrPerDay_TAPIN)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.5583  0.5000 17.5300
> dim(Temp[Temp$NMO_FrPerDay_TAPIN>2, ])
[1] 214  59
> dim(Temp[Temp$NMO_FrPerDay_TAPIN>3, ])
[1] 126  59
-------------------------------------------------------------------------------------------
Temp <- TargetUser[(!is.na(TargetUser$BbyAllMo_Fr_TAPIN)) ,]
> summary(Temp$BbyAllMo_Fr_TAPIN)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.1252  0.0000  1.0000 
 
dim(Temp[Temp$BbyAllMo_Fr_TAPIN > .9 ])
> dim(Temp[Temp$BbyAllMo_Fr_TAPIN > .9 ,])
[1] 266  59

#na present

#
----------------------------------------------------------------------
#Business
----------------
Businesslst =list()

Temp <- TargetUser[(!is.na(TargetUser$NCountry_Called)) ,]
Business <- Temp[Temp$NCountry_Called >=6 , ]
dim(Business)
dim(Business)
BNCountry_Called <-Business$IMSI



Temp <- TargetUser[TargetUser$CUGMO >=8 , ]
dim(Temp)
Business <- union(Business$IMSI,Temp$IMSI)
length(Business)
BCUGMO <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
Temp <- Temp[Temp$UniqFrMo >=20 , ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BUniqFrMo <-Temp$IMSI


Temp <- TargetUser[(!is.na(TargetUser$UNR)) ,]
Temp <- Temp[Temp$UNR > .3, ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BUNR <-Temp$IMSI

Temp <- (TargetUser[(!is.na(TargetUser$BbyAllMt)) ,])
Temp <- Temp[Temp$BbyAllMt > .8  ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BBbyAllMt <-Temp$IMSI

#Temp <- TargetUser[(!is.na(TargetUser$MobyMt)) ,]
#Temp <- Temp[Temp$MobyMt > .3 & Temp$MobyMt < 2, ]
#dim(Temp)
#Business <- union(Business,Temp$IMSI)
#length(Business)
#BMobyMt <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$PerCallGrtEq120sec)) ,]
Temp <- Temp[Temp$PerCallGrtEq120sec >=.75 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BPerCallGrtEq120sec <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$NumTrips)) ,]
Temp <- Temp[Temp$NumTrips >=4 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BNumTrips <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$NMO_FrPerDay_TAPIN)) ,]
Temp <- Temp[Temp$NMO_FrPerDay_TAPIN>3, ]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BNMO_FrPerDay_TAPIN <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$BbyAllMo_Fr_TAPIN)) ,]
Temp<- Temp[Temp$BbyAllMo_Fr_TAPIN > .9 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BBbyAllMo_Fr_TAPIN <-Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$StayDuration)) ,]
Temp<- Temp[Temp$StayDuration < 7 ,]
dim(Temp)
Business <- union(Business,Temp$IMSI)
length(Business)
BStayDuration <-Temp$IMSI



#input  <-list(StayDuration= BStayDuration,NCountry_Called = BNCountry_Called, UniqFrMo = BUniqFrMo, BbyAllMt =BBbyAllMt, BbyAllMo_Fr_TAPIN = BBbyAllMo_Fr_TAPIN)

#venn(input)
--------------------------------------------------------------------------------------------------
#Family
-------------------------------------------------------------------------------------------------
Family <- TargetUser[TargetUser$StayDuration >= 21, ]
dim(Family)
FStayDuration <-Family$IMSI

Temp <- TargetUser[(!is.na(TargetUser$FbyAllNMO_TAPIN)) ,]
Temp <- Temp[Temp$FbyAllNMO_TAPIN > .8 ,]
dim(Temp)
Family <- union(Family$IMSI,Temp$IMSI)
length(Family)
FFbyAllNMO_TAPIN <- Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$MOperDay_TAPIN)) ,]
Temp <- Temp[Temp$MOperDay_TAPIN < 0.2 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)
FMOperDay_TAPIN <- Temp$IMSI


Temp <- TargetUser[(!is.na(TargetUser$UniqFrMo)) ,]
Temp <- Temp[Temp$UniqFrMo <= 2 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)
FUniqFrMo <- Temp$IMSI


Temp <- TargetUser[(!is.na(TargetUser$UNR)) ,]
Temp <- Temp[Temp$UNR <.02 , ]
#Temp <- TargetUser[(!is.na(TargetUser$PersOfZeroDurationCall)) ,]
#Temp <- Temp[Temp$PersOfZeroDurationCall > .05 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
FUNR <- Temp$IMSI
length(Family)


Temp <- TargetUser[(!is.na(TargetUser$FbyAllMO)) ,]
Temp <- Temp[Temp$FbyAllMO >.75 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)
FFbyAllMO <- Temp$IMSI

Temp <- TargetUser[(!is.na(TargetUser$FbyAllMt)) ,]
Temp <- Temp[Temp$FbyAllMt > .75 , ]
dim(Temp)
Family <- union(Family,Temp$IMSI)
length(Family)
FFbyAllMt <- Temp$IMSI

#length(Family)
#FPersOfZeroDurationCall <- Temp$IMSI





input  <-list(StayDuration = FStayDuration, FbyAllNMO_TAPIN = FFbyAllNMO_TAPIN, FMOperDay_TAPIN = FMOperDay_TAPIN, UniqFrMo = FUniqFrMo)

venn(input)

input  <-list(Business= Business,Family = Family)

venn(input)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Businesslst=c(BNCountry_Called, BCUGMO, BUniqFrMo, BUNR, BBbyAllMt, BPerCallGrtEq120sec, BNumTrips, BNMO_FrPerDay_TAPIN, BBbyAllMo_Fr_TAPIN, BStayDuration)

 Businesslst=list(BNCountry_Called, BCUGMO, BUniqFrMo, BUNR, BBbyAllMt, BPerCallGrtEq120sec, BNumTrips, BNMO_FrPerDay_TAPIN, BBbyAllMo_Fr_TAPIN, BStayDuration)

Familylst <- list(FStayDuration, FFbyAllNMO_TAPIN, FMOperDay_TAPIN, FUniqFrMo, FUNR, FFbyAllMO, FFbyAllMt)

Cuser <- intersect(Business,Family)

BusinesslstName=list("BNCountry_Called", "BCUGMO", "BUniqFrMo", "BUNR", "BBbyAllMt", "BPerCallGrtEq120sec", "BNumTrips", "BNMO_FrPerDay_TAPIN", "BBbyAllMo_Fr_TAPIN", "BStayDuration")

FamilylstName <- list("FStayDuration", "FFbyAllNMO_TAPIN", "FMOperDay_TAPIN", "FUniqFrMo"," FUNR", "FFbyAllMO", "FFbyAllMt")

i=1
sum=0
for(Buser in Businesslst)
{       
           j=1
               for(Fuser in Familylst)
               {
                       
                       print(c(BusinesslstName[i] , FamilylstName[j]))
                       #len=length(intersect(intersect(Buser, Fuser), Cuser))
                       len=length(intersect(Buser, Fuser))
                       len1=length(union(Buser, Fuser))
                       print(len)
                       print(len1)
                       print(len/len1*100)
                       sum=sum+len
                       j=j+1
                       
               }
       i=i+1
}

#print(sum)


DataFrame= as.data.frame(BusinessFeature=numeirc,FamilyFeature,intersection,)
n=70
Chart <- data.frame(BusinessFeature = numeric(n),FamilyFeature = numeric(n) ,intesection=numeric(n),union=numeric(n),percentage=numeric(n))

Businesslst=list(BNCountry_Called, BCUGMO, BUniqFrMo, BUNR, BBbyAllMt, BPerCallGrtEq120sec, BNumTrips, BNMO_FrPerDay_TAPIN, BBbyAllMo_Fr_TAPIN, BStayDuration)

Familylst <- list(FStayDuration, FFbyAllNMO_TAPIN, FMOperDay_TAPIN, FUniqFrMo, FUNR, FFbyAllMO, FFbyAllMt)

Cuser <- intersect(Business,Family)

BusinesslstName=list("BNCountry_Called", "BCUGMO", "BUniqFrMo", "BUNR", "BBbyAllMt", "BPerCallGrtEq120sec", "BNumTrips", "BNMO_FrPerDay_TAPIN", "BBbyAllMo_Fr_TAPIN", "BStayDuration")

FamilylstName <- list("FStayDuration", "FFbyAllNMO_TAPIN", "FMOperDay_TAPIN", "FUniqFrMo"," FUNR", "FFbyAllMO", "FFbyAllMt")

i=1
sum=0
count=1
for(Buser in Businesslst)
{       print(BusinesslstName[i])
           j=1
               for(Fuser in Familylst)
               {
                       
                       print(FamilylstName[j])
                       #len=length(intersect(intersect(Buser, Fuser), Cuser))
                       len=length(intersect(Buser, Fuser))
                       len1=length(union(Buser, Fuser))
                       print(len)
                       print(len1)
                       print(len/len1*100)
                       sum=sum+len
                       Chart$BusinessFeature[count] <- BusinesslstName[[i]]
                       Chart$FamilyFeature[count]<- FamilylstName[[j]]
                       Chart$intesection[count] <- len
                       Chart$union[count]  <- len1
                       Chart$percentage[count]  <-len/len1*100
                       j=j+1
                       count=count+1
               }
       i=i+1
}
write.csv(Chart,file="chart.csv")



Temp1 = TargetUser[TargetUser$NCountry_Called >=6 & TargetUser$StayDuration >=21,]

