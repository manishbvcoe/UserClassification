library(stringi)
library(data.table)
library(hash)
library(date)

## Read TAPIN records for Airtel Gabon from Sep to Dec
source("MSCCDR_features.r")
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
TAPINForsave <-TAPIN 

## 
TAPIN<-TAPIN[,c(1,2,5,7,8,9,10,11,12,13,14,15)]
#Remove the August entries from TAPIN
temp=which(substr(TAPIN[,3], 5,6)=="08")
TAPIN=TAPIN[-temp,]


## Extract Targeated user entry
setnames(TAPIN,c("V1","V2","V5","V7","V8","V9","V10"),c("EventType","Country","TimeStamp","IMSI","MSISDN","IMEI","Duration"))
Temp= TAPIN[TAPIN$Country=="FRANCE", ]
TravelledUser <- unique(Temp$IMSI)
rm(Temp)
TargetUser=intersect(TravelledUser, final_mat$IMSI)
TargetUser <- as.data.frame(TargetUser)
setnames(TargetUser,"TargetUser","IMSI") 
TargetUser.TAPIN<-merge(TargetUser,TAPIN,by="IMSI")
Temp <- TargetUser.TAPIN[,c("IMSI","TimeStamp")]
Temp$Date <- paste(substr(Temp$TimeStamp,1,4),substr(Temp$TimeStamp,5,6), substr(Temp$TimeStamp,7,8), sep="-")
TargetUser.TAPIN$Date <- Temp$Date
rm(TAPIN,Temp)



France_TAPIN<- TargetUser.TAPIN[TargetUser.TAPIN$Country=="FRANCE",c("IMSI","Date")]
NonFrance_TAPIN<- TargetUser.TAPIN[TargetUser.TAPIN$Country!="FRANCE",c("IMSI","Date")]
France_TAPIN <- unique(France_TAPIN)
NonFrance_TAPIN <- unique(NonFrance_TAPIN)
DummyUsers <- read.csv("CSVFiles/dummy.csv",header=T)
DummyUsers[,2] <- as.character(as.Date(DummyUsers[,2],"%d/%m/%y"))
France_TAPIN <- rbind(France_TAPIN, DummyUsers)





#Extract All the MO and MT Records of Target users
MO <- read.csv("CSVFiles/MO_GABON",sep=";",header=F,na.strings= c( "NA", " ", ""),colClasses = c(rep("character" , 12)))
MO<- MO[,c("V3","V6")]
setnames(MO,c("V3","V6"),c("IMSI","Date"))
MT <- read.csv("CSVFiles/MT_GABON",sep=";",header=F,na.strings= c( "NA", " ", ""),colClasses = c(rep("character" , 11)))
MT<- MT[,c("V4","V6")]
setnames(MT,c("V4","V6"),c("IMSI","Date"))
MO <- unique(MO)
MT <- unique(MT)

#bind all entry add dummy users of all date
IMSIDate_MSCCDR <- rbind(MO,MT,NonFrance_TAPIN)
temp <- which(substr(IMSIDate_MSCCDR[,2], 6,7)=="07")
IMSIDate_MSCCDR = IMSIDate_MSCCDR[-temp,]
temp <- which(substr(IMSIDate_MSCCDR[,2], 6,7)=="08")
IMSIDate_MSCCDR = IMSIDate_MSCCDR[-temp,]
IMSIDate_MSCCDR  <- rbind(IMSIDate_MSCCDR, DummyUsers)
IMSIDate_MSCCDR  <- unique(IMSIDate_MSCCDR)

#rm(MO,MT)
#Extract common user
commonuser <- as.data.frame(intersect(IMSIDate_MSCCDR$IMSI,France_TAPIN$IMSI))
setnames(commonuser,"intersect(IMSIDate_MSCCDR$IMSI, France_TAPIN$IMSI)","IMSI")
#IMSIDate_MSCCDR <- merge(IMSIDate_MSCCDR, commonuser,by="IMSI")
#France_TAPIN <- merge(France_TAPIN,commonuser,by="IMSI")

IMSIDateMatrix_MSCCDR <-as.data.frame.matrix(table(IMSIDate_MSCCDR$IMSI, IMSIDate_MSCCDR$Date))
IMSIDateMatrix_TAPIN<-as.data.frame.matrix(table(France_TAPIN$IMSI, France_TAPIN$Date))

IMSIDateMatrix_MSCCDR = IMSIDateMatrix_MSCCDR[rownames(IMSIDateMatrix_MSCCDR) %in% commonuser$IMSI, ]
IMSIDateMatrix_TAPIN = IMSIDateMatrix_TAPIN[rownames(IMSIDateMatrix_TAPIN) %in% commonuser$IMSI, ]
#NMT=NMT[rownames(NMT) %in% IMSIList, ]


#Use both NMO and NMT to determine whether a person was in domestic country
trips=matrix(nrow=length(rownames(IMSIDateMatrix_TAPIN)), ncol=1, 0)
duration=matrix(nrow=length(rownames(IMSIDateMatrix_TAPIN)), ncol=1, 0)
weekend=matrix(nrow=length(rownames(IMSIDateMatrix_TAPIN)), ncol=1, 0)

for(count in 1:length(rownames(IMSIDateMatrix_TAPIN)))
{
        print(count)
        travelled_index=which(IMSIDateMatrix_TAPIN[count, ]==1)
        trips[count, 1]= 0
        duration[count, 1]=0
        imsi=rownames(IMSIDateMatrix_TAPIN[count,])
		weekend_counter=0
		index=1
		flag="TripContinue"
        while(index < length(travelled_index))
        {

			if((sum(IMSIDateMatrix_MSCCDR[imsi, (travelled_index[index]+1):(travelled_index[index+1])])>0))
            {   
            	if(flag=="Tripbreak" | index==1)
            	{
            		trips[count, 1]=trips[count,1]+1
            		#print(index)
            		d=as.Date(colnames(IMSIDateMatrix_TAPIN[count, ][travelled_index[index]]),format ="%Y-%m-%d")
					weekday=weekdays(d)
					weekend_counter = weekend_counter + (weekday=="Sunday" | weekday=="Saturday")*1
            	}
                t=min(which(IMSIDateMatrix_MSCCDR[imsi, (travelled_index[index]+1):(travelled_index[index+1])] > 0))
                duration[count,1]=duration[count,1]+t
                flag="Tripbreak"
				if((travelled_index[index]+t) == travelled_index[index+1])
                {
                	index=index+1
                	print "zzz"
                }
                
            }
			else
			{
				#No Need to increase the weekend counter here since traveller is not doing a new trip here
				if(flag=="Tripbreak"| index==1)
        		{
        			trips[count, 1]= trips[count, 1]+1
        			d=as.Date(colnames(IMSIDateMatrix_TAPIN[count, ][travelled_index[index]]),format ="%Y-%m-%d")
					weekday=weekdays(d)
					weekend_counter = weekend_counter + (weekday=="Sunday" | weekday=="Saturday")*1
        		}
				duration[count,1]=duration[count,1]+travelled_index[index+1]-travelled_index[index]		
				flag="TripContinue"
			}
			index=index+1
        }
        if(index == length(travelled_index))
        {
        	if((sum(IMSIDateMatrix_MSCCDR[imsi, (travelled_index[index]+1):(length(IMSIDateMatrix_MSCCDR))]) > 0))
            {             	
            	t=min(which(IMSIDateMatrix_MSCCDR[imsi, (travelled_index[index]+1):(length(IMSIDateMatrix_MSCCDR))] > 0))
            	duration[count,1]=duration[count,1]+t
            }
             else
			{
				#No Need to increase the weekend counter here since traveller is not doing a new trip here	
				duration[count,1]=duration[count,1]+3
				#duration[count,1]=duration[count,1]+IMSIDateMatrix_MSCCDR[imsi,length(IMSIDateMatrix_MSCCDR)]
				#duration[count,1]=duration[count,1]+travelled_index[index+1]-travelled_index[index]
				which(IMSIDateMatrix_TAPIN[count, ]==1)	
			}
				if(flag=="Tripbreak"| index==1)
        		{
        		  trips[count, 1]= trips[count, 1]+1
        		  d=as.Date(colnames(IMSIDateMatrix_TAPIN[count, ][travelled_index[index]]),format ="%Y-%m-%d")
				  weekday=weekdays(d)
				  weekend_counter = weekend_counter + (weekday=="Sunday" | weekday=="Saturday")*1
        	    }
			
	  }
 
		duration[count,1]=duration[count,1]/trips[count,1]
		weekend[count, 1]=((weekend_counter/trips[count,1])>.5)*1
}

#assign the feature from the caluclations above
TargetUser <- as.data.frame(rownames(IMSIDateMatrix_TAPIN))
setnames(TargetUser,"rownames(IMSIDateMatrix_TAPIN)","IMSI")
TargetUser$NumTrips=trips
TargetUser$StayDuration=duration
TargetUser$Weekend=weekend
write.csv(TargetUser,userTravelInfo.csv)

#-------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#x <- data.frame(u=c("a","a","b"),d=c("2013-11-01","2013-12-02","2013-11-03"))
#y <- data.frame(u=c("b","b","a","a"),d=c("201-12-05","2013-12-04","2013-12-01","2013-12-04"))
#z <- rbind(x,y) 
#k<- data.frame(u=c("b","b","a"),d=c("2014-12-04","2013-12-01","2013-12-04"))
#u <- union(z$d,k$d)
#u<- as.vector(u)
#u <- sort(u)
#z<-z[z$d %in% u,]


