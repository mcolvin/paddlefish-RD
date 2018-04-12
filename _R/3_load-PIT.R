#######################################################################
# NOTES
#######################################################################


# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(11532,11529)
# pit==1005085830



#######################################################################
#
# COM TO DBASE
#
#######################################################################

comm<- odbcConnectAccess2007("_dat/Paddlefish Database.accdb")

#######################################################################
#
# QUERY EFFORT DATA
#
#######################################################################
    
effort<- sqlQuery(comm,"SELECT [Paddlefish Effort Data].occasionId, [Paddlefish Effort Data].ID AS id, [Paddlefish Effort Data].Date AS [date], [Paddlefish Effort Data].set_number, [Paddlefish Effort Data].effort, [Paddlefish Effort Data].n_paddlefish
FROM [Paddlefish Effort Data];")    

## FORMAT AND CLEAN UP EFFORT DATA
effort$doy<-as.numeric(format(effort$date, "%j"))
effort$year<-as.numeric(format(effort$date, "%Y"))
effort$date<-as.Date(effort$date)
## FORMATTING INDICES
effort<- effort[order(effort$date, effort$set_number),]
#effort$sample<- paste(effort$year,effort$doy

effort <- transform(effort,occasionId=as.numeric(factor(date)))
effort$secid<-as.factor(c(1:nrow(effort)))## SECONDARY OCCASION ID

## START DATE
strDate<- min(effort$date)


####################################################################### 
##   
## QUERY TAGGING DATA
##
#######################################################################

## QUERY TAGGING DATA
taggingData<- sqlQuery(comm,"SELECT [Paddlefish Tagging Data].ID, [Paddlefish Tagging Data].Date, [Paddlefish Tagging Data].set AS set_number, [Paddlefish Tagging Data].tl, [Paddlefish Tagging Data].rfl, [Paddlefish Tagging Data].efl, [Paddlefish Tagging Data].girth, [Paddlefish Tagging Data].floy_color, [Paddlefish Tagging Data].floy_number, [Paddlefish Tagging Data].weight, [Paddlefish Tagging Data].pit, [Paddlefish Tagging Data].new_recap, [Paddlefish Tagging Data].transmitter, [Paddlefish Tagging Data].sex, [Paddlefish Tagging Data].transmitter_long FROM [Paddlefish Tagging Data]; ")
taggingData$year<- format(taggingData$Date,"%Y")
taggingData$doy<- as.numeric(as.character(format(taggingData$Date,"%j")))
## ASSIGN OCCASSION ID TO MASTER CAPTURE RECAPTURE DATASET
tmp<-merge(taggingData,effort,by=c("year","doy","set_number"),all.x=TRUE)
tmp<- tmp[which(is.na(tmp$occasionId)==FALSE),]
tmp$tmp<-1
## SET UP CAPTURE HISTORY FOR ALL FISH
ch<-reshape2::dcast(tmp,pit~secid,value.var="tmp",sum,
    drop=FALSE)

# BUNDLE UP DATA FOR JAGS

dat<-list() 

## SCALAR; NUMBER OF DAYS
dat$D<-as.integer(max(effort$date)-(min(effort$date)-1))
## VECTOR; DAY OF SAMPLING
dat$int_str<-sort(unique(effort$date-(min(effort$date)-1)))
## VECTOR; DAY BEFORE NEXT SAMPLING
dat$int_end<-dat$int_str[-1]-1
## MATRIX; CAPTURE HISTORIES, IND FISH
dat$ch<-ch[,-1]
dat$ch[dat$ch>1]<-1 ## some fish have replicate length/weight data
dat$secid<-effort$occasionId
## SCALAR; NUMBER OF PRIMARY OCCASIONS
dat$nprim<-max(effort$occasionId)
## SCALAR; NUMBER OF TAGGED FISH
dat$M<-nrow(dat$ch)
## SCALAR; NUMBER OF SECONDARY OCCASIONS
dat$nocc<-ncol(dat$ch)    
## MATRIX; DAILY COVARIATE
dat$X<-rep(1,dat$D)



#######################################################################
#
#  LOAD COVARIATES
#
#######################################################################


# USGS GAGING STATION DATA
siteNo <- "02448000"
pCode <- c("00060","00065")
start.date <- min(effort$date)
end.date <- max(effort$date)
## #         79689       00065     Gage height, feet
## #         79690       00060     Discharge, cubic feet per second
## READ IN NOXUBEE RIVER AT MACON GAGE DATA
noxGage <- dataRetrieval::readNWISuv(siteNumbers = siteNo,
    parameterCd = pCode,
    startDate = start.date,
    endDate = end.date)
## RENAME TO SOMETHING USEFUL
names(noxGage)[c(4,6)]<- c("Q","gage")
## CONVERT POSIX TO DATE
noxGage$Date<-as.Date(noxGage$dateTime)
## DAILY MEANS
noxDaily<- aggregate(cbind(Q,gage)~Date,noxGage,mean)






############# OKTOC SPILLWAY STAGE LOGGER (SN: 10868627) ############
OktocSpillway<-sqlFetch(comm,"Temp & Stage Query",as.is=TRUE)


stage.dat<-na.omit(subset(OktocSpillway, Serial_Number==10868627)) 
stage.dat$Date1<-ymd_hms(stage.dat$Date) 
stage.dat$Oktocstage<-as.numeric(stage.dat$SensorDepthM) 

stage.dat$year<-as.numeric(format(stage.dat$Date1, "%Y"))
stage.dat$month<-as.numeric(format(stage.dat$Date1, "%m"))
stage.dat$day<-as.numeric(format(stage.dat$Date1, "%d"))


## Stage Data
    Oktoc.Stage<-aggregate(Oktocstage~year+month+day,stage.dat,mean)
    Oktoc.Stage<-Oktoc.Stage[order(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day),] 
    Oktoc.Stage<-Oktoc.Stage[order(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day),] 
    Oktoc.Stage$date<-as.POSIXct(paste(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day, sep='-'))
    

## Temperature Data
    temp.dat<-subset(OktocSpillway, Serial_Number==10760997)
    
    temp.dat$Date1<-ymd_hms(temp.dat$Date) 
    temp.dat$temp.dat<-as.numeric(temp.dat$TempC) 
    
    temp.dat$year<-as.numeric(format(temp.dat$Date1, "%Y"))
    temp.dat$month<-as.numeric(format(temp.dat$Date1, "%m"))
    temp.dat$day<-as.numeric(format(temp.dat$Date1, "%d"))
    
    Oktoc.TempC<-aggregate(TempC~year+month+day,temp.dat,mean)
    Oktoc.TempC<-Oktoc.TempC[order(Oktoc.TempC$year,Oktoc.TempC$month,Oktoc.TempC$day),]
    Oktoc.TempC$date<-as.POSIXct(paste(Oktoc.TempC$year,Oktoc.TempC$month,Oktoc.TempC$day, sep='-'))
    Oktoc.TempC<-subset(Oktoc.TempC, select=-c(year,month,day)) ## final Temp dataset


# final merged with temp & stage dataset
alldat<-merge(Oktoc.Stage,Oktoc.TempC, by=c("date"),all=TRUE) 
alldat<-alldat[order(alldat$date),] 


