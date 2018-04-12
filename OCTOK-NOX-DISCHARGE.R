#######################################################################
#
#  PREDICT FLOW FROM MACON GAUGE
#
#######################################################################

# USGS GAGING STATION DATA
siteNo <- "02448000"
pCode <- c("00060","00065")
start.date <- "2016-02-11"
end.date <- "2018-04-3"
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



# LOGGER DATA: STAGE LOGGER LOCATED IN OKTOC CREEK
comm<-RODBC::odbcConnectAccess2007("_dat/Paddlefish Database.accdb")
logger<-RODBC::sqlQuery(comm,"SELECT [Temp & Stage Logger Data].Site_ID, [Temp & Stage Logger Data].Serial_Number, [Temp & Stage Logger Data].Date, [Temp & Stage Logger Data].AbsPres, [Temp & Stage Logger Data].TempF, [Temp & Stage Logger Data].TempC, [Temp & Stage Logger Data].AbsPresBar, [Temp & Stage Logger Data].SensorDepth, [Temp & Stage Logger Data].SensorDepthM
    FROM [Temp & Stage Logger Data]
    WHERE ((([Temp & Stage Logger Data].Serial_Number)=10868627));")
## CALCULATE DAILY STAGE
oktocDaily<- aggregate(SensorDepthM~Date,logger,mean)
## CONVERT POSIX TO DATE
oktocDaily$Date<-as.Date(oktocDaily$Date)

## MERGE OKTOC TO NOXUBEE AT MACON
allData<- merge(noxDaily,oktocDaily,by="Date",all.x=TRUE)
allData$month<-as.numeric(format(allData$Date,"%m"))


## ADD FACTORS TO ACCOUNT FOR DRIFT

allData$id<- NA

allData[allData$Date>as.Date("2016-04-20")
    & allData$Date<=as.Date("2016-06-22"),]$id<-1
allData[allData$Date>as.Date("2016-06-22")
    & allData$Date<=as.Date("2016-07-28"),]$id<-2   
#allData[allData$Date>as.Date("2016-07-28")
#    & allData$Date<=as.Date("2016-11-16"),]$id<-4   
allData[allData$Date>as.Date("2016-11-16")
    & allData$Date<=as.Date("2016-12-01"),]$id<-3 
allData[allData$Date>as.Date("2016-12-01")
    & allData$Date<=as.Date("2017-03-10"),]$id<-4  
allData[allData$Date>as.Date("2017-03-10")
    & allData$Date<=as.Date("2017-04-27"),]$id<-5 
allData[allData$Date>as.Date("2017-04-27")
    & allData$Date<=as.Date("2017-06-22"),]$id<-6 
allData[allData$Date>as.Date("2017-06-22")
    & allData$Date<=as.Date("2017-11-03"),]$id<-7  
allData$id<-factor(allData$id,levels=c(1:7))
allData<-allData[order(allData$Date),]
sid=6
fit<-lm(SensorDepthM ~ gage*id, allData)

allData$p<- predict(fit,allData)
plot(SensorDepthM~p,allData);abline(0,1)
plot(SensorDepthM~log(gage),allData,subset=id==sid)

plot(p~Date,allData,type='l',col="red")
points(SensorDepthM~Date,allData,type='l')

plot(SensorDepthM~log(gage),allData,subset=id==sid)
curve(coef(fit)['a'] * exp(-coef(fit)['b']*exp(-coef(fit)['b']*x)) ,add=TRUE) 

## 
plot(noxDaily$Q,type='l')
plot(oktocDaily$SensorDepthM,type='l')




plot(log(log(SensorDepthM))~log(log(Date)),allData,type='l')

plot(log(log(SensorDepthM))~log(log(gage)),allData,subset=Date>as.Date("2016-11-16")
    & Date<=as.Date("2016-12-01"),col="green")
plot(log(log(SensorDepthM))~log(log(gage)),allData,subset=Date>as.Date("2017-03-10")
    & Date<=as.Date("2017-04-27"),col="red",pch=19)  


plot(gage~Date,allData,type='l',ylim=c(0,35))
points(SensorDepthM~Date,allData,type='l')

## PREDICT OKTOC STAGE FROM NOXUBEE AT MACON
plot(SensorDepthM~gage,allData,
    xlab="Gage height (ft; Noxubee River at Macon)",
    ylab="Stage height (m; Oktoc Creek)")

    
## STAGE LOGGER DRIFT AND PREDICTION
plot(log(SensorDepthM)~log(gage),allData,type='n')
points(log(SensorDepthM)~log(gage),allData,subset=Date<=as.Date("2016-04-20"),col="red")
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2016-04-20")
    & Date<=as.Date("2016-06-22"),col="red")
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2016-06-22")
    & Date<=as.Date("2016-07-28"),col="blue")
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2016-07-28")
    & Date<=as.Date("2016-11-16"),col="green")    
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2016-11-16")
    & Date<=as.Date("2016-12-01"),col="green")      
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2016-12-01")
    & Date<=as.Date("2017-03-10"),col="black")          
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2017-03-10")
    & Date<=as.Date("2017-04-27"),col="red",pch=19)      
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2017-04-27")
    & Date<=as.Date("2017-06-22"),col="green",pch=19)   
points(log(SensorDepthM)~log(gage),allData,subset=Date>as.Date("2017-06-22")
    & Date<=as.Date("2017-11-03"),col="blue",pch=19)      

## EVALUATING STAGE LOGGER DRIFT
plot(SensorDepthM~Date,allData,subset=Date>as.Date("2016-04-20")
    & Date<=as.Date("2016-06-22"),col="red")
plot(SensorDepthM~Date,logger,subset=Date>as.Date("2016-04-20")
    & Date<=as.Date("2016-06-22"),col="red",type='l')
    
plot(SensorDepthM~Date,allData,subset=Date>as.Date("2016-04-20")
    & Date<=as.Date("2016-07-28"),col="red",type='l')
points(SensorDepthM~Date,allData,subset=Date>as.Date("2016-06-22")
    & Date<=as.Date("2016-07-28"),col="blue",type='l')