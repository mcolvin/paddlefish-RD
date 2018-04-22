#######################################################################
# NOTES
#######################################################################


# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(11532,11529)
# pit==1005085830
#11539
#121021201081

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
effort$dayId<- as.integer(effort$date-min(effort$date))+1L
## START DATE
strDate<- min(effort$date)
endDate<- max(effort$date)




####################################################################### 
##   
## PIT TAGGING MATRIX
##
#######################################################################

## QUERY TAGGING DATA
taggingData<- sqlQuery(comm,"SELECT [Paddlefish Tagging Data].ID, [Paddlefish Tagging Data].Date, [Paddlefish Tagging Data].set AS set_number, [Paddlefish Tagging Data].tl, [Paddlefish Tagging Data].rfl, [Paddlefish Tagging Data].efl, [Paddlefish Tagging Data].girth, [Paddlefish Tagging Data].floy_color, [Paddlefish Tagging Data].floy_number, [Paddlefish Tagging Data].weight, [Paddlefish Tagging Data].pit, [Paddlefish Tagging Data].new_recap, [Paddlefish Tagging Data].transmitter, [Paddlefish Tagging Data].sex, [Paddlefish Tagging Data].transmitter_long FROM [Paddlefish Tagging Data]; ")
taggingData<-taggingData[order(taggingData$ID),]
taggingData$year<- format(taggingData$Date,"%Y")
taggingData$doy<- as.numeric(as.character(format(taggingData$Date,"%j")))
## ASSIGN OCCASSION ID TO MASTER CAPTURE RECAPTURE DATASET
tmp<-merge(taggingData,effort,by=c("year","doy","set_number"),all.x=TRUE)
tmp<- tmp[which(is.na(tmp$occasionId)==FALSE),]
tmp$tmp<-1
tmp$transmitter_long<- as.character(tmp$transmitter_long)

## ASSIGN WHETHER A FISH WAS ACOUSTICALLY TAGGED
tag<- dcast(tmp, pit+transmitter_long~"n",value.var="tmp",sum)
acoustics<-subset(tag,!(is.na(transmitter_long)))
tag$acoustic<-ifelse(is.na(tag$transmitter_long),0,1)
tag<-dcast(tag,pit~acoustic,value.var="n",length)
tag<-tag[,c(1,3)]
names(tag)[2]<-"type"
tag$type<-ifelse(tag$type==0,"p",'a')
## SET UP CAPTURE HISTORY FOR ALL FISH
ch<-reshape2::dcast(tmp,pit~secid,value.var="tmp",sum,
    drop=FALSE)
## CAPTURE HISTORY FOR PIT TAG ONLY FIHS
ch_p<-subset(ch,pit%in% tag$pit[tag$type=="p"])    
## CAPTURE HISTORY FOR ACOUSTIC TAG FISH
ch_a<-subset(ch,pit%in% tag$pit[tag$type=="a"])    
ch_a<-ch_a[match(acoustics$pit,ch_a$pit),]  

#######################################################################
#
#  STATES MATRIX FOR ACOUSTIC FISH
#
#######################################################################
meta<- sqlFetch(comm, "Transmitter Numbers")
meta<-subset(meta,Transmitter != "A69-1303-11539")
meta$Implantation<-as.Date(meta$"Implantation Date")
duration<- max(effort$date)-min(effort$date)
dates<-min(effort$date)+c(0:duration)
state_matrix<-matrix(1,nrow=length(dates),ncol=nrow(meta))

## NAMING ROWS AND COLUMNS
colnames(state_matrix)<-meta$Transmitter #COLUMNS = FISH id
dates<-as.character(dates)
rownames(state_matrix)<-dates #ROWS = DATES


## ASSIGNING STATE 2 FOR MOVERS TO NOXUBEE

#state_matrix[dates>="2017-04-05","A69-1303-11417"]<-2
state_matrix[dates>="2017-03-11","A69-1303-11541"]<-2  
state_matrix[dates>="2017-06-25","A69-1303-11542"]<-2 
state_matrix[dates>="2017-04-05","A69-1303-11544"]<-2
state_matrix[dates>="2017-04-06","A69-9001-20196"]<-2
state_matrix[dates>="2017-04-06","A69-9001-20200"]<-2 #moved into Noxubee in 2016, coded  below, returned to the pool, and then left again in 2017 and hasn't been detected
state_matrix[dates>="2017-04-06","A69-9001-20203"]<-2
state_matrix[dates>="2017-03-15","A69-9001-20208"]<-2
state_matrix[dates>="2018-03-16","A69-1303-11529"]<-2

#weird fish that came back to the pool
state_matrix[dates>="2017-03-15" & dates<="2017-07-15" ,"A69-1303-11413"]<-2
state_matrix[dates>="2016-04-15" & dates<="2016-05-07" ,"A69-9001-20200"]<-2
state_matrix[dates>="2017-04-05" & dates<="2017-05-20" ,"A69-1303-11417"]<-2  


## ASSIGNING STATE 3 TO TRANSLOCATION FISH
state_matrix[dates>="2018-01-12","A69-1303-11415"]<-2 #1
state_matrix[dates>="2018-01-12","A69-9001-20199"]<-2 #2
state_matrix[dates>="2018-01-12","A69-1303-11538"]<-2 #3
state_matrix[dates>="2018-01-13","A69-1303-11530"]<-2 #4
state_matrix[dates>="2018-01-15","A69-9001-20202"]<-2 #5
state_matrix[dates>="2018-01-15","A69-1303-11547"]<-2 #6
state_matrix[dates>="2018-01-25" & dates<="2018-03-20","A69-1303-11531"]<-2 #7  #11531 was translocated upstream and returned to pool on 3/20/18
state_matrix[dates>="2018-01-25","A69-1303-11545"]<-2 #8

state_matrix["2018-01-12","A69-1303-11415"]<-3 #1
state_matrix["2018-01-12","A69-9001-20199"]<-3 #2
state_matrix["2018-01-12","A69-1303-11538"]<-3 #3
state_matrix["2018-01-13","A69-1303-11530"]<-3 #4
state_matrix["2018-01-15","A69-9001-20202"]<-3 #5
state_matrix["2018-01-15","A69-1303-11547"]<-3 #6
state_matrix["2018-01-25","A69-1303-11531"]<-3 #7
state_matrix["2018-01-25","A69-1303-11545"]<-3 #8

  
## ASSIGN VALUES PRE IMPLANTATION AS -99
for(i in 1:nrow(meta))
    {
    state_matrix[dates<meta[i,]$Implantation,i]<- NA ## CENSOR PRE TAGGING
    state_matrix[dates>meta[i,]$Failure_date,i]<- NA ## CENSOR POST TAG FAILURE
    }    

# 1=in pool pr(capture)=?
# 2=outside pool pr(capture)=0

## SET INDEX FOR STATES THAT ARE ASSIGNED
stuff<-lapply(1:ncol(state_matrix),function(x)
    {
    id<-1:nrow(state_matrix)
    yy<-state_matrix[,x]
    xx<-range(id[yy %in% c(1,2)]) 
    return(data.frame(tag=colnames(state_matrix)[x],tagged=xx[1],fail=xx[2]))
    })

ac_meta<-do.call(rbind,stuff)## IDS TO EVALLUATE OVER (TAG DAY:END|FAILURE DAY)
## ADD PIT TAG NUMBERS
ac_meta<- merge(ac_meta, acoustics[,-3], 
    by.x="tag",by.y="transmitter_long",all.x=TRUE)
ac_meta$life<- ac_meta$fail-ac_meta$tagged
ac_meta <- ac_meta[order(ac_meta$tagged,
    -1*ac_meta$life,decreasing=FALSE),] 
rownames(ac_meta)<-1:nrow(ac_meta) 

## ORDER STATE MATRIX TO MATCH AC_META
indx<- match(ac_meta$tag,colnames(state_matrix))
state_matrix<- state_matrix[,indx]
## ORDER CAPTURE HISTORIES TO MATCH AC_META
indx<- match(ac_meta$pit,ch_a$pit)
ch_a<-ch_a[indx,]




  
#######################################################################
#
#  LOAD COVARIATES
#
#######################################################################
if(run==TRUE)
    {
    # USGS GAGING STATION DATA
    siteNo <- "02448000"
    pCode <- c("00060","00065")
    ## #         79689       00065     Gage height, feet
    ## #         79690       00060     Discharge, cubic feet per second
    ## READ IN NOXUBEE RIVER AT MACON GAGE DATA
    noxGage <- dataRetrieval::readNWISuv(siteNumbers = siteNo,
        parameterCd = pCode,
        startDate = strDate,
        endDate = endDate)
    ## RENAME TO SOMETHING USEFUL
    names(noxGage)[c(4,6)]<- c("Q","gage")
    ## CONVERT POSIX TO DATE
    noxGage$Date<-as.Date(noxGage$dateTime)
    ## DAILY MEANS
    noxDaily<- aggregate(cbind(Q,gage)~Date,noxGage,mean)
    noxDaily<- subset(noxDaily, Date<=endDate)
    saveRDS(noxDaily,"_output/noxDaily.RDS")
    }
noxDaily<-readRDS("_output/noxDaily.RDS")

############# OKTOC SPILLWAY STAGE LOGGER (SN: 10868627) ############
OktocSpillway<-sqlFetch(comm,"Temp & Stage Query",as.is=TRUE)

stage.dat<-na.omit(subset(OktocSpillway, Serial_Number==10868627)) 
stage.dat$Date1<-ymd_hms(stage.dat$Date) 
stage.dat$Oktocstage<-as.numeric(stage.dat$SensorDepthM) 

stage.dat$year<-as.numeric(format(stage.dat$Date1, "%Y"))
stage.dat$month<-as.numeric(format(stage.dat$Date1, "%m"))
stage.dat$day<-as.numeric(format(stage.dat$Date1, "%d"))


## Stage Data
Oktoc.Stage<-aggregate(cbind(Oktocstage,TempC)~year+month+day,stage.dat,mean)
Oktoc.Stage<-Oktoc.Stage[order(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day),] 
Oktoc.Stage<-Oktoc.Stage[order(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day),] 
Oktoc.Stage$Date<-as.POSIXct(paste(Oktoc.Stage$year,Oktoc.Stage$month,Oktoc.Stage$day, sep='-'))


## Temperature Data
temp.dat<-subset(OktocSpillway, Serial_Number%in%c(10416312,10760997))

temp.dat$Date1<-ymd_hms(temp.dat$Date) 
temp.dat$temp.dat<-as.numeric(temp.dat$TempC) 

temp.dat$year<-as.numeric(format(temp.dat$Date1, "%Y"))
temp.dat$month<-as.numeric(format(temp.dat$Date1, "%m"))
temp.dat$day<-as.numeric(format(temp.dat$Date1, "%d"))

Oktoc.TempC<-aggregate(TempC~year+month+day,temp.dat,mean)
Oktoc.TempC<-Oktoc.TempC[order(Oktoc.TempC$year,Oktoc.TempC$month,Oktoc.TempC$day),]
Oktoc.TempC$Date<-as.POSIXct(paste(Oktoc.TempC$year,Oktoc.TempC$month,Oktoc.TempC$day, sep='-'))
Oktoc.TempC<-subset(Oktoc.TempC, select=-c(year,month,day)) ## final Temp dataset


# final merged with temp & stage dataset
alldat<-merge(Oktoc.Stage,Oktoc.TempC, by=c("Date"),all=TRUE) 
alldat<-alldat[order(alldat$Date),] 
## FILL MISSING TEMPERATURE DATA
alldat$TempC.y<-ifelse(is.na(alldat$TempC.y),alldat$TempC.x,alldat$TempC.y)

# MERGE WITH USGS GAGE DATA
alldat$day<- as.integer((as.Date(alldat$Date)-strDate)+1)
noxDaily$day<- as.integer((as.Date(noxDaily$Date)-strDate)+1)
alldat2<- merge(noxDaily, alldat,by="day",all.x=TRUE)
X<- alldat2[,match(c("day","Q","gage","Oktocstage","TempC.y"),names(alldat2))]
X<- X[order(X$day),]



#######################################################################
#
#  BUNDLE UP DATA FOR JAGS
#
#######################################################################

## DATA FORMATTED FOR IMP
ipmdat<-list()
### SCALAR; NUMBER OF DAYS
ipmdat$D<-as.integer(max(effort$date)-(min(effort$date)))+1L
### VECTOR; DAY OF SAMPLING
ipmdat$tt<-sort(unique(effort$date-(min(effort$date)-1)))
### CAPTURE HISTORY FOR PIT AND ACOUSTIC
chipm<-rbind(ch_p[,-1],ch_a[,-1])
ipmdat$ch<-as.matrix(chipm)
ipmdat$ch[ipmdat$ch>1]<-1 ## multiple lengths
colnames(ipmdat$ch)<-NULL
ipmdat$ch<- rbind(ipmdat$ch,matrix(0,20,ncol(ipmdat$ch))) ## AUGMENT BY 20 ROWS
ipmdat$M<-nrow(ipmdat$ch)
ipmdat$secid<-effort$occasionId
ipmdat$dayid<-effort$dayId    ## DAYS EACH SECONDARY OCCASION OCCURRED ON.
ipmdat$nprim<-length(ipmdat$tt)
ipmdat$nocc<-length(effort$occasionId)

## MATRIX OF 0 OR 1; 1 IF KNOWN FOR SURE TO BE IN POOL
Z_known_p<-ch_p[,-1]
Z_known_p[Z_known_p>0]<-0
Z_known_p<-Z_known_p[,1:58]
colnames(Z_known_p)<-NULL
Z_known_a<-t(state_matrix[ipmdat$tt,])
Z_known_a[is.na(Z_known_a)]<-0
Z_known_a[Z_known_a==2]<-0
Z_known_a[Z_known_a==3]<-1
colnames(Z_known_a)<-NULL
rownames(Z_known_a)<-NULL
ipmdat$Z_known<- rbind(as.matrix(Z_known_p),as.matrix(Z_known_a))
ipmdat$Z_known<- rbind(ipmdat$Z_known,matrix(0,20,ncol(ipmdat$Z_known)))

## MATRIX; DAILY COVARIATE
ipmdat$X<-X
ipmdat$X[,2]<-scale(ipmdat$X[,2],center=mean(ipmdat$X[,2]),scale=sd(ipmdat$X[,2]))
ipmdat$X[,3]<-scale(ipmdat$X[,3],center=mean(ipmdat$X[,3]),scale=sd(ipmdat$X[,3]))
ipmdat$X[,4]<-scale(ipmdat$X[,4],center=mean(ipmdat$X[,4]),scale=sd(ipmdat$X[,4]))
ipmdat$X[,5]<-scale(ipmdat$X[,5],center=mean(ipmdat$X[,5]),scale=sd(ipmdat$X[,5]))


ipmdat$ncap<- lapply(1:ipmdat$nprim,function(x)
    {
    phys<-sum(apply(ipmdat$ch[,which(ipmdat$secid==x)],1,max))
    acou<- sum(ipmdat$Z_known[,x])
    both<- length(which(rowSums(cbind(apply(ipmdat$ch[,which(ipmdat$secid==x)],1,max),
        ipmdat$Z_known[,x]))==2))
    
    return(data.frame(phys=phys,acou=acou,both=both))
    })
ipmdat$ncap<-do.call("rbind",ipmdat$ncap)
ipmdat$ncap<- (ipmdat$ncap$phys+ipmdat$ncap$acou)-ipmdat$ncap$both  
save(ipmdat,file="_output/ipmdat-season.Rdata")
 
 
cha<-ipmdat$ch
cha[]<-0
for(i in 1:nrow(ipmdat$ch))
    {
    for(p in 1:ipmdat$nocc)
        {
        cha[i,p]<- ifelse(ipmdat$Z_known[i,ipmdat$secid[p]]==1,ipmdat$ch[i,p],0)
        ch[i,p]<- ifelse(ipmdat$Z_known[i,ipmdat$secid[p]]==1,0,ipmdat$ch[i,p],)
        }
    }

 
 
 