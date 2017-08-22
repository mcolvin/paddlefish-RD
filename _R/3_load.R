#######################################################################
# NOTES
#######################################################################


# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(11532,11529)
# pit==1005085830



#######################################################################
# COM TO DBASE
#######################################################################

comm<- odbcConnectAccess2007("C:/Users/mcolvin/Google Drive/Paddlefish/Paddlefish Database-2.accdb")




#######################################################################
# EFFORT DATA
#######################################################################

#effort<- sqlFetch(comm,
#    'qry-effort-data')
    
effort<- sqlQuery(comm,
    "SELECT [Paddlefish Effort Data].occasionId, [Paddlefish Effort Data].ID AS id, [Paddlefish Effort Data].Date AS [date], [Paddlefish Effort Data].set_number, [Paddlefish Effort Data].effort, [Paddlefish Effort Data].n_paddlefish
FROM [Paddlefish Effort Data];")    
    
## FORMAT AND CLEAN UP EFFORT DATA
effort$doy<-as.numeric(format(effort$date, "%j"))
effort$year<-as.numeric(format(effort$date, "%Y"))
## THESE ARE SETS THAT WERE FISHED 
## CONCURRENT WITH ANOTHER SET
effort<- subset(effort,set_number!=9999)
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in reciever
#effort<- subset(effort, !(year==2016 & doy < 78| year==2015))

## SUMMARIZE PRIMARY OCCASIONS
primary_occasions<- data.frame(
    occasion=unique(paste(effort$year,effort$doy,sep="_")))
primary_occasions$id<- 1:nrow(primary_occasions)
primary_occasions$year_f<- as.numeric(substr(primary_occasions$occasion,1,4))+
    (as.numeric(substr(primary_occasions$occasion,6,nchar(as.character(primary_occasions$occasion))))/365)
#######################################################################    
# TAGGING DATA
#######################################################################

#taggingData<- sqlFetch(comm,
#   "qry-tagging-data")

taggingData<- sqlQuery(comm,"SELECT [Paddlefish Tagging Data].ID AS id, [Paddlefish Tagging Data].Date, [Paddlefish Tagging Data].set, [Paddlefish Tagging Data].tl, [Paddlefish Tagging Data].rfl, [Paddlefish Tagging Data].efl, [Paddlefish Tagging Data].girth, [Paddlefish Tagging Data].floy_color, [Paddlefish Tagging Data].floy_number, [Paddlefish Tagging Data].weight, [Paddlefish Tagging Data].pit, [Paddlefish Tagging Data].new_recap, [Paddlefish Tagging Data].transmitter, [Paddlefish Tagging Data].sex, [Paddlefish Tagging Data].setId
FROM [Paddlefish Tagging Data]
ORDER BY [Paddlefish Tagging Data].Date;")

## EXTRACT DAY OF YEAR FROM DATE
taggingData$doy<-as.numeric(
    format(taggingData$Date, "%j"))
## EXTRACT YEAR FROM DATE
taggingData$year<-as.numeric(
    format(taggingData$Date, "%Y"))
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in reciever
#taggingData<- subset(taggingData, !(year==2016 & doy < 78| year==2015))
taggingData<- subset(taggingData, Date %in% unique(effort$date))
#######################################################################
# ACOUSTIC TAG DATA
#######################################################################

#tags<- sqlFetch(comm,
#    'qry-transmitter-number')
tags<-sqlQuery(comm,"SELECT Right([Transmitter Numbers.Transmitter],5) AS transmitter, [Transmitter Numbers].[Tag Number] AS tagNumber, [Transmitter Numbers].[Implantation Date] AS implantDate
FROM [Transmitter Numbers];")


tags$first_tagged<- paste(
    format(tags$implantDate,"%Y"),
    as.numeric(format(tags$implantDate,"%j")),
    sep="_")
tags<-merge(tags,primary_occasions  
    ,by.x="first_tagged",
    by.y="occasion",all.x=TRUE)
## ADD A PIT TAG NUMBER TO EACH TRANSMITTER
pit<- dcast(taggingData,pit+transmitter~"yy",
    value.var="transmitter",length)[,-3]
pit<- subset(pit, !(is.na(transmitter )))
tags<- merge(tags,pit,by="transmitter")
tags$year_tagged<- as.numeric(format(tags$implantDate,"%Y"))
tags$doy_tagged<- as.numeric(format(tags$implantDate,"%j"))


#######################################################################
# POOL RECIEVER DATA
#######################################################################

#poolReciever<- sqlFetch(comm,
#    "qry-pool-reciever-data")

#recieverData<-sqlQuery(comm,"SELECT [Reciever Data].[Date and Time (UTC)] AS [date], 
#    Right([Reciever Data.Transmitter],5) AS transmitter, 
 #   [Reciever Data].[Site ID] AS siteId
#    FROM [Reciever Data]
#    WHERE ((([Reciever Data].[Site ID])='12'));")
#saveRDS(recieverData,file="_output/recieverData.RDS")
 
recieverData<- readRDS("_output/recieverData.RDS")

# SUBSET OUR TAGS AND POOL SITE = 12
poolReciever<-subset(recieverData, transmitter %in%
    tags$transmitter & siteId==12)
## MERGE PIT TAGS WITH RECIEVER DATA
## TO LINK TO CAPTURE DATA
poolReciever<- merge(poolReciever,
    tags[,c(1,3,7,8,9)],
    by="transmitter")
poolReciever$doy<- as.numeric(
    format(poolReciever$date, "%j"))
poolReciever$year<- as.numeric(
    format(poolReciever$date, "%Y"))
poolReciever$set<- 0  # FOR SETTING UP FIRST OCCASION
    
    
## KEEP ALL DATA TO CHECK DAILY IN/OUT
poolReciever_all<-poolReciever

## DELETE RECORDS LESS THAN OR EQUAL TO THE DATE OF IMPLANT
#poolReciever<- subset(poolReciever, doy<=doy_tagged & year<=year_tagged)
#poolReciever$del<- ifelse(poolReciever$doy<=poolReciever$doy_tagged 
 #   & poolReciever$year<=poolReciever$year_tagged,1,0)


## SUBSET OUT DAYS THAT MATCH EFFORT OCCASIONS
poolReciever<- subset(poolReciever, date %in% unique(effort$date))
poolReciever<-poolReciever[order(poolReciever$date),]


    
    
    
#######################################################################
# SET UP DATA FOR ROBUST DESIGN 
# IN PROGRAM MARK
#######################################################################

ttt<- taggingData[,match(c("pit","year","doy","set"),names(taggingData))]
rrr<- poolReciever[,match(c("pit","year","doy","set"),names(poolReciever))]

xx<-rbind(ttt,rrr)
xx$tmp<-1

## OCCASIONS VECTOR FOR RMARK
## RECIEVER OCCASIONS ARE iNCLUDED
occasions<- dcast(effort, 
    year+doy~"sets",
    value.var="set_number",
    fun=length)
## VECTOR OF OCCASIONS FOR RMARK
occ<- unlist(lapply(1:nrow(occasions),
    function(x){c(rep(0,occasions$sets[x]),1)}))# ADD 1 TO THE END TO ACCOUNT FOR ACOUSTIC
occ<- occ[-length(occ)]# drop last 1 for Rmark format
## ALL OCCASIONS
all_occasions<- unlist(lapply(1:nrow(occasions),
    function(x){
        paste(occasions$year[x],
            occasions$doy[x],
            c(0,1:occasions$sets[x]),
            sep="_")
    }))


## THE BIG MAMA JAMMA - THE WHOLE ENCHILADA
names(occasions)[3]<- "set"
## EXPAND OCCASIONS
ggg<-lapply(1:nrow(occasions),function(x)
    {
    tmp<-occasions[rep(x,occasions$set[x]+1),]
    tmp$set<-c(0:(nrow(tmp)-1))
    return(tmp)
    })
ggg<-do.call("rbind",ggg)
xxx<- merge(xx, ggg, 
    by=c("year","doy","set"),
    all=TRUE)# FILLS EMPTY NETS

xxx$tmp<- ifelse(xxx$tmp==1,1,0)
xxx$occ_id<- factor(paste(xxx$year,xxx$doy,xxx$set,sep="_"),
    levels=all_occasions)
## DROP NA OCC_ID, THESE ARE FISH THAT WERE
## CAPTURED FROM SIMULTANEOUSLY FISHED NETS
xxx<- xxx[!(is.na(xxx$occ_id)),]
xxx<- xxx[!(is.na(xxx$tmp)),]

ch<- dcast(xxx, pit~occ_id,
    value.var='tmp',
    drop=FALSE,
    fun=sum)
ch[is.na(ch)]<-0
pit<-ch[,1]
ch[ch>0]<-1

## FISH LEVEL COVARIATES
fish<- data.frame(pit=pit)
fish<- merge(fish,tags,by='pit',all.x=TRUE)
fish$id<- ifelse(is.na(fish$id),0,fish$id)
## MAKE SURE EVERYBODY MATCHES UP
ch<- ch[order(pit),]
fish<- fish[order(fish$pit),]
ch_raw<-ch
# BUNLDLE THIS UP 
ch_dat<- data.frame(ch=apply(ch[,-1],1,paste,collapse=""),
    freq=1,
    first_captured=fish$id,
    stringsAsFactors=FALSE)

occ_indx<- which(c(occ,1)==1)
occ_strt<- occ_indx-c(occ_indx[1]-1, (diff(occ_indx)-1))

ncap<-c()
for(i in 1:length(occ_indx))
    {
    ncap<-c(ncap, sum(apply(ch_raw[,occ_strt[i]:occ_indx[i]],1,max)))
    }

#######################################################################
## only do last occasion
#######################################################################
ch_last<-ch
acoustics<-grep("_0",names(ch))
ch_last<- ch[,-acoustics[-length(acoustics)]]
indx<-which(rowSums(ch_last[,-1])>0)
fish_dat<- fish[indx,]
ch_last<-ch_last[indx,]
# BUNLDLE THIS UP 
ch_last_dat<- data.frame(ch=apply(ch_last[,-1],1,paste,collapse=""),
    freq=1,
    acoustic=ifelse(fish$id>0,1,0),
    stringsAsFactors=FALSE)

occ_last<- rep(0,ncol(ch_last)-1)
xx<-sapply(names(ch_last),function(x) unlist(strsplit(x,"_"))[3])
occ_strt<-which(xx==1)-1
occ_strt<-occ_strt-1

occ_indx<- occ_strt[occ_strt>0]
occ_last[occ_indx]<-1
occ_last<-occ_last[-length(occ_last)]

odbcClose(comm)




#######################################################################
# BUNDLE UP FOR JAGS
#######################################################################

## PRIMARY AND SECONDARY OCCASIONS
occasions<- dcast(effort, 
    year+doy~"sets",
    value.var="set_number",
    fun=length)
occasions$pocc<-c(1:nrow(occasions))
names(occasions)[3]<-"nsocc"

socc<- lapply(1:nrow(occasions),function(x)
    {
    data.frame(pocc=rep(occasions$pocc[x],occasions$nsocc[x]),
        socc=c(1:occasions$nsocc[x]))
    })
socc<-do.call("rbind",socc)
socc$occId<-c(1:nrow(socc))
## PRIMARY OCCASIONS [PHYSICAL CAPTURES]
    
ch<- dcast(xxx, pit~occ_id,
    value.var='tmp',
    drop=FALSE,
    fun=sum)
ch[is.na(ch)]<-0
pit<-ch[,1]
ch<-ch[,-1]
ch[ch>0]<-1


#######################################################################
## ACOUSTIC DATASET
#######################################################################

ch_acoustic<- ch[,grep("_0",names(ch))]
receiverOn<-rep(1,ncol(ch_acoustic))
receiverOn[c(1,2,3,8,9,17,18)]<- 0

## FISH LEVEL COVARIATES
fish<- data.frame(pit=pit)
fish<- merge(fish,tags,by='pit',all.x=TRUE)
fish$id<- ifelse(is.na(fish$id),0,fish$id)
## MAKE SURE EVERYBODY MATCHES UP
ch_acoustic<- ch_acoustic[order(pit),]
fish_acoustic<- fish[order(fish$pit),]
## DROP PIT TAGGED FISH
indx<-which(rowSums(ch_acoustic)>0)
ch_acoustic<- ch_acoustic[indx,]
fish_acoustic<- fish_acoustic[indx,]

#######################################################################
## PIT TAGGED DATASET
#######################################################################

ch<- ch[,-grep("_0",names(ch))]    
N<- round(nrow(ch)*2.5,0)
M<- nrow(ch)


# DATA AUGMENTATION
ch<- rbind(as.matrix(ch),
    matrix(0,N-M,ncol(ch)))
fish<-data.frame(id=c(1:nrow(ch)),
    pit=c(pit,rep(0,N-M)))   



dat<-list(
    N=N,
    pocc=as.matrix(occasions),
    socc=as.matrix(socc),
    fish=as.matrix(fish),
    ch=as.matrix(ch),
    
    ch_acoustic=as.matrix(ch_acoustic),
    ch_acoustic=as.matrix(ch_acoustic),
    receiverOn=receiverOn
    #fish_acoustic=fish_acoustic[1,2]
    )
    
    
    
    