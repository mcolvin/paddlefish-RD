#######################################################################
# NOTES
#######################################################################


# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(11532,11529)
# pit==1005085830



#######################################################################
# COM TO DBASE
#######################################################################

comm<- odbcConnectAccess2007("C:/Users/mcolvin/Google Drive/Paddlefish/Paddlefish Database.accdb")




#######################################################################
# EFFORT DATA
#######################################################################

effort<- sqlFetch(comm,
    'qry-effort-data')
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

taggingData<- sqlFetch(comm,
    "qry-tagging-data")

## EXTRACT DAY OF YEAR FROM DATE
taggingData$doy<-as.numeric(
    format(taggingData$Date, "%j"))
## EXTRACT YEAR FROM DATE
taggingData$year<-as.numeric(
    format(taggingData$Date, "%Y"))
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in reciever
#taggingData<- subset(taggingData, !(year==2016 & doy < 78| year==2015))



#######################################################################
# POOL RECIEVER DATA
#######################################################################

poolReciever<- sqlFetch(comm,
    "qry-pool-reciever-data")
# FORMAT AND CLEAN UP POOL RECIEVER
## DATA FIX TRANSMITTER CODE
## FIX NON-UNIFORM TRANSMITTER CODES
poolReciever$date<-as.POSIXct(poolReciever$date)

## KEEP ALL DATA TO CHECK DAILY IN/OUT
poolReciever_all<-poolReciever
poolReciever_all$doy<- as.numeric(
    format(poolReciever_all$date, "%j"))
poolReciever_all$year<- as.numeric(
    format(poolReciever_all$date, "%Y"))
 poolReciever_all<-poolReciever_all[order(poolReciever_all$date),]   
## SUBSET OUT DAYS THAT MATCH EFFORT OCCASIONS
poolReciever<- subset(poolReciever, date %in% unique(effort$date))

poolReciever$doy<- as.numeric(
    format(poolReciever$date, "%j"))
poolReciever$year<- as.numeric(
    format(poolReciever$date, "%Y"))
poolReciever$set<- 0  # FOR SETTING UP FIRST OCCASION
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in receiver
#poolReciever<- subset(poolReciever, !(year==2016 & doy < 78| year==2015))

poolReciever<-poolReciever[order(poolReciever$date),]
    
    

    
    
#######################################################################
# ACOUSTIC TAG DATA
#######################################################################

tags<- sqlFetch(comm,
    'qry-transmitter-number')
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




#######################################################################
# SET UP DATA FOR ROBUST DESIGN 
# IN PROGRAM MARK
#######################################################################
poolReciever<- merge(poolReciever,tags[,c(1,6)],by="transmitter")
poolReciever_all<- merge(poolReciever_all,tags[,c(1,6)],by="transmitter")
xx<- rbind.fill(taggingData,poolReciever)
xx$tmp<- 1 # DUMMY FOR SUMMARY


## OCCASIONS VECTOR FOR RMARK
## RECIEVER OCCASIONS ARE iNCLUDED
occasions<- dcast(effort, 
    year+doy~"sets",
    value.var="set_number",
    fun=length)
occ<- unlist(lapply(1:nrow(occasions),
    function(x){c(rep(0,occasions$sets[x]),1)}))# ADD 1 TO THE END TO ACCOUNT FOR ACOUSTIC
occ<- occ[-length(occ)]# drop last 1 for Rmark format
all_occasions<- unlist(lapply(1:nrow(occasions),
    function(x){
        paste(occasions$year[x],
            occasions$doy[x],
            c(0,1:occasions$sets[x]),
            sep="_")
    }))

## RECIEVER OCCASIONS ARE iNCLUDED    

occ_noac<- unlist(lapply(1:nrow(occasions),
    function(x){c(rep(0,occasions$sets[x]-1),1)}))# ADD 1 TO THE END TO ACCOUNT FOR ACOUSTIC
occ_noac<- occ_noac[-length(occ_noac)]# drop last 1 for Rmark format

          
        
## THE BIG MAMA JAMMA - THE WHOLE ENCHILADA
names(occasions)[3]<- "set"
xxx<- merge(xx, occasions, 
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
    fun=mean)
ch[is.na(ch)]<-0
zeros<-apply(ch[,-1],1,max)
ch<-ch[which(zeros==1),]


## FISH LEVEL COVARIATES
fish<- data.frame(pit=ch[,1])
fish<- merge(fish,tags,by='pit',all.x=TRUE)
fish$id<- ifelse(is.na(fish$id),0,fish$id)
## MAKE SURE EVERYBODY MATCHES UP
ch<- ch[order(ch$pit),]
fish<- fish[order(fish$pit),]
ch_raw<-ch
# BUNLDLE THIS UP 
ch<- data.frame(ch=apply(ch[,-1],1,paste,collapse=""),
    freq=1,
    first_captured=fish$id,
    stringsAsFactors=FALSE)
 
ch_noac<- data.frame(ch=apply(ch_raw[,-c(1,grep("_0",names(ch_raw)))],1,paste,collapse=""),
    freq=1,
    first_captured=fish$id,
    stringsAsFactors=FALSE)
