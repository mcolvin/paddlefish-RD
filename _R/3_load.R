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
effort<- subset(effort, !(year==2016 & doy < 78| year==2015))



#######################################################################    
# TAGGING DATA
#######################################################################

taggingData<- sqlFetch(comm,
    "qry-tagging-data")

## EXTRACT DAY OF YEAR FROM DATE
taggingData$doy<-as.numeric(
    format(taggingData$date, "%j"))
## EXTRACT YEAR FROM DATE
taggingData$year<-as.numeric(
    format(taggingData$date, "%Y"))
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in reciever
taggingData<- subset(taggingData, !(year==2016 & doy < 78| year==2015))



#######################################################################
# POOL RECIEVER DATA
#######################################################################

poolReciever<- sqlFetch(comm,
    "qry-pool-reciever-data")
# FORMAT AND CLEAN UP POOL RECIEVER
## DATA FIX TRANSMITTER CODE
## FIX NON-UNIFORM TRANSMITTER CODES
poolReciever$date<-as.POSIXct(poolReciever$date)
## SUBSET OUT DAYS THAT MATCH EFFORT OCCASIONS
poolReciever<- subset(poolReciever, date %in% unique(effort$date))

poolReciever$doy<- as.numeric(
    format(poolReciever$date, "%j"))
poolReciever$year<- as.numeric(
    format(poolReciever$date, "%Y"))
poolReciever$set<- 0  # FOR SETTING UP FIRST OCCASION
## SUBSET OCCASIONS PRIOR TO
## MARCH 2016 (when we put in reciever
poolReciever<- subset(poolReciever, !(year==2016 & doy < 78| year==2015))


    
    

    
    
#######################################################################
# ACOUSTIC TAG DATA
#######################################################################

tags<- sqlFetch(comm,
    'qry-transmitter-number')
## ADD A PIT TAG NUMBER TO EACH TRANSMITTER
pit<- dcast(taggingData,pit+transmitter~"yy",value.var="transmitter",length)[,-3]
pit<- subset(pit, !(is.na(transmitter )))
tags<- merge(tags,pit,by="transmitter")




#######################################################################
# SET UP DATA FOR ROBUST DESIGN 
# IN PROGRAM MARK
#######################################################################
poolReciever<- merge(poolReciever,tags[,c(1,4)],by="transmitter")

xx<- rbind.fill(taggingData,poolReciever)
xx$tmp<- 1 # DUMMY FOR SUMMARY


## OCCASIONS VECTOR FOR RMARK
occasions<- dcast(effort, 
    year+doy~"sets",
    value.var="set_number",
    fun=length)
occ<- unlist(lapply(1:nrow(occasions),
    function(x){c(rep(0,occasions$sets[x]),1)}))
occ<- occ[-length(occ)]# drop last 1 for Rmark format
all_occasions<- unlist(lapply(1:nrow(occasions),
    function(x){
        paste(occasions$year[x],
            occasions$doy[x],
            c(0,1:occasions$sets[x]),
            sep="_")
    }))

        
        
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
fish$acoustic<- as.factor(ifelse(is.na(fish$transmitter)==TRUE,"No","Yes"))

## MAKE SURE EVERYBODY MATCHES UP
ch<- ch[order(ch$pit),]
fish<- fish[order(fish$pit),]

# BUNLDLE THIS UP 
ch<- data.frame(ch=apply(ch[,-1],1,paste,collapse=""),
    freq=1,
    acoustic=fish$acoustic,
    stringsAsFactors=FALSE)
 

