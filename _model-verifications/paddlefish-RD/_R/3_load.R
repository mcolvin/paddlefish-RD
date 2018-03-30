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

comm<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Paddlefish/analysis/data/Paddlefish Database.accdb")

#######################################################################
#
# QUERY EFFORT DATA
#
#######################################################################
    
effort<- sqlFetch(comm,"qry-effort-data")    

## FORMAT AND CLEAN UP EFFORT DATA
effort$doy<-as.numeric(format(effort$date, "%j"))
effort$year<-as.numeric(format(effort$date, "%Y"))

## FORMATTING INDICES
effort<- effort[order(effort$date, effort$set_number),]
#effort$sample<- paste(effort$year,effort$doy

effort <- transform(effort,occasionId=as.numeric(factor(date)))
effort$secid<-as.factor(c(1:nrow(effort)))## SECONDARY OCCASION ID

####################################################################### 
##   
## QUERY TAGGING DATA
##
#######################################################################

## QUERY TAGGING DATA
#taggingData<- sqlQuery(comm,"SELECT [Paddlefish Tagging Data].ID, [Paddlefish Tagging Data].date, [Paddlefish Tagging Data].set AS set_number, [Paddlefish Tagging Data].tl, [Paddlefish Tagging Data].rfl, [Paddlefish Tagging Data].efl, [Paddlefish Tagging Data].girth, [Paddlefish Tagging Data].floy_color, [Paddlefish Tagging Data].floy_number, [Paddlefish Tagging Data].weight, [Paddlefish Tagging Data].pit, [Paddlefish Tagging Data].new_recap, [Paddlefish Tagging Data].transmitter, [Paddlefish Tagging Data].sex FROM [Paddlefish Tagging Data];")
taggingData<-sqlFetch(comm,"qry-capture-data")
taggingData$year<- format(taggingData$Date,"%Y")
taggingData$doy<- as.numeric(as.character(format(taggingData$Date,"%j")))
## ASSIGN OCCASSION ID TO MASTER CAPTURE RECAPTURE DATASET
tmp<-merge(taggingData,effort,by=c("year","doy","set_number"),all.x=TRUE)
tmp<- tmp[which(is.na(tmp$occasionId)==FALSE),]
tmp$tmp<-1
## SET UP CAPTURE HISTORY FOR ALL FISH
ch<-reshape2::dcast(tmp,pit~secid,value.var="tmp",sum,
    drop=FALSE)

    
dat<-list()
## LAST STUDY DAY
dat$D<- as.integer(max(effort$date)-min(effort$date))+1
## INTERVAL IN DAYS BETWEEN EFFORTS
intervals<- sort(unique(effort$date))
indx<- as.integer(round((intervals-intervals[1])/86400,0))+1
indx<-cbind(indx[-length(indx)],indx[-1])
dat$int_str=indx[,1]
dat$int_end=indx[,2]

## DAILY COVARIATES
## dat$X<- temp

## CAPTURE HISTORY
dat$ch<-ch[,-1]
dat$ch[dat$ch>1]<-1 ## some fish have replicate length/weight data

## NUMBER OF PRIMARY OCCASIONS
dat$nprim<-max(effort$occasionId)
dat$secid<-effort$occasionId
dat$M<-nrow(dat$ch)
dat$nocc<-ncol(dat$ch)


