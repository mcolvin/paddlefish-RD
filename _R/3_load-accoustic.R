#######################################################################
# NOTES
#######################################################################


# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(11532,11529)
# pit==1005085830

dat<-list()

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
    
    
 
dat<- sqlFetch(comm, "SELECT [Reciever Data].[Site ID], [Reciever Data].[Date and Time (UTC)], [Reciever Data].Transmitter, [Reciever Data].[Transmitter Name] FROM [Transmitter Numbers] INNER JOIN [Reciever Data] ON [Transmitter Numbers].Transmitter = [Reciever Data].Transmitter WHERE ((([Reciever Data].[Site ID])='12'));")
 
    
    
    
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

#######################################################################
#
#  LIST OF PIT TAGS FOR ACCOUSTIC TAGGED FISH
#
####################################################################### 
   
## DETERMINE WHICH FISH ARE ACOUSTICALLY TAGGED
acc_tags<- reshape2::dcast(tmp,pit+transmitter_long~"n",value.var='id',length)
## SUBSET OUT ACOUSTICALLY TAGGED FISH
acc_tags<- acc_tags[!(is.na(acc_tags$transmitter=="NA")),]  



#######################################################################
#
# POOL RECIEVER DATA
#
#######################################################################
acNumbers<- sqlFetch(comm,"Transmitter Numbers")

poolReciever<- sqlFetch(comm,"qry-pool-reciever-data")
## SUBSET TAGS WE USED ONLY, ELMINATE TAG COLLISIONS
poolReciever<- subset(poolReciever,transmitter%in% acNumbers$Transmitter)
poolReciever$transmitter<- factor(poolReciever$transmitter)
# ONE FISH HAS 2 TRANSMITTERS IN IT
# TRASMITTTER %IN% C(A69-1303-11532,A69-1303-11529)


    


dat<-list(
    ## PIT TAGGED FISH
    N=N,
    pocc=as.matrix(occasions),
    socc=as.matrix(socc),
    fish=as.matrix(fish),
    ch=as.matrix(ch),
    ## ACOUSTIC FISH
    ch_acoustic=as.matrix(ch_acoustic),
    ch_acoustic=as.matrix(ch_acoustic),
    receiverOn=receiverOn
    #fish_acoustic=fish_acoustic[1,2]
    )
    
save(dat ,file="full.RData")
    
    