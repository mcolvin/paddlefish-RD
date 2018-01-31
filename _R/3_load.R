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

#comm<- odbcConnectAccess2007(file.path(Sys.getenv("USERPROFILE"),"Google Drive/Paddlefish/Paddlefish Database.accdb"))
comm<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Paddlefish/analysis/data/Paddlefish Database.accdb")

#######################################################################
#
# QUERY EFFORT DATA
#
#######################################################################
    
effort<- sqlQuery(comm,
    "SELECT [Paddlefish Effort Data].occasionId, [Paddlefish Effort Data].ID AS id, 
    [Paddlefish Effort Data].Date AS [date], 
    [Paddlefish Effort Data].set_number, 
    [Paddlefish Effort Data].effort, 
    [Paddlefish Effort Data].n_paddlefish
    FROM [Paddlefish Effort Data];")    

## FORMAT AND CLEAN UP EFFORT DATA
effort$doy<-as.numeric(format(effort$date, "%j"))
effort$year<-as.numeric(format(effort$date, "%Y"))

## FORMATTING INDICES
effort<- effort[order(effort$date,effort$set_number),]
effort$id<-c(1:nrow(effort))
effort$occasionid<- sequence(rle(effort$occasionId)$length)  


#primary<- aggregate(effort~occasionId+date+doy,effort,length)
#primary<- primary[order(primary$occasionId),]
#primary$dt<- c(0,round(diff(primary$date),0))
#primary<- primary[,-2]
#names(primary)[3]<- "secoccs"
#dat$primary<- primary
#dat$secondary<- effort[,-c(3,6,7,8)]


####################################################################### 
##   
## TAGGING DATA
##
#######################################################################
taggingData<- sqlQuery(comm,"SELECT [Paddlefish Tagging Data].ID, [Paddlefish Tagging Data].date, [Paddlefish Tagging Data].set AS set_number, [Paddlefish Tagging Data].tl, [Paddlefish Tagging Data].rfl, [Paddlefish Tagging Data].efl, [Paddlefish Tagging Data].girth, [Paddlefish Tagging Data].floy_color, [Paddlefish Tagging Data].floy_number, [Paddlefish Tagging Data].weight, [Paddlefish Tagging Data].pit, [Paddlefish Tagging Data].new_recap, [Paddlefish Tagging Data].transmitter, [Paddlefish Tagging Data].sex
FROM [Paddlefish Tagging Data];")


## ASSIGN OCCASSION ID 

tmp<-merge(taggingData,effort[,c(1:4)],by=c("date","set_number"),all.x=TRUE)
tmp<- tmp[which(is.na(tmp$occasionId)==FALSE),]
   

## DETERMINE WHICH FISH ARE ACOUSTICALLY TAGGED
acc_tags<- reshape2::dcast(tmp,pit~transmitter,value.var='id',length)
acc_tags<- acc_tags[,-which(names(acc_tags)=="NA")]  
acc_tags<- acc_tags$pit[which(rowSums(acc_tags[,-1])>0)]

   

## SET UP CAPTURE HISTORY FOR ALL FISH
ch<-reshape2::dcast(xx,pit~secid,value.var="tmp",sum,
    drop=FALSE)
ch_pit<- ch[which(!(ch$pit%in% acc_tags)),-1] 
ch_acc<- ch[which(ch$pit%in% acc_tags),-1]    
    
    
    
   
   
## MERGE SECONDARY ID
xx<-merge(taggingData,dat$secondary, by=c("occasionId","set_number"),all.x=TRUE)
xx$tmp<-1
xx$secid<-factor(xx$id.y,levels=dat$secondary$id)




   
# MAKE A MATRIX OF WHEN FISH ARE AVAILABLE
left<- data.frame(
    pit=c(1005085885, 1005085855, 1005085870, 1005085839, 1005085832, 1005085858,
        1005085863, 1005085855, 1005085842), 
    id_left=c(8, 9, 34, 37, 37, 37, 37, 37, 37),
    id_return=c(NA,11,NA,39,NA,NA,NA,NA,NA))
tags_ac<- merge(tags,left,by="pit",all.x=TRUE)



#######################################################################
# POOL RECIEVER DATA
#######################################################################

poolReciever<- sqlFetch(comm,"qry-pool-reciever-data")

recieverData<-sqlQuery(comm,'SELECT [Reciever Data].[Date and Time (UTC)] AS [date], 
    DatePart("yyyy",[Date and Time (UTC)]) AS [year], 
    DatePart("y",[Date and Time (UTC)]) AS doy, Right([Reciever Data.Transmitter],5) AS transmitter, [Reciever Data].[Site ID] AS siteId
    FROM [Reciever Data]
    WHERE ((([Reciever Data].[Site ID])="12"));')


# DATA AUGMENTATION
 
    


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
    
    