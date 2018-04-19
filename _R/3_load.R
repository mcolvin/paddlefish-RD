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

comm<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Paddlefish/analysis/_dat/Paddlefish Database.accdb")

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

dat$ch<-ch
dat$occasionId<-effort$occasionId
    
    
    
    
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
#  CAPTURE HISTORIES FOR PIT AND ACCOUSTIC TAGGED FISH
#
#######################################################################

ch_pit<- ch[which(!(ch$pit%in% acc_tags)),-1] 
ch_acc<- ch[which(ch$pit%in% acc_tags$pit),-1]    
ch_pit_pit<- ch[which(!(ch$pit%in% acc_tags)),1]   
ch_acc_pit<- ch[which(ch$pit%in% acc_tags$pit),1]     
    
 
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


dailyEffort<-aggregate(n_paddlefish~occasionId+doy+year,effort,sum)


poolReciever<-merge(poolReciever,dailyEffort,by=c("year","doy"),all.x=TRUE)  
## SUBSET OUT DAYS THAT WERE NOT EFFORT
poolReciever<- subset(poolReciever, !(is.na(n_paddlefish)))

## WAS THE FISH DETECTED ON THE POOL RECIEVER ON THE SAMPLING OCCASION
ppp<- reshape2::dcast(poolReciever,transmitter~occasionId,
    value.var='n_paddlefish',length,drop=FALSE)

## PRIMARY OCCASIONS
prim<- unique(effort$occasionId)   
prim<- prim[prim>5]    
    
    
dat<-lapply(prim,function(x)
    {
    ## GET FIRST TO LAST OCCASION, WHAT WAS AT LARGE
    pit<- ch_pit[,c(1:match(max(as.numeric(as.character(effort[effort$occasionId==prim[x],]$secid))),as.numeric(as.character(names(ch_pit)))))]
    acc<- ch_acc[,c(1:match(max(as.numeric(as.character(effort[effort$occasionId==prim[x],]$secid))),as.numeric(as.character(names(ch_pit)))))]
    
    # CAPTURE HISTORIES
    
    ## PIT TAG ONLY FISH
    ## PHYSICALLY CAPTURED
    pit_indx<- which(rowSums(pit)>0)
    pit<- pit[pit_indx,]
    
    ## ACCOUSTIC TAGGED FISH
    ## PHYSICALLY CAPTURED
    sec_indx<- as.numeric(as.character(effort[effort$occasionId==prim[x],]$secid))
    acc_indx<- which(rowSums(acc)>0)
    acc<- acc[acc_indx,sec_indx]
    acc_pit<-ch_acc_pit[which(rowSums(acc)>=1)]
    ## SUBSET ACCOUSTICALLY TAGGED FISH BY ONES THAT WERE
    ## AROUND TO BE CAPTURED
    ppp_col_indx<- match(as.character(x),names(ppp))
    ppp_row_indx<- which(ppp[,ppp_col_indx]>0)
    pitAvailible<- subset(acc_tags,acc_tags$transmitter_long%in% as.character(ppp[ppp_row_indx,1]))
    
    ## ACOUSTIC FISH AVAILABIBLE AT THE OCCASION
    ## VECTOR OF TRANSMITTERS
    
    
    
    ppp[which(ppp$transmitter%in% pitAvailible$transmitter_long),]$transmitter
    
    ## 
    
    
    })
    
    
  

  
    
    
    


   
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
    
    