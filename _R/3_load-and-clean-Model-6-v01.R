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
## CAPTURE HISTORY FOR PIT TAG ONLY FISH
ch_p<-subset(ch,pit%in% tag$pit[tag$type=="p"])    
## CAPTURE HISTORY FOR ACOUSTIC TAG FISH
ch_a<-subset(ch,pit%in% tag$pit[tag$type=="a"])    
ch_a<-ch_a[match(acoustics$pit,ch_a$pit),]  

#ch_p<-array(0,180,max(effort$set_number),dat$nprim)
#
#pp<-reshape2::dcast(tmp,occasionId+date+set_number+pit~"n",value.var="tmp",sum)
#pp$n<-ifelse(pp$n>1,1,pp$n) ## fix fish with replicate meassurements

#for(i in 1:nrow(pp))
#    {
#    
#    }

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
    state_matrix[dates<=meta[i,]$Implantation,i]<- NA ## CENSOR PRE TAGGING
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






#######################################################################
#
#  BUNDLE UP DATA FOR JAGS
#
#######################################################################

 
 
dat<-list() 

## SCALAR; NUMBER OF DAYS
dat$D<-as.integer(max(effort$date)-(min(effort$date)))+1L
## VECTOR; DAY OF SAMPLING
dat$int_str<-sort(unique(effort$date-(min(effort$date)-1)))
## VECTOR; DAY BEFORE NEXT SAMPLING
dat$int_end<-dat$int_str[-1]-1
## SCALAR; NUMBER OF PRIMARY OCCASIONS
dat$nprim<-max(effort$occasionId)
## MATRIX; CAPTURE HISTORIES, IND FISH
dat$ch_a<-ch_a[,-1]
dat$ch_a[dat$ch_a>1]<-1 ## some fish have replicate length/weight data
dat$ch_p<-ch_p[,-1]
dat$ch_p[dat$ch_p>1]<-1 ##

dat$secid<-effort$occasionId
dat$dayid<-effort$dayId    ## DAYS EACH SECONDARY OCCASION OCCURRED ON.
dat$occId<- effort$set_number
## SCALAR; NUMBER OF TAGGED FISH
dat$M_a<-nrow(dat$ch_a)
dat$M_p<-nrow(dat$ch_p)
## SCALAR; NUMBER OF SECONDARY OCCASIONS
dat$nocc<-ncol(dat$ch_p)    
## MATRIX; DAILY COVARIATE
#dat$X<-X

## MATRIX; ACOUSTIC TAGS

dat$tag_state<-state_matrix 
dat$obs_state<-state_matrix 
dat$obs_state[dat$obs_state==3]<-1 
dat$obs_state[dat$obs_state==2]<-0 
dat$obs_state[is.na(dat$obs_state)]<-0 

dat$obs_state_p<- dat$obs_state
dat$obs_state_p[is.na(dat$obs_state_p)]<- 0

dat$ac_meta<-ac_meta[,-1]
dat$N_ac<- ncol(state_matrix)



## BREAK OUT PIT AND ACOUSTIC
tmp<-lapply(1:dat$nprim,function(x)
    {
    ## MAKE CAPTURE HISTORIES FOR PIT TAGGED FISH AND FAILED RECIEVER
    tmppp<- ch_p[,-1]
    tmppp<- tmppp[,which(dat$secid==x)]
    tmppp_app<-ch_a[,-1]
    tmppp_app<-tmppp_app[,which(dat$secid==x)] ## acoustic tag fish, either not tagged yet or tag failed
    tmppp<- rbind(tmppp[which(rowSums(tmppp)>0),],
        tmppp_app[which(Z_known_a[,x]==0& rowSums(tmppp_app)>0) ,])
    n=nrow(tmppp)
    nocc<- ncol(tmppp)    
    a<-matrix(0,175,12)  #  
    na=nrow(tmppp)
    nocc<- ncol(tmppp)
    if(n>0)
        {
        a[1:na,1:nocc]<-as.matrix(tmppp[1:na,1:nocc])
        }

    
    ## ACOUSTICALLY TAGGED FISH THAT WERE AVAILIBLE FOR CAPTURE
    tmppp<- ch_a[,-1]
    tmppp<- ch_a[,which(dat$secid==x)]
    tmppp<-tmppp[which(Z_known_a[,x]==1),]
    b<-matrix(0,58,12)
    p<-rep(0,58)
    nb=nrow(tmppp)
    if(n>0)
        {
        b[1:nb,1:nocc]<-as.matrix(tmppp[1:nb,1:nocc])
        p[1:nb]<-1
        }
    return(list(a=a,na=na, b=b,nb=nb,p=p,nocc=nocc))
    })

    
a<-array(0,c(175,12,58))
b<-array(0,c(58,12,58))
nocc<-nb<-rep(0,58)

for(i in 1:dat$nprim)
    {
    a[,,i]<- as.matrix(tmp[[i]]$a)
    b[,,i]<- as.matrix(tmp[[i]]$b)
    nb[i]<-tmp[[i]]$nb
    nocc[i]<-tmp[[i]]$nocc
    }
dat<-list()
dat$nprime<- max(effort$occasionId)
a[a>1]<-1
dat$a<-a
b[b>1]<-1
dat$b<-b
dat$nb<- nb
dat$na<- 175
dat$nocc<- nocc

