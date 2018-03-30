

## GO BACK TO M0.
makeData_MO<- function(occasion,...)
    {
    ## FISH ON RECIEVER
    onReciever<- pp[,c(1,which(names(pp)==occasion))]
    names(onReciever)<-c("pit","detected")
    onReciever<- subset(onReciever,detected==1)

    sets<- subset(ggg,id==occasion&set>0)
    sets<-as.character(paste(sets$year,sets$doy,sets$set,sep="_"))
    ch_dat<- subset(xxx,id==occasion & set>0)
    ch_dat$occ_id<- factor(ch_dat$occ_id,
        levels=sets)
    if(nrow(ch_dat)>0 & !is.null(ncol(onReciever)))
        {

        ch<- dcast(ch_dat, pit~occ_id,
            value.var='tmp',
            drop=FALSE,
            fun=sum)
        ch<-merge(onReciever,ch,by="pit",all=TRUE)    
        ch$detected<-ifelse(is.na(ch$detected==1),2,ch$detected)  
        ch[is.na(ch)]<-0  
        ch<-as.matrix(ch)
        
        cha<-ch[ch[,2]==1,-c(1:2)]
        #chp<-as.matrix(ch[ch[,2]==2,-c(1:2)],nrow=length(which(ch$detected==2)) ,ncol=ncol(cha))
        chp<-ch[ch[,2]==2,-c(1:2),drop=FALSE]
        dat_aug<- 100
        z<- c(rep(1,nrow(chp)),rep(0,dat_aug-nrow(chp)))
        chp<-rbind(chp,matrix(0,nrow=dat_aug-nrow(chp) ,ncol(chp)))# data augmentation needs to be matrix of 0s not NAs for JAGs
        out<- list(ch=as.matrix(chp),
            cha=as.matrix(cha), 
            Ncha=nrow(cha),
            occ=ncol(chp),
            M=nrow(chp))
        
        return(out)
        }else{return(list(ret=-1))}
    }
    



#######################################################################
#
#  M0 MODEL
#
#######################################################################
NN<-rep(NA,40)
indx<- c(4:7,10,16,19:40)
for(i in indx)
    {

    dat<-makeData_MO(i) 
    if(is.null(dat$ret)&sum(dat$ch)>0)
        {
        z<- rep(1,nrow(dat$ch))
        inits<- function()
            {
            list(a=-2.5,omega=0.5,z=z)
            }
        params<-c("a","N","omega","p_cap")

            # THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
            # AND THE R2jags PACKAGE
            out <- try(jags(data=dat,
                inits=inits,
                parameters=params,	
                model.file=mod,
                n.chains = 3,	
                n.iter = 15000,	
                n.burnin = 6000, 
                n.thin=2,
                working.directory=getwd()))
            NN[i]<-if(class(out)!="try-error") {out$BUGSoutput$mean$N}
        }
    }
   

out<-cbind(primary_occasions,NN)

plot(NN~year_f,out,ylim=c(0,125))
presPlot()
plot(NN~year_f,out,ylim=c(0,125),type='h',
    ylab="Abundance",xlab="Year",lwd=5)

    
    
makeData_MO(40)