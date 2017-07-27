
rd<- process.data(data=ch, 
    groups=c("first_captured"),# 
    model="Robust", 
    time.intervals=occ) 
rd_ddl<-make.design.data(rd)

# PLACES TO SET TO 1
## ACOUSTICALLY TAGGED FISH IN SESSION 1
indx1<-c()
indx0<-c()
for(i in 1:max(ch$first_captured))
    {
    if(i <5)# receiver went in on occasion 5
        {
        indx1<-c(indx1,which(rd_ddl$p$first_capture==i & 
            rd_ddl$p$session %in% c(5:max(primary_occasions$id)) & 
            rd_ddl$p$time==1))         
        }
    if(i>=5)# fish going in after the reciever was put in
        {
         indx1<-c(indx1,which(rd_ddl$p$first_capture==i & 
            rd_ddl$p$session %in% c((i+1):max(primary_occasions$id)) & 
            rd_ddl$p$time==1))       
        }
    }  

for(i in 5:max(ch$first_captured))
    {    
    indx0<-c(indx0, which(rd_ddl$p$first_capture==i & 
        rd_ddl$p$session %in% c(1:i) & 
        rd_ddl$p$time==1))
    }
## BACKFILL 0S WHERE RECIEVER WAS NOT OUT
indx0<-c(indx0, 
    which(rd_ddl$p$first_capture%in%c(0:4) & 
        rd_ddl$p$session %in% c(1:4) & 
        rd_ddl$p$time==1))
indx0<-c(indx0, 
    which(rd_ddl$p$first_capture==0 & rd_ddl$p$time==1))
table(indx1%in%indx0)
table(indx0%in%indx1)

## BREAK APART WHETHER FISH ARE PIT ONLY OR NOT
rd_ddl$p$pbin<-0
rd_ddl$c$pbin<-0
rd_ddl$p$pbin[which(rd_ddl$p$time==1)]<-1

## SET VALUES TO BE CLOSE TO 1 OR 0 AS IT MESSES UP 
## THE ESTIMATES OF S AND GAMMA PRIME
p=list(formula=~pbin:first_captured+session,share=TRUE,
    fixed= list(
        index =c(indx1,indx0), 
        value= c(rep(0.999,length(indx1)),rep(0.001,length(indx0)))))
S=list(formula=~1)# SURVIVAL
GammaDoublePrime=list(formula=~1,share=TRUE,fixed=0)
#p=list(formula=~1)# SURVIVAL
fit_ac<-mark(data = rd,
    ddl=rd_ddl,
    model = "Robust", 
    time.intervals=time.intervals,
    model.parameters=list(
        S=S,
        GammaDoublePrime=GammaDoublePrime,
        p=p),
    threads=2,
    brief=FALSE,output=FALSE,invisible=TRUE)
N_ac<- fit_ac$results$derived$`N Population Size`
NN_ac<- (matrix(N_ac$estimate,nrow=40,byrow=FALSE))
NN_ac<- rowSums(NN_ac)
# by adding a group it give separate estimates for f0
plot(NN_ac~primary_occasions$year_f,ylim=c(0,50),type='b')
ff_ac<-summary(fit_ac,show.fixed=TRUE)$reals$p
SS_ac<-summary(fit_ac,show.fixed=TRUE)$reals$S
gdp_ac<-summary(fit_ac,show.fixed=TRUE)$reals$GammaDoublePrime

vals<-do.call("rbind.data.frame",ff_ac)

summary(fit_ac)$beta




ch_raw[c(55,77,79,81,85,94),grep("_0",names(ch_raw))]
ch_raw[c(55,77,79,81,85,94),1]
# pit: 1005085863   1005085888   1005085891   1005085894   1005085898 121021201081

2016_104_0 
2016_113_0
2016_295_0
2016_322_0

poolReciever_all[poolReciever_all$pit=="1005085863",]
poolReciever_all[poolReciever_all$pit=="121021201081",]
poolReciever_all[poolReciever_all$pit=="1005085898",]
poolReciever_all[poolReciever_all$pit=="1005085894",]
poolReciever_all[poolReciever_all$pit=="1005085888",]
poolReciever_all[poolReciever_all$pit=="1005085891",]
poolReciever_all[poolReciever_all$pit%in%c("1005085863","1005085891"),]


rd<- process.data(data=ch_noac, 
    #groups=c("first_captured"), 
    model="Robust", 
    time.intervals=occ_noac) 
rd_ddl<-make.design.data(rd)
S=list(formula=~1)# SURVIVAL
GammaDoublePrime=list(formula=~1,share=TRUE,fixed=0)
p=list(formula=~1,share=TRUE)# SURVIVAL
fit_ac<-mark(data = rd,
    ddl=rd_ddl,
    model = "Robust", 
    time.intervals=time.intervals,
    model.parameters=list(
        S=S,
        GammaDoublePrime=GammaDoublePrime,
        p=p),
    threads=2,
    brief=FALSE,output=FALSE,invisible=TRUE)
N_ac<- fit_ac$results$derived$`N Population Size`
NN_ac<- (matrix(N_ac$estimate,nrow=40,byrow=FALSE))
NN_ac<- rowSums(NN_ac)
# by adding a group it give separate estimates for f0
matplot(NN_ac,ylim=c(0,50))
ff_ac<-summary(fit_ac,show.fixed=TRUE)$reals$p
SS_ac<-summary(fit_ac,show.fixed=TRUE)$reals$S
gdp_ac<-summary(fit_ac,show.fixed=TRUE)$reals$GammaDoublePrime
