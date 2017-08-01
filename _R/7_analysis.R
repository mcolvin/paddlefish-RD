#######################################################################
# NOTES
#######################################################################
## THE VR2 WAS IN THE POOL FOR GOOD 2016 DOY  189



rd<- process.data(data=ch, 
    groups=c("first_captured"),# 
    model="Robust", 
    time.intervals=occ) 
rd_ddl<-make.design.data(rd)

# PLACES TO SET TO 1
## ACOUSTICALLY TAGGED FISH IN SESSION 1
recieverOn<- c(1:40)[-c(1,2,3,8,9,17,18)]# NO RECIEVER, NO DETECTIONS
recieverOff<- c(1,2,3,8,9,17,18)# NO RECIEVER, NO DETECTIONS

rd_ddl$p$fix<-NA
for(i in recieverOn)# FIX P TO 0.999 WHEN RECIEVER IS ON AND TIME=0
    {
    rd_ddl$p$fix<-ifelse(as.numeric(as.character(rd_ddl$p$session))==i&
        as.numeric(as.character(rd_ddl$p$Time))==0  ,0.999,rd_ddl$p$fix)
    }
for(i in recieverOff)# FIX P TO 0.001 WHEN RECIEVER IS OFF AND TIME=0
    {
    rd_ddl$p$fix<-ifelse(as.numeric(as.character(rd_ddl$p$session))==i&
        as.numeric(as.character(rd_ddl$p$Time))==0  ,0.001,rd_ddl$p$fix)
    }
# FIX P TO 0.001 FOR TO OCCASION THAT A FISH WAS CAPTURED
rd_ddl$p$fix<-ifelse(as.numeric(as.character(rd_ddl$p$session))<=
    as.numeric(as.character(rd_ddl$p$first_captured)) &
    as.numeric(as.character(rd_ddl$p$Time))==0,0.001,rd_ddl$p$fix)
# FIX P TO 0.001 FOR PIT TAGGED FISH     
rd_ddl$p$fix<-ifelse(as.numeric(as.character(rd_ddl$p$first_captured))==0 &
    as.numeric(as.character(rd_ddl$p$Time))==0 ,0.001,rd_ddl$p$fix) 


function(1==2){  
    rd_ddl$p[100:200,]
    rd_ddl$p[200:400,]
    rd_ddl$p[400:600,]
    rd_ddl$p[600:800,]
    rd_ddl$p[800:1000,]
    rd_ddl$p[1000:1200,]
    rd_ddl$p[1200:1400,]
    rd_ddl$p[1400:1600,]
    rd_ddl$p[1600:1800,]
    rd_ddl$p[1800:2000,]
    rd_ddl$p[2000:2200,]
    rd_ddl$p[2200:2400,]
    rd_ddl$p[2400:2600,]
    rd_ddl$p[2600:2800,]
}

## BREAK APART WHETHER FISH ARE PIT ONLY OR NOT
rd_ddl$p$pbin<-0
rd_ddl$c$pbin<-0
rd_ddl$p$pbin[which(rd_ddl$p$time==1)]<-1
rd_ddl$c$pbin[which(rd_ddl$c$time==1)]<-1
rd_ddl$c$fix<-NA
## SET VALUES TO BE CLOSE TO 1 OR 0 AS IT MESSES UP 
## THE ESTIMATES OF S AND GAMMA PRIME
indx<-which(!(is.na(rd_ddl$p$fix)))
p=list(formula=~first_captured:session,
    share=TRUE,
    fixed= list(
        index =indx, 
        value= rd_ddl$p$fix[indx]))

        
S=list(formula=~Time)# SURVIVAL
GammaDoublePrime=list(formula=~Time,
    share=TRUE)

fit_ac<-mark(data = rd,
    ddl=rd_ddl,
    model = "Robust", 
    time.intervals=time.intervals,
    model.parameters=list(
        S=S,
        GammaDoublePrime=GammaDoublePrime,
        p=p),
    threads=2,
    brief=FALSE,output=FALSE,invisible=FALSE)
N_ac<- fit_ac$results$derived$`N Population Size`
NN_ac<- (matrix(N_ac$estimate,nrow=40,byrow=FALSE))
NN_ac<- rowSums(NN_ac)
# by adding a group it give separate estimates for f0
plot(NN_ac~primary_occasions$year_f,
    ylim=c(0,150),type='b',
    xaxt='n')
points(ncap~primary_occasions$year_f,col='red')
yy<- data.frame(
    year=2016,
    month=doy_months$month,
    month_lab=doy_months$month_lab,
    doy= doy_months$doy)
yy<- rbind(yy,data.frame(
    year=2017,
    month=doy_months$month,
    month_lab=doy_months$month_lab,
    doy= doy_months$doy)  )  
    
yy$tt<- yy$year+(yy$doy/365)
axis(1,at=yy$tt,labels=yy$month_lab,
    tck=-0.01)


    
points(primary_occasions$year_f-0.01,ncaptured,type='b',col="red")
legend("topleft",
    c("Captured","Estimated"),
    col=c("red","black"))
    
  table(duplicated(p$fixed$index))
  
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

