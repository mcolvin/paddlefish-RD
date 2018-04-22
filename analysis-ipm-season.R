
#######################################################################
#
#  LOAD SCRIPTS
#
#######################################################################
library(RODBC)
source("_R/1_global.R")
source("_R/2_functions.R")
run<-FALSE # SET TO TRUE TO REQUERY NOXUBEE GAGE DATA
source("_R/3_load-and-clean.R") #needs internet to pull gage data
source("_R/4_tables.R")
source("_R/5_figures.R")




load("_output/ipmdat.Rdata")
source("_R/6_models-ipm-season.R")
library(R2jags)

## SESSION 1: 2/11/2016 TO 7/1/2016
dat<- list(D=as.integer(as.Date("2016-07-11")-(min(effort$date)))+1L)
dat$tt<-ipmdat$tt[ipmdat$tt<=dat$D]
dat$nprim<-length(dat$tt)
dat$secid<- ipmdat$secid[ipmdat$secid<=dat$nprim]
dat$nocc<-length(dat$secid)
dat$ch<- ipmdat$ch[,c(1:length(secid))]
dat$X<- ipmdat$X[ipmdat$X[,1]<=dat$D,]   
dat$M=nrow(dat$ch)
dat$Z_known=ipmdat$Z_known[,1:dat$nprim] 
dat$ncap=ipmdat$ncap[1:dat$nprim] 
dat$noccs<-table(ipmdat$secid[ipmdat$secid<=dat$nprim])
dat$occId<-unlist(sapply(dat$noccs,function(x) 1:x))

ini<-list()
Z<-matrix(1,dat$M,dat$nprim)
inits<- function()
	{
	list(omega=apply(Z,2,mean),
        p1=0.1,Z=Z,
        b=c(-6,0.02,-0.04),
        a=c(-10,1.3,-1.04))
	}
params<-c("N","N_lat","NNtest","a","b")#"pcap","p1","N1")	
ptm<- proc.time()
out<-NULL
out <- jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 3000, 
	n.thin=1,
    export_obj_names=c("Z","D"),
	working.directory=getwd())
tot<-proc.time()-ptm
print(paste0(round(tot[3]/60,1)," minutes")) 
#save(out,file="_output/ests-with-a-b.Rdata")

plot(out$BUGSoutput$mean$N_lat[dat$tt],out$BUGSoutput$mean$N);abline(0,1)
 
plot(out$BUGSoutput$mean$N_lat[tt],out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat,type='l',ylim=c(0,150))
points(dat$tt,out$BUGSoutput$mean$N,type='l',col="red");abline(v=tt)
 
upr<-out$BUGSoutput$summary
upr<-upr[grep("N_lat",rownames(upr)),c(1,2,3,7)]
 
estdat<- as.Date("2016-02-11")+dat$X$day
plot(estdat,out$BUGSoutput$mean$N_lat,type='n',ylim=c(0,150))
y<-c(upr[,3],rev(upr[,4]))
x<-c(estdat,rev(estdat))
polygon(x,y,col="lightgrey",border='lightgrey')
points(estdat,upr[,1],type='l',ylim=c(0,150))


upr<-out$BUGSoutput$summary
upr<-upr[grep("N\\[",rownames(upr)),c(1,2,3,7)]
plot(ipmdat$tt,upr[,1],ylim=c(0,120),pch=19)
segments(ipmdat$tt,upr[,3],ipmdat$tt,upr[,4])

upr<-out$BUGSoutput$summary
coeffs<-upr[grep("b",rownames(upr)),c(1,2,3,7)]
coeffs<-rbind(coeffs,upr[grep("a",rownames(upr)),c(1,2,3,7)])
coeffs
