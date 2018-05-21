
#######################################################################
#
#  LOAD SCRIPTS
#
#######################################################################
library(RODBC);library(R2jags)
source("_R/1_global.R")
source("_R/2_functions.R")
run<-FALSE # SET TO TRUE TO REQUERY NOXUBEE GAGE DATA
source("_R/3_load-and-clean-Model-6-v02.R") #needs internet to pull gage data
source("_R/4_tables.R")
source("_R/5_figures.R")
source("_R/6_models-ipm-V02.R")


inits<- function()
	{
	list(omega=rep(0.2,58),
        p=matrix(0.1,58,12),
        Z=apply(dat$a,c(1,3),max),
        cN=runif(1,40,160),
        bbeta= rep(0,2),
        abeta= rep(0,2)
        )
	}
params<-c("N","N_lat","p","a","b")#"pcap","p1","N1")	
params<-c("Nhat","estAdults")

ptm<- proc.time()
out<-NULL
adat<- dat [c("a",'b',"nprime","na","nb","nocc","D",
    "X",'tt')]
#adat<- dat [c("a",'b',"nprime","na","nb","nocc")]
adat$ncap<-colSums(apply(dat$a,c(1,3),max))
out <- jags.parallel(data=adat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 3000, 
	n.thin=1,
    export_obj_names=c("dat"),
	working.directory=getwd())
tot<-proc.time()-ptm
print(paste0(round(tot[3]/60,1)," minutes"))


plot(out$BUGSoutput$mean$estAdults,type='l')
points(dat$tt,out$BUGSoutput$mean$Nhat)
points(dat$tt,(out$BUGSoutput$mean$N+adat$nb),col="red")
plot(dat$tt,out$BUGSoutput$mean$N/out$BUGSoutput$mean$Nhat)


plot(2:dat$D,out$BUGSoutput$mean$mu1,ylim=c(0,200),pch=19,col="green")
points(dat$tt,out$BUGSoutput$mean$Nhat,ylim=c(0,200),pch=19,col="red")





 
save(out,file="_output/ests-with-a-b.Rdata")

plot(out$BUGSoutput$mean$N_lat,out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat,out$BUGSoutput$mean$Nhat);abline(0,1)
 
plot(N[tt],out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat[tt],out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat,type='l',ylim=c(0,150))
points(N,type='l',col="red");abline(v=tt)
 
upr<-out$BUGSoutput$summary
upr<-upr[grep("N_lat",rownames(upr)),c(1,2,3,7)]
 
estdat<- as.Date("2016-02-11")+dat$X$day
plot(estdat,out$BUGSoutput$mean$N_lat,type='n',ylim=c(0,150))
y<-c(upr[,3],rev(upr[,4]))
x<-c(estdat,rev(estdat))
polygon(x,y,col="lightgrey",border='lightgrey')
points(estdat,upr[,1],type='l',ylim=c(0,150))

estdat<- as.Date("2016-02-11")+dat$X$day
upr<-out$BUGSoutput$summary
upr<-upr[grep("mu1\\[",rownames(upr)),c(1,2,3,7)]
plot(estdat[-1],upr[,1],ylim=c(0,120),pch=19)
segments(ipmdat$tt,upr[,3],ipmdat$tt,upr[,4])

upr<-out$BUGSoutput$summary
upr<-upr[grep("mu1\\[",rownames(upr)),c(1,2,3,7)]
plot(dat$tt,upr[,1],ylim=c(0,120),pch=19)
segments(ipmdat$tt,upr[,3],ipmdat$tt,upr[,4])

upr<-out$BUGSoutput$summary
coeffs<-upr[grep("b",rownames(upr)),c(1,2,3,7)]
coeffs<-rbind(coeffs,upr[grep("a",rownames(upr)),c(1,2,3,7)])
coeffs

