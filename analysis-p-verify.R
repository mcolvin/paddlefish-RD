
library(R2jags)
load("_output/ipmdat.Rdata")
## MODEL 03: INTEGRATED POPULATION MODEL FORMULATION

mod<-function()
    {
    for(ii in 1:nprim)
        {
        N[ii]<- sum(Z[,ii])  ## fixme ## below
        pcap[ii]<- 1-exp(sum(log_p0[ii,1:noccs[ii]])) #PROBABILIYT OF CAPTURE
        ncap[ii]~dbin(pcap[ii],N_lat[ii])        
        }    
    # ESTIMATE ABUNDANCE FROM CMR
    for(i in 1:M)
        {
        for(j in 1:nprim)
            {
            ip[i,j]<- omega[j]*(1-Z_known[i,j])+Z_known[i,j]# INCLUSION PROBABILITY OMEGA OR 1, 1 = ACOUSTIC
            Z[i,j]~dbern(ip[i,j])            
            }
        for(k in 1:nocc)
            {
            ch[i,k]~dbern(Z[i,secid[k]]*p[secid[k],occId[k]])
            }
        }    
        
    # PRIORS
    for(i in 1:nprim)
        {
        omega[i]~dunif(0,1)
        mu1[i]~dunif(0,160)
        N_lat[i]<-round(mu1[i])##
        for(j in 1:12)
            {
            p[i,j]~dunif(0,1)
            p0[i,j]<- 1-p[i,j]
            log_p0[i,j]<- log(p0[i,j])
            }
        }
    }
    
library(R2jags)
noccs<-table(ipmdat$secid)
occId<-unlist(sapply(noccs,function(x) 1:x))
dat<- list(
    ch=ipmdat$ch,
    nocc=length(ipmdat$secid),
    noccs=table(ipmdat$secid),
    occId=occId,
    secid=ipmdat$secid,
    tt=ipmdat$tt,
    D=ipmdat$D,
    nprim=ipmdat$nprim,
    M=ipmdat$M,
    Z_known=ipmdat$Z_known,
    ncap=ipmdat$ncap)
ini<-list()
Z<-matrix(1,dat$M,dat$nprim)
inits<- function()
	{
	list(omega=apply(Z,2,mean),
        p1=0.1,Z=Z)
	}
params<-c("N","N_lat","pcap","p")#,"p")#"pcap","p1","N1","a","b")	
ptm<- proc.time()
out<-NULL
out <- jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 3000,	
	n.burnin = 1500, 
	n.thin=1,
    export_obj_names=c("Z","D"),
	working.directory=getwd())
tot<-proc.time()-ptm
print(paste0(round(tot[3]/60,1)," minutes")) 
prod(1-out$BUGSoutput$mean$p[1,1:noccs[1]])

ncap[1]/(out$BUGSoutput$mean$pcap[1])

xxx<-cbind(out$BUGSoutput$mean$N,
    out$BUGSoutput$mean$N_lat)
plot(xxx);abline(0,1)
ncap/(1-out$BUGSoutput$mean$pcap)
ff<-2
cap<-rowSums(ch[,secid==ff])
length(cap[cap>0])
ncap[ff]
 
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


upr<-out$BUGSoutput$summary
upr<-upr[grep("N\\[",rownames(upr)),c(1,2,3,7)]
plot(ipmdat$tt,upr[,1],ylim=c(0,120),pch=19)
segments(ipmdat$tt,upr[,3],ipmdat$tt,upr[,4])
