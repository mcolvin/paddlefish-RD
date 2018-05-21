
#######################################################################
#
#  LOAD SCRIPTS
#
#######################################################################
library(RODBC);library(R2jags)
source("_R/1_global.R")
source("_R/2_functions.R")
run<-FALSE # SET TO TRUE TO REQUERY NOXUBEE GAGE DATA
source("_R/3_load-and-clean-Model-6-v01.R") #needs internet to pull gage data
source("_R/4_tables.R")
source("_R/5_figures.R")
source("_R/6_models-ipm-V01.R")


inits<- function()
	{
	list(omega=rep(0.2,58),
        p=matrix(0.1,58,12),
        Z=apply(dat$a,c(1,3),max))
	}
params<-c("N","N_lat","p","a","b")#"pcap","p1","N1")	
params<-c("Nhat")

ptm<- proc.time()
out<-NULL
adat<- dat [c("a",'b',"nprime","na","nb","nocc")]
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
save(out,file="_output/ests-with-a-b.Rdata")

plot(out$BUGSoutput$mean$N_lat,out$BUGSoutput$mean$N);abline(0,1)
 
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

upr<-out$BUGSoutput$summary
coeffs<-upr[grep("b",rownames(upr)),c(1,2,3,7)]
coeffs<-rbind(coeffs,upr[grep("a",rownames(upr)),c(1,2,3,7)])
coeffs

dat<-readRDS("_output/dat.RDS")
## MODEL 02: INTEGRATES MODEL 00 AND 01
## TO INCORPORATE PIT AND ACOUSTIC TAGS
## IN FISH.
## MODEL 02: COMBINES ACOUSTIC AND PIT
mod02<-function()
    {
    for(indp in 1:M_p)
        {
        for(t_p in 1:nocc)
            {        
            ch_p[indp,t_p]~dbern(ZZ_p[dayid[t_p],indp]*p[secid[t_p]]) # PERFECT DETECTION
            }      
                ### PROCESS MODEL        
        ZZ_p[1,indp]~dbern(ini)
        for(ddd in 2:D)
            {
            ZZ_p[ddd,indp]~dbern(tmpp[ddd,indp])  ## 1 = in, 0 = out                     
            tmpp[ddd,indp]<-ZZ_p[ddd-1,indp]*psi[ddd,1]+
                (1-ZZ_a[ddd-1,indp])*psi[ddd,2]
            }
        }
       
    ## PROCESS MODEL
    ### ASSIGNS FISH AS INSIDE OR OUTSIDE 
    for(ind in 1:M_a)
        {
        ### RECIEVER CAPTURES
        #for(day in ac_meta[ind,1]:ac_meta[ind,2]) ## loop over days with data
        for(day in 1:D) ## loop over days with data
            {
            pp_aa[day,ind]<-ZZ_a[day,ind]*obs_state_p[day,ind]           
            obs_state[day,ind]~dbern(obs_state_p[day,ind]) # PERFECT DETECTION
            }        
        ### PHYSICAL CAPTURES
        for(t_ in 1:nocc)
            {
            pp_a[ind,t_]<-ZZ_a[dayid[t_],ind]*p[secid[t_]]
            ch_a[ind,t_]~dbern(pp_a[ind,t_])
            }
        ### PROCESS MODEL        
        ZZ_a[1,ind]~dbern(ini)
        for(dd in 2:D)
            {
            ZZ_a[dd,ind]~dbern(tmp[dd,ind])  ## 1 = in, 0 = out                     
            tmp[dd,ind]<-ZZ_a[dd-1,ind]*psi[dd,1]+(1-ZZ_a[dd-1,ind])*psi[dd,2]
            }
        }
    ## PREDICT STAGE FOR OKTOC FROM NOXUBEE
    for(i in 1:D)
        {
        logit(gammaprimeprime[i])<-lo_gpp[1]+lo_gpp[2]*X[i,3] +lo_gpp[3]*X[i,5] 
        logit(gammaprime[i])<-lo_gp[1]+lo_gp[2]*X[i,3] +lo_gp[3]*X[i,5] 
        # DAILY STATE TRANSITIONS: 1=pool, 2= outside, 3=translocated
        psi[i,1]<- 1-gammaprimeprime[i]   # in-->in 
        psi[i,2]<- 1-gammaprime[i]        # out-->in
        #psi[i,2]<- gammaprimeprime[i]     # in-->out        
        #psi[i,2]<- gammaprime[i]          # out-->out

        # PREDICT MISSING OKTOC STAGES
        mu_stage[i]<-b[1]+b[2]*X[i,3]
        X[i,4]~dnorm(mu_stage[i],tau)# inverse of variance 
        }

    # PRIORS
    
    ## INITIAL STATE
    a~dnorm(0,0.37)
    logit(ini)<-a
    
    ## STAGE MODEL
    b[1]~dnorm(0,0.001)
    b[2]~dnorm(0,0.001)
    tau~dgamma(0.001,0.001)# inverse of variance
    sigma<-1/sqrt(tau)
    
    ## TRANSITION PROBABILITY PARAMETERS (1->2 AND 2->1)
    ### 1->2: GAMMA''
    lo_gpp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gpp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gpp[3]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    
    ### 2->1: GAMMA'
    lo_gp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY
    lo_gp[3]~dnorm(0,0.37)## CAPTURE PROBABILITY

    ### CAPTURE PROBABILITY
    for(k in 1:nprim)
        {
        p[k]~dunif(0,1)
        }
    }
ZZ_a<-matrix(1,dat$D,dat$M_a)
ZZ_p<-matrix(1,dat$D,dat$M_p)
inits<-function(t)
    {
    list(a=0,b=c(0,0),lo_gpp=c(0,0,0),
		lo_gp=c(0,0,0),ZZ_a=ZZ_a,ZZ_p=ZZ_p
    )}
params<- c('a',"b","lo_gp","lo_gpp")
ptm <- proc.time()
rundat<- dat[c("D","X","nocc","dayid",#"ac_meta","tag_state",
    "ch_a","ch_p","nprim","secid","obs_state","obs_state_p",
    "M_a","M_p")] 
out <- jags.parallel(data=rundat,
	inits=inits,
	parameters=params,	
	model.file=mod02,
	n.chains = 3,	
	n.iter = 7500,	
	n.burnin = 4000,
	export_obj_names=c("ZZ_a","ZZ_p"),	
	n.thin=2,
	working.directory=getwd())
proc.time()-ptm
saveRDS(out,"_output/rd-run.RDS")
xx<-update(out,n.iter=2000, n.thin=1) 





    
out <- bugs(data=rundat,
	inits=inits,
	parameters.to.save=params,	
	model.file=paste(getwd(),"_output/mod02.bug",sep="/"),
    debug=TRUE,
    bugs.directory="C:/Users/mcolvin/Documents/WinBUGS14",
	n.chains = 1,	
	n.iter = 50,	
	n.burnin = 25, 
	n.thin=2,
     working.directory=getwd())  

file.show("_output/mod02.bug")    
    
## MODEL 01: ESTIMATES THE PROBABILITY
## ON A DAILY BASIS AND ESTIAMTES MISSING
## STAGE DATA

Z<-matrix(1,dat$M,dat$nprim)  # in or out of pool 
qq<-rep(0,dat$nprim)   # pl  
oo<-rep(0,dat$nprim)   # pl  
inits<-function()
    {list(Z=Z,qq=qq,oo=oo)}
.
inits<-function()
    {list(b=c(0,0),lo_gpp=c(0,0),lo_gp=c(0,0))}
params<- c("b","lo_gp","lo_gpp")
ptm <- proc.time()
rundat<- dat[c("X","D","tag_state","obs_state","ac_meta","N_ac")]
out <- jags.parallel(data=rundat,
	inits=inits,
	parameters=params,	
	model.file=mod01,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 2500, 
	n.thin=2,
	working.directory=getwd())

ptm <- proc.time()-ptm

out$BUGSoutput$mean$psi

plot(out$BUGSoutput$mean$mu_stage,X[,4])
plot(out$BUGSoutput$mean$mu_stage,X[,2])
plot(out$BUGSoutput$mean$mu_stage,X[,3])
plot(out$BUGSoutput$mean$mu_stage,type='l',ylim=c(0,4))
points(X[,4],type='l')

fit<-lm(log(X[,4])~log(X[,3]+X[,2]))#
plot(fitted(fit)~resid(fit))
summary(fit)
out$BUGSoutput$mean$b0
out$BUGSoutput$mean$b1


#######################################################################
#
#  MODEL 0
#
#######################################################################
Z<-matrix(1,dat$M,dat$nprim)  # in or out of pool 
qq<-rep(0,dat$nprim)   # pl  
oo<-rep(0,dat$nprim)   # pl  
inits<-function()
    {list(Z=Z,qq=qq,oo=oo)}
params<- c("qq","oo","Z")
#  RUN MODEL AND TRACK ESTIMATES 
params<- c("qq","oo")
ptm <- proc.time()
out <- R2jags::jags.parallel(data=dat[c("ch","nprim","secid","M","nocc")],
	inits=inits,
	parameters=params,	
	model.file=mod0,
    export_obj_names=c("Z","qq","oo"),
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())

saveRDS(out, "_output/ests-Mod0.RDS")


ptm <- proc.time()-ptm


out<- readRDS("_output/ests-Mod0.RDS")
out$BUGSoutput$summary
out$BUGSoutput$mean$Nhat

matplot(t(out$BUGSoutput$mean$Nhat),type='l')




#######################################################################
#
#  MODEL INTIAL VALUES
#
#######################################################################
Z<-rep(1,dat$M) # resident or migrant
ZZ<- matrix(1,dat$M,dat$nprim)
qq<-rep(0,dat$nocc)    
lo_G<-rep(0,dat$nprim)    
lo_S<-rep(0,dat$nprim)    
inits<-function()
    {list(a=0.3,b=0.3,
        Z=Z,
        ZZ=ZZ,
        qq=qq,
        lo_G=lo_G,
        lo_p=0,sigma=0.1,
        a_G=0,sigma_G=0.3,
        beta0=0,beta1=0)}

#######################################################################
#
#  RUN MODEL AND TRACK ESTIMATES
#
#######################################################################     
params<-c("gamma","a","b","Nhat","Ntype","lo_p",
    "sigma","tau","beta0","beta1",
    "a_G","sigma_G","S")	
out <- R2jags::jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod1,
    export_obj_names=c("Z","ZZ","qq","lo_G"),
	n.chains = 3,	
	n.iter = 150000,	
	n.burnin = 60000, 
	n.thin=2,
	working.directory=getwd())

saveRDS(out, "_output/ests.RDS")



out<- readRDS("_output/ests.RDS")
out$BUGSoutput$summary
out$BUGSoutput$mean$Nhat

matplot(t(out$BUGSoutput$mean$Nhat),type='l')



