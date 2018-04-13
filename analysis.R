
#######################################################################
#
#  LOAD SCRIPTS
#
#######################################################################
library(R2jags)
library(RODBC)
#library(dataRetrieval)
#library(lubridate)
source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R") #needs internet
dat<-readRDS("_output/dat.Rds")
source("_R/4_tables.R")
source("_R/5_figures.R")
source("_R/6_models.R")



mod<-function()
    {
    for(ind in 1:N_ac)
        {
        for(day in (ac_meta[ind,1]+1):ac_meta[ind,2]) ## loop over days with data
            {
            obs_state[day,ind]~dcat(psi[day,,tag_state[day-1,ind]])
            }        
        }
    for(i in 1:D)
        {
        logit(gammaprimeprime[i])<-lo_gpp[1]+lo_gpp[2]*X[i,3] 
        logit(gammaprime[i])<-lo_gp[1]+lo_gp[2]*X[i,3] 
        # DAILY STATE TRANSITIONS: 1=pool, 2= outside, 3=translocated
        psi[i,1,1]<- 1-gammaprimeprime[i]   # stays in 
        psi[i,2,1]<- gammaprimeprime[i]     # moves out
        psi[i,1,2]<- 1-gammaprime[i]        # moves in
        psi[i,2,2]<- gammaprime[i]          # moves out
        psi[i,1,3]<- 0                      # moves in
        psi[i,2,3]<- 1                      # moves out: translocated
        # PREDICT MISSING OKTOC STAGES
        mu_stage[i]<-b[1]+b[2]*X[i,3]
        X[i,4]~dnorm(mu_stage[i],tau)# inverse of variance 
        }

    # PRIORS
    ## STAGE MODEL
    b[1]~dnorm(0,0.001)
    b[2]~dnorm(0,0.001)
    tau~dgamma(0.001,0.001)# inverse of variance
    sigma<-1/sqrt(tau)
    
    ## TRANSITION PROBABILITY PARAMETERS (1->2 AND 2->1)
    ### 1->2: GAMMA''
    lo_gpp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gpp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    
    ### 2->1: GAMMA'
    lo_gp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY       
    }
    
    
    

inits<-function()
    {list(b=c(0,0),lo_gpp=c(0,0),lo_gp=c(0,0))}
params<- c("b","lo_gp","lo_gpp")
ptm <- proc.time()
rundat<- dat[c("X","D","tag_state","obs_state","ac_meta","N_ac")]
out <- jags.parallel(data=rundat,
	inits=inits,
	parameters=params,	
	model.file=mod,
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



