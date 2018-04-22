

load<-0 ## 1 TO LOAD DATA AGAIN; PREBLE CODE AT BOTTOM OF MODEL.r
library(R2jags)
source("_R/6_models-rd.R")
dat<-readRDS("_output/rd-dat.RDS")

## MODR2

n<-150
omega<- 0.8
ini_p<-0.4
nprim<-58
D<-7
gamma2prime<-0.3
gammaprime<-0.3
Z<-rbinom(n,1,omega)    
pool<-matrix(0,n,D)   
pool[,1]<-rbinom(n,1,ini_p*Z)
tmat<-matrix(0,n,D)
## DAILY TRANSITION
for(d in 2:D)
    {
    for(i in 1:n)
        {
        tmat[i,d]<- ((1-gamma2prime)*pool[i,d]+
            (gammaprime)*(1-pool[i,d]))*Z[i]
        pool[i,d]<-rbinom(1,1,tmat[i,d])  
        }
    }

dat<- list(D=D,pool_obs=pool,M=n)#,
inits<- function(){list(
    Z=rep(1,n),
    pool=matrix(1,n,nprim),
    ini=1,
    omega=1,
    gammaprime=0.3,gamma2prime=0.3)})
ptm<-proc.time()
out <- jags(data=dat,
	inits=NULL,
	parameters=c("omega","gammaprime","gamma2prime"),	
	model.file=modRD2,
	n.chains = 3,	
	n.iter = 1000,	
	n.burnin = 500,
	n.thin=1,
	working.directory=getwd())  
tot<-proc.time()-ptm


  out$BUGSoutput$median$pool







## MODEL

n<-150
omega<- 0.8
ini_p<-0.4
nprim<-58
D<-700
gamma2prime<-0.3
gammaprime<-0.3

tmat<-array(NA,c(D,3,3))
tmat[1,1,1]<-omega*(1-ini_p)    # in 
tmat[1,2,1]<-omega*ini_p        # out
tmat[1,3,1]<-1-omega            # not in population
tmat[1,1,2]<- omega*(1-ini_p)     # in
tmat[1,2,2]<- omega*ini_p       # out
tmat[1,3,2]<- 1-omega            # not in population
tmat[1,1,3]<- omega*(1-ini_p)      
tmat[1,2,3]<- omega*ini_p        
tmat[1,3,3]<- (1-omega)   

Z<-matrix(0,n,D)    
Z[,1]<-sample(1:3,n,replace=TRUE,prob=tmat[1,,1])    
## DAILY TRANSITION
for(d in 2:D)
    {
    tmat[d,1,1]<- 1-gamma2prime      # in to in
    tmat[d,2,1]<- gamma2prime        # in to out
    tmat[d,3,1]<- 0                  # in to out
    
    tmat[d,1,2]<- 1-gammaprime       # out to in
    tmat[d,2,2]<- gammaprime        # out to out
    tmat[d,3,2]<- 0                 # out to out
    
    tmat[d,1,3]<- 0       
    tmat[d,2,3]<- 0        
    tmat[d,3,3]<- 1           
    
    for(i in 1:n)
        {
        Z[i,d]<-sample(1:3,1,replace=TRUE,prob=tmat[d,,Z[i,d-1]]) 
        }
    }

dat<- list(D=D,Z=Z,M=n)#,
inits<- function(){list(gammaprime=0.3,gamma2prime=0.3,omega=0.9)})
ptm<-proc.time()
out <- jags(data=dat,
	inits=NULL,
	parameters=c("omega","gammaprime","gamma2prime"),	
	model.file=modRD,
	n.chains = 3,	
	n.iter = 100,	
	n.burnin = 50,
	n.thin=1,
	working.directory=getwd())  
tot<-proc.time()-ptm
 






 
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

