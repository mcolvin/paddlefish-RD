
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

library(R2jags)
dat<-readRDS("_output/dat.RDS")
dat$X[,2]<-scale(dat$X[,2],center=mean(dat$X[,2]),scale=sd(dat$X[,2]))
dat$X[,3]<-scale(dat$X[,3],center=mean(dat$X[,3]),scale=sd(dat$X[,3]))
dat$X[,3]<-scale(dat$X[,4],center=mean(dat$X[,4]),scale=sd(dat$X[,4]))
## MODEL 02: INTEGRATES MODEL 00 AND 01
## TO INCORPORATE PIT AND ACOUSTIC TAGS
## IN FISH.
## MODEL 02: COMBINES ACOUSTIC AND PIT
mod02<-function()
    {
    ## PIT TAG MODEL
    for(indp in 1:M_p)
        {
        for(t_p in 1:nocc)
            {        
            ch_p[indp,t_p]~dbern(ZZ_p[dayid[t_p],indp]*p[secid[t_p]]) 
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

