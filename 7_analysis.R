
source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-PIT.R")
source("_R/4_tables.R")
source("_R/5_figures.R")



mod<-function()
    {  
    # S(.); daily survival=constant 
    # Gamma(.); daily movement probability=constant
    # p(.); capture probability=constant w/in primary
    for(i in 1:M)
        {
        Z[i]~dcat(phi)
        ZZ[i,1]~dcat(ini[Z[i],])
        for(t_ in 2:nprim)
            {
            ZZ[i,t_]~dcat(tmat[ZZ[i,t_-1],,Z[i],(t_-1)])
            }
        }   
        
    ## OBSERVATION MODEL PIT TAGGED FISH
    for(m in 1:M)
        {
        for(o in 1:nocc)
            {
            logit(pp[m,o,1])<-qq[o]
            pp[m,o,2]<-0
            pp[m,o,3]<-0            
            ch[m,o]~dbern(pp[m,o,ZZ[m,secid[o]]])
            }
        }
    #######################################################################
    #
    #  DERIVED PARAMETERS
    #
    #######################################################################
    for(ii in 1:M)
        {
        type[ii,1]<-equals(Z[ii],1)# N RESIDENTS
        type[ii,2]<-equals(Z[ii],2)# N MIGRANT
        }
    Ntype[1]<-sum(type[,1]) # RESIDENT
    Ntype[2]<-sum(type[,2]) # MIGRANT
   for(tt in 1:(nprim-1))
        {
        for(m in 1:M)
            {          
            N[m,tt,1]<-equals(ZZ[m,tt],1)
            N[m,tt,2]<-equals(ZZ[m,tt],2)
            N[m,tt,3]<-equals(ZZ[m,tt],3)
            }
        ## STATE SPECIFIC ABUNDANCE ESTIMATES
        Nhat[1,tt]<-sum(N[,tt,1])
        Nhat[2,tt]<-sum(N[,tt,2])
        Nhat[3,tt]<-sum(N[,tt,3]) 
        
        ## TRANSITION MATRICES (TIME VARIANT)
        ### RESIDENT LIFE HISTORY
        tmat[1,1,1,tt]<-1*S[tt]
        tmat[1,2,1,tt]<-0
        tmat[1,3,1,tt]<-1-S[tt]
        tmat[2,1,1,tt]<-0
        tmat[2,2,1,tt]<-0
        tmat[2,3,1,tt]<-0
        tmat[3,1,1,tt]<-0
        tmat[3,2,1,tt]<-0
        tmat[3,3,1,tt]<-1
        ### MIGRANT LIFE HISTORY
        tmat[1,1,2,tt]<-S[tt]*(1-gammaP[tt]) # in -> in
        tmat[1,2,2,tt]<-S[tt]*gammaP[tt] # in-> out
        tmat[1,3,2,tt]<-1-S[tt] # in-> dead
        tmat[2,1,2,tt]<-S[tt]*gammaPP[tt]
        tmat[2,2,2,tt]<-S[tt]*(1-gammaPP[tt])
        tmat[2,3,2,tt]<-1-S[tt]
        tmat[3,1,2,tt]<-0
        tmat[3,2,2,tt]<-0
        tmat[3,3,2,tt]<-1        
        }# tt

    #######################################################################
    #
    #  PRIORS
    #
    #######################################################################
    
    ## PROBABLITY OF BEING A RESIDENT OR A MIGRANT LIFE FORM
    a~dunif(0,1)
    phi[1]<-a
    phi[2]<-1-a 
    
    ## PROBABILITY OF BEING INSIDE OR OUT FOR MIGRATORY FISH
    b~dunif(0,1)    
    ini[1,1]<-1     # RESIDENT FISH ARE ALWAYS IN
    ini[1,2]<-0     # RESIDENT FISH CANNOT BE OUT
    ini[2,1]<-b     # PROBABILITY OF BEING IN STUDY AREA AT T=1 
    ini[2,2]<-1-b   # PROBABILITY OF BEING OUTSIDE STUDY AREA AT T=1
    
    # VARY BETWEEN PRIMARY OCCASIONS
    ## SURVIVAL
    beta0~dnorm(0,0.37)     # INTERCEPT
    beta1~dnorm(0,0.37)     # EFFECT OF X    
    for(dd in 1:D)
        {
        ### DAILY SURVIVAL
        logit(Y[dd])<- beta0 + beta1*X[dd]
        lnS[dd]<- log(Y[dd])
        }
    for(int in 1:(nprim-1))
        {
        S[int]<-exp(sum(lnS[int_str[int]:int_end[int]]))
        }
    ## GAMMA
    a_G~dnorm(0,0.37)
    sigma_G~dunif(0,1)
    tau_G <- pow(sigma_G, -2)    
    for(jj in 1:nprim)
        {
        ### GAMMA
        lo_G[jj]~dnorm(a_G,tau_G)
        logit(gamma[jj])<-lo_G[jj]
        gammaP[jj]<-gamma[jj]
        gammaPP[jj]<-gamma[jj]     
        }
    ## CAPTURE PROBABILITY  
    lo_p~dnorm(0,0.37)
    sigma~dunif(0,1)
    tau <- pow(sigma, -2)    
    for(kk in 1:nocc)
        {
        qq[kk]~dnorm(lo_p,tau)
        }
    }


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
	model.file=mod,
    export_obj_names=c("Z","ZZ","qq","lo_G"),
	n.chains = 3,	
	n.iter = 150,	
	n.burnin = 60, 
	n.thin=2,
	working.directory=getwd())








dat<- makeData(40)
dat$occs<-ncol(dat$ch)
dat$inn_ac<- 31
dat$total_ac<-42
states<-dat$Z

inits<- function()
	{
	list(omega=0.5,p=0.1,lambda=0.1)
	}
params<-c("cap_p","Sprob","Nhat",
    "omega","p","lambda")	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=primary,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
plot(out)
out








## THE VR2 WAS IN THE POOL FOR GOOD 2016 DOY  189
