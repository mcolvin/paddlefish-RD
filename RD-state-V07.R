library(R2jags)
## V02-hetergeneous capture probabilities [done]
## V03-add survival constant [done]
## V04-hetergeneous survival [done]
## V05-hetergeneous gamma [done]
## V06-parameter estimate check by sim [done]
## V07-daily covariate on survival []
##     fixed issue with secid not repping 
##     all primaries-need to verify results.


nprim<-10
nsec<-5
duration<- 365*2
intervals<- (runif(nprim-1,0,1))
intervals<-c(0,cumsum(intervals)/sum(intervals))
intervals<- round(intervals*duration,0)
intervals[1]<-1
## DAILY TEMPERATURE
temp<- runif(duration, -3,3) # centered
b0<- 8
b1<- -0.01
y<- b0+b1*temp
dailyS<- plogis(b0+b1*temp)

#plot(dailyS)
#plot(dailyS~temp)
## verify that daily is equal to S^interval
#Z<- matrix(0,5000,duration)
#Z[,1]<-1
#for(i in 2:duration)
#    {
#    Z[,i]<-rbinom(5000,1,dailyS[i]*Z[,i-1])
#    }
#indx<- cbind(intervals[-(nprim-1)],intervals[-1])
#lnS<- rep(0,nprim-1)
#for(k in 1:(nprim-1))
#    {    
#    lnS[k]<- sum((log(dailyS[indx[k,1]:indx[k,2]])))
#    }
#intSurv<-exp(lnS)
#N<-colSums(Z[,intervals])
#cbind(intSurv,N[-1]/N[1:nprim-1])
#plot(colSums(Z))
#abline(v=intervals)

secid<-sort(rep(1:nprim,nsec)) 
nocc<-length(secid)

## INIITALIZE THE POPULATION
Z<- c(rep(1,20),rep(2,50))

## SURVIVAL
#phi_mu<-qlogis(0.85)
#phi_er<- 0.2
#phi<- plogis(rnorm(nprim,phi_mu,phi_er))  
indx<- cbind(intervals[-(nprim)],intervals[-1]-1)
lnS<- rep(0,nprim-1)
for(k in 1:(nprim-1))
    {    
    lnS[k]<- sum(log(dailyS[indx[k,1]:indx[k,2]]))
    #lnS[k]<- prod(dailyS[indx[k,1]:indx[k,2]])
    }
phi<-c(0,exp(lnS))

## MOVEMENT
### random movement
### Gamma'' out->out
### Gamma' in->out
gamma_mu<- qlogis(0.3)
gamma_re<- 0.05
gamma<- plogis(rnorm(nprim,gamma_mu,gamma_re))
gammaP<-gamma
gammaPP<-gamma

## TRUE VALUES
tru<-list()
tru$gamma_mu<-gamma_mu
tru$gamma_re<-gamma_re
tru$gammaP<-gamma
tru$gammaPP<-gamma

#######################################################################
#
# STATE DYNAMICS 
#
####################################################################### 

## TRANSITION MATRICES
tmat<-array(0,c(3,3,2,nprim))
for(t_ in 2:nprim)
    {
    ## resident fish (1=in,2=out,3=dead)
    tmat[1,,1,t_]<-c(phi[t_]*1,0,(1-phi[t_])) # in->in
    tmat[2,,1,t_]<-c(0,0,0) # outside->in
    tmat[3,,1,t_]<-c(0,0,1) # dead->dead

    ## migrant fish (1=in,2=out,3=dead)
    tmat[1,,2,t_]<-c(phi[t_]*(1-gammaP[t_]),
        phi[t_]*gammaP[t_],1-phi[t_]) # in->in
    tmat[2,,2,t_]<-c(phi[t_]*gammaPP[t_],
        phi[t_]*(1-gammaPP[t_]),1-phi[t_]) # outside->in
    tmat[3,,2,t_]<-c(0,0,1) # dead->dead
    }

#######################################################################
#
#  PROCESS MODEL
#
#######################################################################

## STATE MATRIX
ZZ<-matrix(0,length(Z),nprim)
## INITIAL STATES
ZZ[which(Z==1),1]<-  1
ZZ[which(Z==2),1]<-  sample(c(1,2),
    length(which(Z==2)),c(1,1),replace=TRUE)

## STATE TRANSITIONS
for(t_ in 2:nprim)
    {
    for(j in 1:length(Z))
        {
        ZZ[j,t_]<-which(rmultinom(1,1,tmat[ZZ[j,t_-1],,Z[j],t_])==1)
        }
    }
 
#######################################################################
#
#  SIMULATE CAPTURE HISTORIES
#
#######################################################################
p_mu<-qlogis(0.8)
p_er<- 0.2
p<- plogis(rnorm(length(secid),p_mu,p_er))  
ch<-matrix(0,length(Z),length(secid))

## OBSERVATION MODEL
for(i in 1:length(Z))
    {  
    for(j in 1:length(secid))
        {
        avail<-ifelse(ZZ[i,secid[j]]==1,1,0)  
        ch[i,j]<-rbinom(1,1,p[j]*avail)        
        }
    }

## TRUE VALUES
tru$p_mu<-p_mu
tru$p_er<-p_er
tru$p<-p
    
#######################################################################
#
# BUGS MODEL  
#
#######################################################################
mod<-function()
    {    
    for(i in 1:M)
        {
        Z[i]~dcat(phi)
        ZZ[i,1]~dcat(ini[Z[i],])
        for(t_ in 2:nprim)
            {
            ZZ[i,t_]~dcat(tmat[ZZ[i,t_-1],,Z[i],(t_-1)])
            }
        }   
        
    ## OBSERVATION MODEL
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
        type[ii,1]<-equals(Z[ii],1)
        type[ii,2]<-equals(Z[ii],2)
        }
    Ntype[1]<-sum(type[,1])
    Ntype[2]<-sum(type[,2])
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
        ### SURVIVAL
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
#  BUNDLE UP THE DATA
#
#######################################################################    

M=length(Z)
dat<-list(
    D=duration,
    int_str=indx[,1],
    int_end=indx[,2],
    X=temp,
    ch=ch,
    nprim=nprim,
    secid=secid,
    M=M, 
    nocc=nocc)
#######################################################################
#
#  MODEL INTIAL VALUES
#
#######################################################################

qq<-rep(0,dat$nocc)    
lo_G<-rep(0,dat$nprim)    
lo_S<-rep(0,dat$nprim)    
inits<-function()
    {list(a=0.3,b=0.3,
        Z=Z,
        ZZ=ZZ,
        qq=qq,
        lo_G=lo_G,
        #lo_S=lo_S,
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
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())

plot(out$BUGSoutput$mean$S,phi[-1])
    
matplot(t(out$BUGSoutput$mean$Nhat))
    