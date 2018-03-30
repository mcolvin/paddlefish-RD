library(R2jags)
## V02-hetergeneous capture probabilities [done]
## V03-add survival constant [done]
## V04-hetergeneous survival [done]
## V05-hetergeneous gamma [done]
## V06-parameter estimate check by sim []


nprim<-10
secid<-sort(rep(1:5,nprim))
nocc<-length(secid)

## INIITALIZE THE POPULATION
Z<- c(rep(1,20),rep(2,50))

## SURVIVAL
phi_mu<-qlogis(0.85)
phi_er<- 0.2



## random movement
## Gamma'' out->out
### Gamma' in->out

gamma_mu<- qlogis(0.3)
gamma_re<- 0.05




p_mu<-qlogis(0.3)
p_er<- 0.2
reps<-data.frame(type="TRUE",phi_mu=phi_mu,phi_er=phi_er,
    gamma_mu=gamma_mu,gamma_re=gamma_re,
    p_mu=p_mu,p_er=p_er)

for(i in 1:50)
    {
    p<- plogis(rnorm(length(secid),p_mu,p_er))
    gamma<- plogis(rnorm(nprim,gamma_mu,gamma_re))

    gammaP<-gamma
    gammaPP<-gamma
    phi<- plogis(rnorm(nprim,phi_mu,phi_er))    

## resident fish (1=in,2=out,3=dead)
tmat<-array(0,c(3,3,2,nprim))



for(t_ in 1:nprim)
    {
    ## resident fish
    tmat[1,,1,t_]<-c(phi[t_]*1,0,(1-phi[t_])) # in->in
    tmat[2,,1,t_]<-c(0,0,0) # outside->in
    tmat[3,,1,t_]<-c(0,0,1) # dead->dead

    ## migrant fish
    tmat[1,,2,t_]<-c(phi[t_]*(1-gammaP[t_]),
        phi[t_]*gammaP[t_],1-phi[t_]) # in->in
    tmat[2,,2,t_]<-c(phi[t_]*gammaPP[t_],
        phi[t_]*(1-gammaPP[t_]),1-phi[t_]) # outside->in
    tmat[3,,2,t_]<-c(0,0,1) # dead->dead
    }
ch<-matrix(0,length(Z),length(secid))

## PROCESS MODEL
ZZ<-matrix(0,length(Z),nprim)

## INITIAL STATES
ZZ[which(Z==1),1]<-  1
ZZ[which(Z==2),1]<-  sample(c(1,2),
    length(which(Z==2)),c(1,1),replace=TRUE)

for(t_ in 2:nprim)
    {
    for(j in 1:length(Z))
        {
        ZZ[j,t_]<-which(rmultinom(1,1,
            tmat[ZZ[j,t_-1],,Z[j],t_])==1)
        }
    }

## OBSERVATION MODEL
for(i in 1:length(Z))
    {
    for(j in 1:length(secid))
        {
        avail<-ifelse(ZZ[i,secid[j]]==1,1,0)
        ch[i,j]<-rbinom(1,1,p[j]*avail)        
        }
    }


mod<-function()
    {    
    for(i in 1:M)
        {
        Z[i]~dcat(phi)
        ZZ[i,1]~dcat(ini[Z[i],])
        for(t_ in 2:nprim)
            {
            ZZ[i,t_]~dcat(tmat[ZZ[i,t_-1],,Z[i],t_])
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
    ## DERIVED PARAMETERS
    for(ii in 1:M)
        {
        type[ii,1]<-equals(Z[ii],1)
        type[ii,2]<-equals(Z[ii],2)
        }
    Ntype[1]<-sum(type[,1])
    Ntype[2]<-sum(type[,2])
    for(tt in 1:nprim)
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
        ## TRANSITION MATRICES
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
        } # tt
    ## PRIORS
    a~dunif(0,1)
    phi[1]<-a
    phi[2]<-1-a 
    
    b~dunif(0,1)
    
    ini[1,1]<-1
    ini[1,2]<-0
    ini[2,1]<-b
    ini[2,2]<-1-b
    
    # VARY BETWEEN PRIMARY OCCASIONS
    ## SURVIVAL
    a_S~dnorm(0,0.37)
    sigma_S~dunif(0,1)
    tau_S <- pow(sigma_S, -2)
    ## GAMMA
    a_G~dnorm(0,0.37)
    sigma_G~dunif(0,1)
    tau_G <- pow(sigma_G, -2)    
    for(jj in 1:nprim)
        {
        ### SURVIVAL
        lo_S[jj]~dnorm(a_S,tau_S)
        logit(S[jj])<-lo_S[jj]
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
M=length(Z)
dat<-list(ch=ch,
    nprim=nprim,secid=secid,
    M=M, nocc=nocc)
qq<-rep(0,dat$nocc)    
lo_G<-rep(0,dat$nprim)    
lo_S<-rep(0,dat$nprim)    
inits<-function()
    {list(a=0.3,b=0.3,Z=Z,ZZ=ZZ,
     qq=qq,lo_G=lo_G,lo_S=lo_S,
     lo_p=0,sigma=0.1,
     a_G=0,sigma_G=0.3,
     a_S=0,sigma_S=0.3)}
    
    
params<-c("gamma","a","b","Nhat","Ntype","lo_p",
    "sigma","tau","a_S","sigma_S",
    "a_G","sigma_G")	
out <- R2jags::jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
    export_obj_names=c("Z","ZZ","qq","lo_G","lo_S"),
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())

app<-data.frame(type="EST",
    phi_mu=(out$BUGSoutput$mean$a_S),phi_er=out$BUGSoutput$mean$sigma_S,
    gamma_mu=(out$BUGSoutput$mean$a_G),gamma_re=out$BUGSoutput$mean$sigma_G,
    p_mu=(out$BUGSoutput$mean$lo_p),p_er=out$BUGSoutput$mean$sigma)
 reps<-rbind(reps,app)
   }
    write.csv(reps,"reps.csv")
   
   boxplot(reps[-1,2])
   abline(h=reps[1,2])

   boxplot(reps[-1,3])
   abline(h=reps[1,3]) # bad...
   
   boxplot(reps[-1,4])
   abline(h=reps[1,4]) # negative
   
   boxplot(reps[-1,5])
   abline(h=reps[1,5]) # badd
   
   boxplot(reps[-1,6])
   abline(h=reps[1,6]) # over
   
   boxplot(reps[-1,7]) 
   abline(h=reps[1,7]) # OK- does ok on random effect of capture prob
   
matplot(t(out$BUGSoutput$mean$Nhat),type='b')
matplot(t(out$BUGSoutput$mean$Nhat),type='b')

out$BUGSoutput$median$Ntype