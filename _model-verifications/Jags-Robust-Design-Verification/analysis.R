
source("_R/1_global.R")  	
source("_R/2_functions.R")         
#source("_R/3_load.R")     
source("_R/3_load-PIT.R")     
#source("_R/3_load-acoustic.R")     
source("_R/5_tables.R") 
source("_R/6_figures.R")  


## lamba is long term probability of being inside or out. if gamma'==gamma''
gammaPrime<- 0.3
gammaPrimePrime<- gammaPrime

(1-gammaPrime)/(gammaPrimePrime-gammaPrime+1)



gprime<- 0.3
gprimeprime<- 0.5



mod<- function()
	{        
	###################################################################	
	# PRIORS
    ###################################################################
    #lo_omega~dnorm(0,0.37)# INCLUSION PROBABILITY (LOG ODDS)
    lo_gamma~dnorm(0,0.37)# PROBABLITY OF MOVING  (LOG ODDS)
    lo_phi~dnorm(0,0.37) # log odds phi; survival
    lo_p~dnorm(0,0.37) # log odds p
        
    for(d in 1:nprim)
        {        
#       logit(omega[d])<- lo_omega
        logit(S[d])<- lo_phi
        logit(gammaPrime[d])<- lo_gamma
        logit(gammaPrimePrime[d])<- lo_gamma
        #logit(p[d])<-lo_p
        lambda[d] <- (1-gammaPrime[d])/(gammaPrimePrime[d]-gammaPrime[d]+1) 
        }
    ## INITIAL STATES
    tmat0[1]<- lambda[1] 
    tmat0[2]<- 1-lambda[1] 
    tmat0[3]<- 0

    
    
    
    for(k in 1:nprim)
        {
        # TRANSITION MATRIX
        # 1 = ONSITE, 2 = OFFISITE, 3 = DEAD
        # TRANSITION ROWS -> COLUMNS
        
        ## ONSITE
        tmat[1,1,k]<- S[k]*gammaPrimePrime[k]
        tmat[1,2,k]<- S[k]*(1-gammaPrimePrime[k])
        tmat[1,3,k]<- (1-S[k])*(1-gammaPrimePrime[k]) + (1-S[k])*(gammaPrimePrime[k])

        ## OFFSITE
        tmat[2,1,k]<- S[k] * gammaPrime[k]
        tmat[2,2,k]<- S[k] * (1-gammaPrime[k])
        tmat[2,3,k]<- (1-S[k])*(1-gammaPrime[k]) + (1-S[k])*(gammaPrime[k])        
      
        ## DEAD
        tmat[3,1,k]<- 0
        tmat[3,2,k]<- 0
        tmat[3,3,k]<- 1 # DEAD -> DEAD
        }
    ## LATENT STATE
    for(m in 1:M)
        {
        Z[m,1]~ dcat(tmat0) 
        for(i in 2:nprim)
            {
            Z[m,i] ~ dcat(tmat[Z[m,i-1],1:3,i])# STATE CONDITIONAL ON PREVIOUS STATE
            }
        } 
        
    ###################################################################
    # THE OBSERVATION MODEL    
    ###################################################################    
    for(j in 1:T)
        {
        logit(p[1,j])<- lo_p
        p[2,j]<-0
        p[3,j]<-0        
        for(m in 1:M)
            {           

            ch[m,j] ~ dbern(p[Z[m,occasionId[j]],j]) 
            }    
        }
    }
   


mod1<- function()
	{
    lo_p~dnorm(0,0.37)#%_%T(-10,10)
    
    for(i in 1:nprim)
        {
        logit(p[i])<-lo_p
        }
    
    for(m in 1:M)
        {
        for(j in 1:T)
            {
            ch[m,j] ~ dbern(p[occasionId[j]]) 
            }    
        }
    }

Z<-matrix(1,dat$M,dat$nprim)
inits<-function()
    {
    list(lo_p=0,lo_gamma=0, lo_phi=0,Z=Z)
    }    
params<-c("lo_p","lo_gamma","lo_phi")	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 1500,	
	n.burnin = 600, 
	n.thin=2,
	working.directory=getwd())
plot(out)
out

dat$M

























	
```{r,eval=FALSE}
source("./R/7_analysis.R")
```	



```{r,echo=FALSE, eval=FALSE}
par(oma=c(0,0,0,0),mar=c(3,3,0.25,1))
figures(1)
figures(2)
```
   
## Appendix

BUGs model used to estimate abundance using auxiliary information
from acoustically tagged fish.
```{r,echo=FALSE}
print(mod)
```
