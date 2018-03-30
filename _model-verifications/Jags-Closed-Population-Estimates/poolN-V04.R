# poolN-V02.R
# This code estimates the number of 
# fish in the pool by capture-recapture
# of marked and acoustically marked fish. 

# V02.
# The model assumes capture probability is 
# a function of effort, net soak time, in this case.

# V03.
# USES Copy of Population Matrix from 4-1-16.xlsx
# AS INPUTS


names(ch)
indx<-grep("_0",names(ch))

socc<-cbind(indx,c(indx[-1]-1 ,nc  ol(ch)))

capturehistories<- lapply(1:nrow(socc),function(x)
    {
    out<- ch[,c(1,socc[x,1]:socc[x,2])]
    acousticTagged # 0 for untagged 1 for tagged and at large
    
    })



library(R2jags)

mod<- function()
	{
    
    ## STATE PROBABILITIES
    Sprob[1]<- lambda*omega # IN POOL AND CAPTURABLE
    Sprob[2]<- (1-lambda)*omega # IN CREEK
    Sprob[3]<- 1-omega   # NOT IN POPULATION
   
   
    ## LATENT STATE 1, 2, OR 3
	for(i in 1:M)
		{
		Z[i]~dcat(Sprob)
        }
    ## DETECTION MODEL FOR ALL FISH (PIT AND ACOUSTIC)
    for(i in 1:M)
        {
        for(s in 1:occs)
            {
            cap_p[i,s]<- equals(Z[i],1)*p[s]
            ch[i,s]~bern(cap_p[i,s]
            }
        }


	## DERIVED PARAMETERS
	N<-sum(z[]) + Ncha
	
    ## KNOWN STATES
    for(i in 1:nknown)
        {
        Z[known[i,1]]<-known[i,2] # FILL KNOWN STATES
        }
	
	## PRIORS
	omega~dunif(0,1)
	p~dunif(0,1)
    }

    
    
    
    
    
    
    
dat_aug<- 100
z<- c(rep(1,nrow(ch)),rep(0,dat_aug-nrow(ch)))
ch<-rbind(ch,matrix(0,nrow=dat_aug-nrow(ch) ,ncol=4))# data augmentation needs to be matrix of 0s not NAs for JAGs
dat<- list(ch=ch,
	cha=cha, 
	eff=eff,
	Ncha=nrow(cha), 
	M=nrow(ch))

inits<- function()
	{
	list(a=-2.5,b=0.05,omega=0.5,z=z)
	}
params<-c("a","b","N","omega","p_cap")	
# THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
# AND THE R2jags PACKAGE
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
out
out$BUGSoutput$mean$N

print(out)
traceplot(out)
	
	
	
