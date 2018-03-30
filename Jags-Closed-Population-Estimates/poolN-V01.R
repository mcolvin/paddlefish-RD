# poolN-V02.R
# This code estimates the number of 
# fish in the pool by capture-recapture
# of marked and acoustically marked fish for a single occasion. 

# V01.
# The model assumes capture probability is 
# constant among capture occasions.


## SIMULATE CAPTURE-RECAPTURE DATA
NatLarge<- 8 # NUMBER OF FISH ACOUSTICAL TAGGED AT LARGE
N<- 50-NatLarge # TRUE NUMBER OF FISH IN POOL, LESS THE ACCOUSTIC FISH

nocc<- 4 # NUMBER OF NET SETS
a = rep(-1.5,nocc)   # INTERCEPT OF A LOGIT LINEAR MODEL
y_cap<- a # constant capture probability on logit scale
p_cap<- exp(y_cap)/(1+exp(y_cap) )# convert from logit to probability


# capture history of untagged fish
ch<- matrix(0,nrow=N, ncol=nocc)
ch[,1]<- rbinom(N,1,p_cap[1])# BINOMIAL PROCESS OF CAPTURE 
ch[,2]<- rbinom(N,1,p_cap[2])# BINOMIAL PROCESS OF CAPTURE 
ch[,3]<- rbinom(N,1,p_cap[3])# BINOMIAL PROCESS OF CAPTURE 
ch[,4]<- rbinom(N,1,p_cap[4])# BINOMIAL PROCESS OF CAPTURE 
# drop 0000, fish never captured
ch<- ch[which(apply(ch,1,sum)>0),] # FISH NEVER CAPTURE DO NOT COUNT HERE
# BECAUSE WE CANNOT DIFFERENTIATE IF THEY WERE NOT CAPTURED OR JUST NOT IN 
# THE POOL


# capture history of acoustically tagged fish
cha<- matrix(0,nrow=NatLarge, ncol=4)
cha[,1]<- rbinom(NatLarge,1,p_cap[1])
cha[,2]<- rbinom(NatLarge,1,p_cap[2])
cha[,3]<- rbinom(NatLarge,1,p_cap[3])
cha[,4]<- rbinom(NatLarge,1,p_cap[4])
# ACOUSTICALLY TAGGED FISH DO COUNT IF THEY ARE NOT CAPTURED, 
# WE KNEW THEY WERE IN THE POOL BUT JUST NOT CAPTURED
ch # SIMULATED CAPTURE HISTORY FOR THE 4 SETS
cha# SIMULATED CAPTURE HISTORY FOR ACOUSTICALLY TAGGED FISH


## end simulated data








## estimate population using a state-space formulation of 
## a capture-recapture models (in this case M_o, Otis 1978). 
## the JAGS model uses data augmentation, basically filling the 
## capture history with a bunch of 0000, and then estimating 
## which one of those is 'occupied' based on the latent Z vector.   

### bugs model
# install.packages("R2jags") # run if needed
library(R2jags)
mod<- function()
	{
	for(i in 1:M)
		{
		z[i]~dbern(omega) # LATENT VARIABLE, DATA AUGMENTATION
		for(j in 1:4)
			{
			p_eff[i,j]<- z[i]*p_cap[j] # CONDITIONAL CAPTURE PROBABILITY
			ch[i,j]~dbern(p_eff[i,j])			
			}#i
		}#j
	# ACOUSTICALLY TAGGED FISH
	for(i in 1:Ncha)
		{
		for(j in 1:4)
			{
			cha[i,j]~dbern(p_cap[j])
			}
		}
#	# CAPTURE PROBABILITY AS A FUNCTION OF EFFORT
	for(occ in 1:4)
		{
		y[occ]<- a
		p_cap[occ]<- exp(y[occ])/(1+exp(y[occ]))
		}
	# DERIVED PARAMETERS
	N<-sum(z[]) + Ncha
		
	# PRIORS
	omega~dunif(0,1)
	a~dnorm(0,0.37)
    }

## number of rows to augment capture histories
## this should be more than you expect the population to be
dat_aug<- 100

## vectors of 0s and 1s indicating whether fish is 
## in the pool or not
z<- c(rep(1,nrow(ch)),rep(0,dat_aug-nrow(ch)))
## capture history plus extra rows of 0s 
ch<-rbind(ch,matrix(0,nrow=dat_aug-nrow(ch) ,ncol=4))# data augmentation needs to be matrix of 0s not NAs for JAGs

## bundle up data for JAGs
dat<- list(ch=ch,# capture history of non-acoustic fish
	cha=cha, # capture history of acoustic fish
	Ncha=nrow(cha), # number of acoustically tagged fish in pool at sampling
	M=nrow(ch))# Number of rows in the augmented capture history matrix
	
## initial values
## set for each chain
inits<- function()
	{
	list(a=-2.5,omega=0.5,z=z)
	}
	
## WHAT PARAMTERS TO KEEP TRACK OF DURING ESTIMATION
params<-c("a","N","omega","p_cap")	

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
out  # OUTPUT
out$BUGSoutput$mean$N # ESTIMATED NUMBER OF FISH IN POOL
traceplot(out)# LOOK AT TRACEPLOTS FOR CONVERGENCE.
	
	
	
