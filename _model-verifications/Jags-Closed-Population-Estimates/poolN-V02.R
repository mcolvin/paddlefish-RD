# poolN-V02.R
# This code estimates the number of 
# fish in the pool by capture-recapture
# of marked and acoustically marked fish. 

# V02.
# The model assumes capture probability is 
# a function of effort, net soak time, in this case.



library(R2jags)
NatLarge<- 8
N<- 50-NatLarge # TRUE NUMBER OF FISH IN POOL


eff<- c(10,24,35,44)
a= -1.5 
b= 0.05
y_cap<- a+b*eff # effect of effort on logit scale -26 to 26
p_cap<- exp(y_cap)/(1+exp(y_cap) )# convert from logit to probability

plot(eff,p_cap)

# capture history of untagged fish
ch<- matrix(0,nrow=N, ncol=4)
ch[,1]<- rbinom(N,1,p_cap[1])
ch[,2]<- rbinom(N,1,p_cap[2])
ch[,3]<- rbinom(N,1,p_cap[3])
ch[,4]<- rbinom(N,1,p_cap[4])
# drop 0000, fish never captured
ch<- ch[which(apply(ch,1,sum)>0),]


# capture history of acoustically tagged fish
cha<- matrix(0,nrow=NatLarge, ncol=4)
cha[,1]<- rbinom(NatLarge,1,p_cap[1])
cha[,2]<- rbinom(NatLarge,1,p_cap[2])
cha[,3]<- rbinom(NatLarge,1,p_cap[3])
cha[,4]<- rbinom(NatLarge,1,p_cap[4])

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
		y[occ]<- a+b*eff[occ]
		p_cap[occ]<- exp(y[occ])/(1+exp(y[occ]))
		}
	# DERIVED PARAMETERS
	N<-sum(z[]) + Ncha
		
	# PRIORS
	omega~dunif(0,1)
	a~dnorm(0,0.37)
	b~dnorm(0,0.37)
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
	
	
	
