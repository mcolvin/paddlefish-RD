## V01: single occasion population estimate-done

library(R2jags)
omega<-0.6
lambda<-1
nocc<-4
n<-250
Z<-rbinom(n,1,omega)
indx<- which(Z==1)
Ntrue<-sum(Z)

nprim<- 4
psi<-c(lambda,
    (1-lambda))
S<-matrix(3,n,nprim)
for(i in 1:nprim)
    {
    S[indx,i]<-apply(rmultinom(
        sum(Z),1,psi),
        2,which.max)
    }

p<-0.3
ch<-array(0,c(nrow(states),nocc,nprim))

for(j in 1:nprim)
    {
    for(i in 1:nocc)
        {
        indx<- which(S[,j]==1)
        ch[indx,i,j]<-rbinom(
            length(indx),
            1,p)
        }
    }

    
    
captured1<- apply(ch[,,1],1,max)
acoustic<- sample(which(captured1==1),10,
    replace=FALSE)
tagged<-rep(0,length(states))
tagged[which(apply(ch,1,max)==1)]<-1
tagged[acoustic]<-2
reciever<-c(0,1)
ch_acoustic<- matrix(0,length(states),nprim)
ch_acoustic[which(S[,2]==1),2]<-1

# subset only captured fish
captured<- apply(ch,1,max)
indx<-which(captured==1)
chpit<- ch[indx,,]
chac<- ch_acoustic[indx,]
tagged<-tagged[indx]


primary<- function()
	{ 
    for(i in 1:M)
        {
        Z[i]~dbern(omega)
        }

    for(i in 1:M)
        {
        for(j in 1:nocc)
            {
            cap_p[i,j]<-Z[i]*p
            chpit[i,j]~dbern(cap_p[i,j])
            }
        }
   
    
    ## DERIVED PARAMETERS
    #N[i]<-equals(S[i],1) + equals(S[i],2)
    Nhat<- sum(Z[]) 

	## PRIORS
	omega~dunif(0,1)
	#lambda~dunif(0,1)
	p~dunif(0,1)
    }


ndaug<-250 
daug<- matrix(0,ndaug,nocc)

chpit<-chpit[,,1]
chpit<-chpit[which(rowSums(chpit)>0),]
chpit<-rbind(chpit,matrix(0,ndaug,nocc))
library(abind)
#chpit<-abind(chpit, 
#    array(0, c(ndaug, nocc,nprim)), 
#        along = 1)
chac<-rbind(chac,matrix(0,ndaug,nprim))

S<-matrix(3,nrow(chac),ncol(chac))
## set captured fish as 2
S[which(apply(chpit,c(1),max)==1),]<-2
S[which(apply(chpit,c(1,3),max)+chac>0)]<-1
tagged<-c(tagged,rep(0,ndaug))
dat<-list(
    M=nrow(chpit),
    chpit=chpit,
    #chac=chac,
    #tagged=tagged,
    #nprim=nprim,
    nocc=nocc)
Z<-apply(chpit,1,max)   
inits<- function()
	{
	list(omega=0.5,p=0.1,Z=Z)
	}
params<-c("omega","p","Nhat")	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=primary,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
        out
        Ntrue