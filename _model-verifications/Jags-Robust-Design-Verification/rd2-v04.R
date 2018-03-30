## V01: single occasion population estimate-done
## V02: 4 occasion RD - done
## V03: same as 2 but with acoustic - done
## V04: adding the far way away state


# # http://warnercnr.colostate.edu/~gwhite/mark/markhelp/linkfunctions.htm
 # mlogit
# psi A to B = exp(B1)/[1 + exp(B1) + exp(B2) + exp(B3)]
# psi A to C = exp(B2)/[1 + exp(B1) + exp(B2) + exp(B3)]
# psi A to D = exp(B3)/[1 + exp(B1) + exp(B2) + exp(B3)]
# psi B to A = exp(B4)/[1 + exp(B4) + exp(B5) + exp(B6)]
# psi B to C = exp(B5)/[1 + exp(B4) + exp(B5) + exp(B6)]
# psi B to D = exp(B6)/[1 + exp(B4) + exp(B5) + exp(B6)]

X<-runif(300,0.2,5)
X[200]<-12
plot(X,type='l')

beta0<- c(0,0,1)


library(R2jags)
omega<-0.6
lambda<-0.7
nocc<-4
n<-250
Z<-rbinom(n,1,omega)
indx<- which(Z==1)
Ntrue<-sum(Z)

nprim<- 4
nstates<- 3
psi<-matrix(0,nstates,nprim)

psi[,1]<-c(lambda,(1-lambda),0)
psi[,2]<-c(lambda,(1-lambda),0)
psi[,3]<-c(lambda,(1-lambda),0.1)
psi[,3]<-psi[,3]/sum(psi[,3])
psi[,4]<-c(lambda,(1-lambda),0)
    
    
    
S<-matrix(NA,n,nprim)
for(i in 1:nprim)
    {
    S[indx,i]<-apply(rmultinom(
        sum(Z),1,psi[,i]),
        2,which.max)
    }

p<-0.3
ch<-array(0,c(nrow(S),nocc,nprim))

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

## ADD ACOUSTIC TAGS 5 PER PRIMARY OCCASION    
ac_tagged<-data.frame(
    whenTagged=0,
    whoTagged=0)
for(i in 1:nprim)
    {
    indx<-(which(rowSums(ch[,,i])>0))
    indx<-indx[!(indx %in% 
        ac_tagged$whoTagged)]
    tmp<-data.frame(whenTagged=i,
        whoTagged=sample(indx,5,replace=FALSE))
    ac_tagged<-rbind(ac_tagged,tmp)
    }
ac_tagged<-ac_tagged[-1,]

## MATRIX OF IF ACOUSTICALLY TAGGED 
acoustic<- matrix(0,nrow(S),
    nprim)
for(i in ac_tagged$whoTagged)
    {
    acoustic[i,c(ac_tagged[
        which(ac_tagged$whoTagged==i),]$whenTagged:nprim)]<-1
    }
## CENSOR MARKING OCCASION
acoustic[cbind(ac_tagged$whoTagged,
    ac_tagged$whenTagged)]<-0

## SET UP ACOUSTIC DETECTIONS    
ch_acoustic<- matrix(0,nrow(S),nprim)
ch_acoustic[S==1]<-1
ch_acoustic<-ch_acoustic*acoustic

# subset only captured fish
captured<- apply(ch,1,max)
indx<-which(captured==1)
chpit<- ch[indx,,]
chac<- ch_acoustic[indx,]

primary<- function()
	{ 
    for(i in 1:M)
        {
        Z[i]~dbern(omega)
        for(j in 1:nprim)
            {
            psi[i,1,j]<- lambda*Z[i]
            psi[i,2,j]<- (1-lambda)*Z[i]
            psi[i,3,j]<- 1-Z[i] 
            S[i,j]~dcat(psi[i,1:3,j])
            }
        }       
        
    for(i in 1:M)
        {
        for(j in 1:nocc)
            {
            for(k in 1:nprim)
                {
                cap_p[i,j,k]<-equals(S[i,k],1)*p
                chpit[i,j,k]~dbern(cap_p[i,j,k])
                }
            }
        }
    for(i in 1:M)
        {
        for(k in 1:nprim)
            {
            p_ac[i,k]<- max(0.0001,min(equals(acoustic[i,k],1)*
                equals(S[i,k],1),0.9999))
            chac[i,k]~dbern(p_ac[i,k])
            }
        }
      
    
    ## DERIVED PARAMETERS
    #N[i]<-equals(S[i],1) + equals(S[i],2)
    Nhat<- sum(Z[]) 

	## PRIORS
	omega~dunif(0,1)
	lambda~dunif(0,1)
	p~dunif(0,1)
    }


ndaug<-250 
daug<- matrix(0,ndaug,nocc)

library(abind)
chpit<-abind(chpit, 
    array(0, c(ndaug, nocc,nprim)), 
        along = 1)
chac<-rbind(chac,matrix(0,ndaug,nprim))
acoustic<-rbind(acoustic,matrix(0,ndaug,nprim))

S<-matrix(3,nrow(chac),ncol(chac))
## set captured fish as 2
S[which(apply(chpit,c(1),max)==1),]<-1

tagged<-c(tagged,rep(0,ndaug))
dat<-list(
    M=nrow(chpit),
    chpit=chpit,
    chac=chac,
    acoustic=acoustic,
    nprim=nprim,
    nocc=nocc)
Z<-apply(chpit,1,max)   
inits<- function()
	{
	list(omega=0.5,p=0.1,Z=Z,lambda=0.2,S=S)
	}
params<-c("omega","p","Nhat","lambda")	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=primary,
	n.chains = 3,	
	n.iter = 1500,	
	n.burnin = 600, 
	n.thin=2,
	working.directory=getwd())
        out
        Ntrue
        
        
        
        
        