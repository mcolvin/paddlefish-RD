

m<-function()
    {
     for(i in 1:Mp)
        {
        Z[i]~dbern(omega)
        for(l in 1:4)
            {
            chp[i,l]~dbern(Z[i]*p)
            }
        }
    pc<- 1-p0
    p0<- (1-p)*(1-p)*(1-p)*(1-p)
    peff<- p/pc    
    N<-sum(Z[])
    omega~dunif(0,1)
    p~dunif(0,1)
    }
    
xx<-data.frame()
for(jj in 1:10)
    {
## how to adjust p when z is known
omega<- 0.8
p<- 0.3
nocc<- 4
M<-500
Z<- rbinom(M,1,omega)
N<-sum(Z)
indx<-sample(which(Z==1) ,20,replace=FALSE)
ch<-matrix(0,M,nocc)
for(i in 1:nocc)
    {
    ch[,i]<-rbinom(M,1,Z*p)    
    }
colSums(ch)/N    
chp<- ch[which(rowSums(ch)>0),]    
chp<- rbind(chp,matrix(0,500,nocc)) 
Z<-rep(1,nrow(chp))
inits<- function()
	{
	list(omega=0.9,p=0.3,Z=Z)
	}
dat<-list(Mp=nrow(chp),chp=chp)
out <- R2jags::jags(data=dat,
	inits=inits,
	parameters=c("p","N","peff","p0","pc"),	
	model.file=m,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 3000, 
	n.thin=1,
	working.directory=getwd())
    tmp<- as.data.frame(out$BUGSoutput$mean)
    tmp$ptrue<-p
    tmp$Ntrue<-N
xx<-rbind(xx,tmp)
print(jj)
}
  
  xx
hist(xx$p)
hist(xx$N);abline(v=N)
plot(N~Ntrue,xx);abline(0,1)
plot(p~ptrue,xx);abline(0,1)   



m<-function()
    {
 for(ii in 1:Mp)
    {
    Z[ii]~dbern(omega)
    }   
    pc<- 1-p0
    p0<- (1-p)*(1-p)*(1-p)*(1-p)
    peff<- p/pc
    for(l in 1:4)
        {
        for(i in 1:Mp)
            {
            chp[i,l]~dbern(Z[i]*p)
            }
        for(k in 1:Ma)
            {
            cha[k,l]~dbern(p)
            }
        }
    
    N<-sum(Z[])+Ma
    omega~dunif(0,1)
    p~dunif(0,1)
    }
 
    
    
    
xx<-data.frame()
for(kk in 1:20)
    {
## how to adjust p when z is known
omega<- 0.8
p<- 0.2
nocc<- 4
M<-500
Z<- rbinom(M,1,omega)
N<-sum(Z)
A<-rep(0,M)
indx<-sample(which(Z==1) ,20,replace=FALSE)
A[indx]<-1
ch<-matrix(0,M,nocc)
for(i in 1:nocc)
    {
    ch[,i]<-rbinom(M,1,Z*p)    
    }

cha<-ch[which(A==1),]  
chp<-ch[which(A==0),]
chp<- chp[which(rowSums(chp)>0),]    
chp<- rbind(chp,matrix(0,500,nocc)) 
Z<-rep(1,nrow(chp))
inits<- function()
	{
	list(omega=0.9,p=0.3,Z=Z)
	}
dat<-list(Mp=nrow(chp),chp=chp,Ma=nrow(cha),cha=cha)
out <- R2jags::jags(data=dat,
	inits=inits,
	parameters=c("p","N","peff","p0","pc"),	
	model.file=m,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 3000, 
	n.thin=1,
	working.directory=getwd())
    tmp<- as.data.frame(out$BUGSoutput$mean)
    tmp$ptrue<-p
    tmp$Ntrue<-N
xx<-rbind(xx,tmp)
print(kk)
}
  
plot(N~Ntrue,xx);abline(0,1)
plot(p~ptrue,xx);abline(0,1)
hist(xx$p)
hist(xx$N);abline(v=N)
    
ncapi<-colSums(ch)
ncap<- length(which(rowSums(ch)>0))

ncap/sum(Z)
apply(ch[which(Z==1),],2,mean) ## catpure probability
(pcap<- 1-(1-p)^nocc)

apply(ch[which(rowSums(ch)>0),],2,mean)


apply(ch[which(Z==1),],2,mean)
obs<-ch[rowSums(ch)>0,]   
nrow(obs)

tmp<-cbind(ch,A)
obs2<-ch[rowSums(tmp)>0,] 
apply(obs,2,mean)
apply(obs2,2,mean)

10/0.3
10/0.15
 
nrow(ch[which(Z==1),])
tmp<-tmp[which(rowSums(tmp)>0),]
ch[A==1,]
apply(tmp,2,mean)



ch[

