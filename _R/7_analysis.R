
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
