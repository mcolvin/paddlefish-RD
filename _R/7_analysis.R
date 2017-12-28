
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






#######################################################################
#
#  M0 MODEL
#
#######################################################################



NN<-rep(NA,40)
indx<- c(4:7,10,16,19:40)
for(i in indx)
    {

    dat<-makeData_MO(i) 
    if(is.null(dat$ret)&sum(dat$ch)>0)
        {
        z<- rep(1,nrow(dat$ch))
        inits<- function()
            {
            list(a=-2.5,omega=0.5,z=z)
            }
        params<-c("a","N","omega","p_cap")

            # THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
            # AND THE R2jags PACKAGE
            out <- try(jags(data=dat,
                inits=inits,
                parameters=params,	
                model.file=mod,
                n.chains = 3,	
                n.iter = 15000,	
                n.burnin = 6000, 
                n.thin=2,
                working.directory=getwd()))
            NN[i]<-if(class(out)!="try-error") {out$BUGSoutput$mean$N}
        }
    }
    

out<-cbind(primary_occasions,NN)

plot(NN~year_f,out,ylim=c(0,125))
presPlot()
plot(NN~year_f,out,ylim=c(0,125),type='h',
    ylab="Abundance",xlab="Year",lwd=5)



## THE VR2 WAS IN THE POOL FOR GOOD 2016 DOY  189
