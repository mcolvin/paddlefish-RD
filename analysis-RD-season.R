library(RODBC)
source("_R/1_global.R")
source("_R/2_functions.R")
run<-FALSE # SET TO TRUE TO REQUERY NOXUBEE GAGE DATA
source("_R/3_load-and-clean-ipm-season.R") #needs internet to pull gage data


library(R2jags)
source("_R/6_models-rd-season.R")
dat<-readRDS("_output/rd-dat-season.RDS")

## SESSION 1: 2/11/2016 TO 7/1/2016
dat$D<-as.integer(as.Date("2016-07-11")-(min(effort$date)))+1L
dat$tt<-dat$tt[dat$tt<=dat$D]
dat$nprim<-length(dat$tt)
dat$secid<- dat$secid[ dat$secid<=dat$nprim]
dat$nocc<-length(dat$secid)
dat$ch<- dat$ch[,c(1:length(secid))]
dat$X<- dat$X[dat$X[,1]<=dat$D,]

## INITIALIZE MODEL
Z<-matrix(1,dat$D,dat$M)
inits<-function(t)
    {
    list(a=0,b=c(0,0),lo_gpp=c(0,0,0),
		lo_gp=c(0,0,0),Z=Z
    )}
params<- c('a',"b","lo_gp","lo_gpp")

ptm <- proc.time()
rundat<- dat[c("D","X",
    "nocc",
    "ch","nprim","secid",
    "M")] 
out <- jags(data=rundat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 1,	
	n.iter = 10,	
	n.burnin = 4,
	#export_obj_names=c("Z"),	
	n.thin=2,
	working.directory=getwd())
proc.time()-ptm
saveRDS(out,"_output/rd-run.RDS")
xx<-update(out,n.iter=2000, n.thin=1) 





    
out <- bugs(data=rundat,
	inits=inits,
	parameters.to.save=params,	
	model.file=paste(getwd(),"_output/mod02.bug",sep="/"),
    debug=TRUE,
    bugs.directory="C:/Users/mcolvin/Documents/WinBUGS14",
	n.chains = 1,	
	n.iter = 50,	
	n.burnin = 25, 
	n.thin=2,
     working.directory=getwd())  

file.show("_output/mod02.bug")    
    
## MODEL 01: ESTIMATES THE PROBABILITY
## ON A DAILY BASIS AND ESTIAMTES MISSING
## STAGE DATA

Z<-matrix(1,dat$M,dat$nprim)  # in or out of pool 
qq<-rep(0,dat$nprim)   # pl  
oo<-rep(0,dat$nprim)   # pl  
inits<-function()
    {list(Z=Z,qq=qq,oo=oo)}
.
inits<-function()
    {list(b=c(0,0),lo_gpp=c(0,0),lo_gp=c(0,0))}
params<- c("b","lo_gp","lo_gpp")
ptm <- proc.time()
rundat<- dat[c("X","D","tag_state","obs_state","ac_meta","N_ac")]
out <- jags.parallel(data=rundat,
	inits=inits,
	parameters=params,	
	model.file=mod01,
	n.chains = 3,	
	n.iter = 5000,	
	n.burnin = 2500, 
	n.thin=2,
	working.directory=getwd())

ptm <- proc.time()-ptm

out$BUGSoutput$mean$psi

plot(out$BUGSoutput$mean$mu_stage,X[,4])
plot(out$BUGSoutput$mean$mu_stage,X[,2])
plot(out$BUGSoutput$mean$mu_stage,X[,3])
plot(out$BUGSoutput$mean$mu_stage,type='l',ylim=c(0,4))
points(X[,4],type='l')

fit<-lm(log(X[,4])~log(X[,3]+X[,2]))#
plot(fitted(fit)~resid(fit))
summary(fit)
out$BUGSoutput$mean$b0
out$BUGSoutput$mean$b1


#######################################################################
#
#  MODEL 0
#
#######################################################################
Z<-matrix(1,dat$M,dat$nprim)  # in or out of pool 
qq<-rep(0,dat$nprim)   # pl  
oo<-rep(0,dat$nprim)   # pl  
inits<-function()
    {list(Z=Z,qq=qq,oo=oo)}
params<- c("qq","oo","Z")
#  RUN MODEL AND TRACK ESTIMATES 
params<- c("qq","oo")
ptm <- proc.time()
out <- R2jags::jags.parallel(data=dat[c("ch","nprim","secid","M","nocc")],
	inits=inits,
	parameters=params,	
	model.file=mod0,
    export_obj_names=c("Z","qq","oo"),
	n.chains = 3,	
	n.iter = 50000,	
	n.burnin = 25000, 
	n.thin=2,
	working.directory=getwd())

saveRDS(out, "_output/ests-Mod0.RDS")


ptm <- proc.time()-ptm


out<- readRDS("_output/ests-Mod0.RDS")
out$BUGSoutput$summary
out$BUGSoutput$mean$Nhat

matplot(t(out$BUGSoutput$mean$Nhat),type='l')




#######################################################################
#
#  MODEL INTIAL VALUES
#
#######################################################################
Z<-rep(1,dat$M) # resident or migrant
ZZ<- matrix(1,dat$M,dat$nprim)
qq<-rep(0,dat$nocc)    
lo_G<-rep(0,dat$nprim)    
lo_S<-rep(0,dat$nprim)    
inits<-function()
    {list(a=0.3,b=0.3,
        Z=Z,
        ZZ=ZZ,
        qq=qq,
        lo_G=lo_G,
        lo_p=0,sigma=0.1,
        a_G=0,sigma_G=0.3,
        beta0=0,beta1=0)}

#######################################################################
#
#  RUN MODEL AND TRACK ESTIMATES
#
#######################################################################     
params<-c("gamma","a","b","Nhat","Ntype","lo_p",
    "sigma","tau","beta0","beta1",
    "a_G","sigma_G","S")	
out <- R2jags::jags.parallel(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod1,
    export_obj_names=c("Z","ZZ","qq","lo_G"),
	n.chains = 3,	
	n.iter = 150000,	
	n.burnin = 60000, 
	n.thin=2,
	working.directory=getwd())

saveRDS(out, "_output/ests.RDS")



out<- readRDS("_output/ests.RDS")
out$BUGSoutput$summary
out$BUGSoutput$mean$Nhat

matplot(t(out$BUGSoutput$mean$Nhat),type='l')



