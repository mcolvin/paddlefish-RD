
#######################################################################
#
#  LOAD SCRIPTS
#
#######################################################################

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-PIT.R")
source("_R/4_tables.R")
source("_R/5_figures.R")
source("_R/6_models.R")



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
	model.file=mod,
    export_obj_names=c("Z","ZZ","qq","lo_G"),
	n.chains = 3,	
	n.iter = 150000,	
	n.burnin = 60000, 
	n.thin=2,
	working.directory=getwd())

saveRDS(out, "_output/ests.RDS")



