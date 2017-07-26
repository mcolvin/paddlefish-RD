
input<-list()
input$nprim = 10
input$phi=rep(0.8,input$nprim-1)## SURVIVAL
# FECUNDITY
input$f=rep(0,input$nprim-1)
input$n=500 # initial population size
## RANDOM MOVEMENT PRIMARY
input$gam_d_prime=rep(0.3,input$nprim-1) # OBSERVABLE[t-1] --> UNOBSERVABLE[t]; TEMP EMIGRATION
input$nsec=rep(4,input$nprim)## SECONDARY OCCASIONS
## RANDOM MOVEMENT SECONDARY
input$gam_d_prime2<-rep(0,input$nprim) # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
input$p=rep(0.3,input$nprim)## CAPTURE PROBABILITY
inputs<-input
ch<-sim_ch(inputs=input)


library(RMark)
rd<-process.data(data=ch$ch, 
    model="Robust", 
    time.intervals=ch$occs)
S=list(formula=~1)# SURVIVAL
# SHARE = TRUE TO SET C = P
p=list(formula=~1,share=TRUE)# CAPTURE PROBABILITY
f0<- list(formula=~time) # NUMBER NOT ENCOUNTERED
GammaDoublePrime=list(formula=~1,share=TRUE)
GammaPrime=list(formula=~1)
fit<-mark(data = rd, 
    model = "Robust", 
    time.intervals=time.intervals,
    filename="rd",
    model.parameters=list(
        S=S,
        GammaDoublePrime=GammaDoublePrime,
        # GammaPrime=GammaPrime, # not needed when share=TRUE
        p=p),
    threads=2,
    brief=TRUE)
summary(fit)
            

T2 <- input$nsec # number of secondary periods per primary period
T <- length(T2) # number of primary periods
ch_array<-array(0,c(nrow(ch$ch_mat),max(T2),T))
secs<-cbind(cumsum(T2),(cumsum(T2)-T2)+1)
for(i in 1:T)
    {
    ch_array[,,i]<- ch$ch_mat[,secs[i,1]:secs[i,2]]
    }


N<- nrow(ch$ch_mat)
n_aug<- N # doubl up population
y<- array(NA,c(N+n_aug, max(T2),T))
y[1:N,,]<-ch_array
y[(N+1):nrow(y),,]<-0# data augmentation
z<- matrix(1,nrow(y),T)
z[1:N,]<-4
dat<-list(
    M = nrow(y),# number of individuals
    T = length(T2),
    T2 = input$nsec,# vector of number of secondary occasions
    y = y) # array of capture histories [ind, sec occasion, primary occasion]
  

## initial values
## set for each chain
inits<- function()
	{
	list(lo_omega=rnorm(1,0,1),lo_gamma=rnorm(1,0,1),lo_phi=rnorm(1,0,0.37),
        lo_p=rnorm(1,0,0.37),z=z)
	}
	

    
## WHAT PARAMETERS TO KEEP TRACK OF DURING ESTIMATION
params<-c("omega","p","S","lambda","gammaPrime")	

# THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
# AND THE R2jags PACKAGE
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod_RD,
	n.chains = 3,	
	n.iter = 1500,	
	n.burnin = 600, 
	n.thin=2,
	working.directory=getwd())
out  # OUTPUT
out$BUGSoutput$mean$N # ESTIMATED NUMBER OF FISH IN POOL
traceplot(out)# LOOK AT TRACEPLOTS FOR CONVERGENCE.
	
	  


  
m1 <- jags.model(file=modname1,
    data=dat,
    inits=inits,
    n.chains=nchains,
    n.adapt=nadapt) 



     
     
     
     
     
   
# need to add date of tagging     
rd_aux<-process.data(data=ch, 
    groups="acoustic",
    model="Robust", 
    time.intervals=occ)
rd_ddl<-make.design.data(rd_aux)



## SURVIVAL
### TIME ALLOWS HETERGENEITY AMONG
### PRIMARY OCCASIONS
S=list(formula=~time)# SURVIVAL


# NUMBER NOT ENCOUNTERED
f0<- list(formula=~time) 


## MIGRATION
### WE'LL DO RANDOM FOR NOW
### AND FIX PERIODS WHERE STREAM
### IS TOO LOW TO 0?

## RANDOM TE
GammaDoublePrime=list(formula=~1,share=TRUE,fixed=0)
    
    
    
## CAPTURE PROBABILITY
### SHARE = TRUE TO SET C = P
# FIX P TO 1 FOR ACOUSTIC FISH AND 0 FOR PIT TAGS)
p_fixed1_indx<- c(which(rd_ddl$p$acoustic=="Yes" &  
    rd_ddl$p$Time==0))
p_fixed0_indx<- c(which(rd_ddl$p$acoustic=="No" & 
        rd_ddl$p$Time==0))

### SET UP P
p=list(formula=~time:session,share=TRUE,
    fixed= list(
        index = c(p_fixed1_indx, p_fixed0_indx), 
        value=  c(rep(1,length(p_fixed1_indx)),
            rep(0, length(p_fixed0_indx))))) 







fit<-mark(data = rd_aux, 
    model = "Robust", 
    time.intervals=time.intervals,
    model.parameters=list(
        S=S,
        GammaDoublePrime=GammaDoublePrime,
        # GammaPrime=GammaPrime, # not needed when share=TRUE
        p=p),
    threads=2,
    brief=TRUE)
derived<- fit$results$derived$`N Population Size`

summary(fit)

plot(derived$estimate)








