

mod<-function()
    {    
    ## PROCESS MODEL
    ### ASSIGNS FISH AS INSIDE OR OUTSIDE 
    for(ind in 1:M)
        {
        for(t_ in 1:nocc)
            {
            pp[ind,t_]<-equals(Z[t_,ind],1)*p[secid[t_]]
            ch[ind,t_]~dbern(pp[ind,t_])
            }
        Z[1,ind]~dcat(tmat[1,1:3,1])        # INITIAL STATES
        for(dd in 2:D)
            {
            Z[dd,ind]~dcat(tmp[dd,1:3,ind])                       
            tmp[dd,1,ind]<-equals(Z[dd,ind],1)*tmat[dd,1,1]+
                equals(Z[dd,ind],2)*tmat[dd,1,2]+
                equals(Z[dd,ind],3)*tmat[dd,1,3]
            tmp[dd,2,ind]<-equals(Z[dd,ind],1)*tmat[dd,2,1]+
                equals(Z[dd,ind],2)*tmat[dd,2,2]+
                equals(Z[dd,ind],3)*tmat[dd,2,3]                
            tmp[dd,3,ind]<-equals(Z[dd,ind],1)*tmat[dd,3,1]+
                equals(Z[dd,ind],2)*tmat[dd,3,2]+
                equals(Z[dd,ind],3)*tmat[dd,3,3] 
            }
    
        }
        
        
        
    ### PROCESS MODEL
    #### INITALIZE SUPER POPULATION
    tmat[1,1,1]<-omega*(1-ini)          # in 
    tmat[1,2,1]<-omega*ini              # out
    tmat[1,3,1]<-1-omega                # not in population
    
    tmat[1,1,2]<- 0
    tmat[1,2,2]<- 0
    tmat[1,3,2]<- 0
   
    tmat[1,1,3]<- 0      
    tmat[1,2,3]<- 0       
    tmat[1,3,3]<- 0

    for(dd in 2:D)
        {
        logit(gamma2prime[dd])<-lo_gpp[1]+
            lo_gpp[2]*X[dd,3] +
            lo_gpp[3]*X[dd,5] 
        logit(gammaprime[dd])<-lo_gp[1]+
            lo_gp[2]*X[dd,3] +
            lo_gp[3]*X[dd,5] 
        # DAILY STATE TRANSITIONS: 1=pool, 2= outside, 
        # 3=not in population, 3=translocated
        tmat[dd,1,1]<- 1-gamma2prime[dd]      # in to in
        tmat[dd,2,1]<- gamma2prime[dd]        # in to out
        tmat[dd,3,1]<- 0                  # in to out
        tmat[dd,1,2]<- 1-gammaprime[dd]       # out to in
        tmat[dd,2,2]<- gammaprime[dd]        # out to out
        tmat[dd,3,2]<- 0                 # out to out
        tmat[dd,1,3]<- 0      
        tmat[dd,2,3]<- 0        
        tmat[dd,3,3]<- 1
        # PREDICT MISSING OKTOC STAGES
        mu_stage[dd]<-b[1]+b[2]*X[dd,3]
        X[dd,4]~dnorm(mu_stage[dd],tau)# inverse of variance 
        }
    # PRIORS
    ## OVERALL INCLUSION PROBABILITY
    omega~dunif(0,1)
    ## INITIAL STATE
    a~dnorm(0,0.37)
    logit(ini)<-a
    ## STAGE MODEL
    b[1]~dnorm(0,0.001)
    b[2]~dnorm(0,0.001)
    tau~dgamma(0.001,0.001)# inverse of variance
    sigma<-1/sqrt(tau)
    ## TRANSITION PROBABILITY PARAMETERS (1->2 AND 2->1)
    ### 1->2: GAMMA''
    lo_gpp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gpp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gpp[3]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    ### 2->1: GAMMA'
    lo_gp[1]~dnorm(0,0.37)## CAPTURE PROBABILITY 
    lo_gp[2]~dnorm(0,0.37)## CAPTURE PROBABILITY
    lo_gp[3]~dnorm(0,0.37)## CAPTURE PROBABILITY
    ### CAPTURE PROBABILITY
    for(k in 1:nprim)
        {
        p[k]~dunif(0,1)
        }
}