modRD2<-function() ## swap out dcat for dbern
    { 
    ## DAY = 1
    for(i in 1:M)
        {
        Z[i]~dbern(omega)         ## IN POPULATION OR NOT
        pool[i,1]~dbern(Z[i]*ini) ## INSIDE OR OUTSIDE CONDITIONAL ON BEING IN POPULATION
        #nox[i,1]<-Z[i]*(1-pool[i,1]) ## OUTSIDE 
        for(d in 2:D)
            {
            pool[i,d]~dbern(Z[i]*pool[i,d-1]*(1-gamma2prime))
            
            pp[i,d]<- Z[i]*(pool[i,d]*(1-gamma2prime)+(1-pool[i,d])*(gammaprime))
            
            pool_obs[i,d]~dbern(pp[i,d])
            #nox[i,d]<-Z[i]*(1-pool[i,d]) ## OUTSIDE 
            } # d: all days
        } # M: all individuals               
    gamma2prime~dunif(0,1)
    gammaprime~dunif(0,1)
    omega~dunif(0,1)
    ini~dunif(0,1)
    }




modRD<-function()
    { 
    ## ASSIGN INITIAL STATE: 1=IN POOL, 2=OUTSIDE POOL, 3=NOT IN POPULATION
    tmat[1,1,1]<-omega*(ini)      # INPOOL
    tmat[1,2,1]<-omega*(1-ini)          # OUTSIDE POOL
    tmat[1,3,1]<-1-omega                    # NOT IN POPULATION
    
    tmat[1,1,2]<-0      
    tmat[1,2,2]<-0                 
    tmat[1,3,2]<-1 
    # NOT IN POPULATION
    tmat[1,1,3]<-0       
    tmat[1,2,3]<-0                   
    tmat[1,3,3]<-1 
   
    for(d in 2:D)
        {
        tmat[d,1,1]<-1-gamma2prime      # in to in
        tmat[d,2,1]<-gamma2prime        # in to out
        tmat[d,3,1]<-0                  # in to out
       
        tmat[d,1,2]<-1-gammaprime       # out to in
        tmat[d,2,2]<- gammaprime        # out to out  
        tmat[d,3,2]<- 0                 # out to out  

        tmat[d,1,3]<-0       
        tmat[d,2,3]<-0                   
        tmat[d,3,3]<-1
        }# end day
    ## DAY = 1
    for(i in 1:M)
        {
        Z[i,1]~dcat(tmat[1,1:3,1])
        for(d in 2:D)
            {        
            tprob[i,1,d]<- equals(Z[i,d-1],1)*tmat[d,1,1]+equals(Z[i,d-1],2)*tmat[d,1,2]+equals(Z[i,d-1],3)*tmat[d,1,3]
            tprob[i,2,d]<- equals(Z[i,d-1],1)*tmat[d,2,1]+equals(Z[i,d-1],2)*tmat[d,2,2]+equals(Z[i,d-1],3)*tmat[d,2,3]
            tprob[i,3,d]<- equals(Z[i,d-1],1)*tmat[d,3,1]+equals(Z[i,d-1],2)*tmat[d,3,2]+equals(Z[i,d-1],3)*tmat[d,3,3]
            Z[i,d]~dcat(tprob[i,1:3,d])
            } # d: all days
        } # M: all individuals               
    gamma2prime~dunif(0,1)
    gammaprime~dunif(0,1)
    omega~dunif(0,1)
    ini~dunif(0,1)
    }


    
    
    


mod02<-function()
    {
    ## PIT TAG MODEL
    for(indp in 1:M_p)
        {
        for(t_p in 1:nocc)
            {        
            ch_p[indp,t_p]~dbern(ZZ_p[dayid[t_p],indp]*p[secid[t_p]]) 
            }      
                ### PROCESS MODEL        
        ZZ_p[1,indp]~dbern(ini)
        for(ddd in 2:D)
            {
            ZZ_p[ddd,indp]~dbern(tmpp[ddd,indp])  ## 1 = in, 0 = out                     
            tmpp[ddd,indp]<-ZZ_p[ddd-1,indp]*psi[ddd,1]+
                (1-ZZ_a[ddd-1,indp])*psi[ddd,2]
            }
        }
       
    ## PROCESS MODEL
    ### ASSIGNS FISH AS INSIDE OR OUTSIDE 
    for(ind in 1:M_a)
        {
        ### RECIEVER CAPTURES
        #for(day in ac_meta[ind,1]:ac_meta[ind,2]) ## loop over days with data
        for(day in 1:D) ## loop over days with data
            {
            pp_aa[day,ind]<-ZZ_a[day,ind]*obs_state_p[day,ind]           
            obs_state[day,ind]~dbern(obs_state_p[day,ind]) # PERFECT DETECTION
            }        
        ### PHYSICAL CAPTURES
        for(t_ in 1:nocc)
            {
            pp_a[ind,t_]<-ZZ_a[dayid[t_],ind]*p[secid[t_]]
            ch_a[ind,t_]~dbern(pp_a[ind,t_])
            }
        ### PROCESS MODEL        
        ZZ_a[1,ind]~dbern(ini)
        for(dd in 2:D)
            {
            ZZ_a[dd,ind]~dbern(tmp[dd,ind])  ## 1 = in, 0 = out                     
            tmp[dd,ind]<-ZZ_a[dd-1,ind]*psi[dd,1]+(1-ZZ_a[dd-1,ind])*psi[dd,2]
            }
        }
    ## PREDICT STAGE FOR OKTOC FROM NOXUBEE
    for(i in 1:D)
        {
        logit(gammaprimeprime[i])<-lo_gpp[1]+lo_gpp[2]*X[i,3] +lo_gpp[3]*X[i,5] 
        logit(gammaprime[i])<-lo_gp[1]+lo_gp[2]*X[i,3] +lo_gp[3]*X[i,5] 
        # DAILY STATE TRANSITIONS: 1=pool, 2= outside, 3=translocated
        psi[i,1]<- 1-gammaprimeprime[i]   # in-->in 
        psi[i,2]<- 1-gammaprime[i]        # out-->in
        #psi[i,2]<- gammaprimeprime[i]     # in-->out        
        #psi[i,2]<- gammaprime[i]          # out-->out

        # PREDICT MISSING OKTOC STAGES
        mu_stage[i]<-b[1]+b[2]*X[i,3]
        X[i,4]~dnorm(mu_stage[i],tau)# inverse of variance 
        }

    # PRIORS
    
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
    
    
    
if(load==1)
    {
    library(RODBC)
    source("_R/1_global.R")
    source("_R/2_functions.R")
    run<-FALSE # SET TO TRUE TO REQUERY NOXUBEE GAGE DATA
    source("_R/3_load-and-clean-rd.R") #needs internet to pull gage data
    }