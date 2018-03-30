


mod<-function()
    {  
    # S(.); daily survival=constant 
    # Gamma(.); daily movement probability=constant
    # p(.); capture probability=constant w/in primary
    for(i in 1:M)
        {
        Z[i]~dcat(phi)
        ZZ[i,1]~dcat(ini[Z[i],])
        for(t_ in 2:nprim)
            {
            ZZ[i,t_]~dcat(tmat[ZZ[i,t_-1],,Z[i],(t_-1)])
            }
        }   
        
    ## OBSERVATION MODEL PIT TAGGED FISH
    for(m in 1:M)
        {
        for(o in 1:nocc)
            {
            logit(pp[m,o,1])<-qq[o]
            pp[m,o,2]<-0
            pp[m,o,3]<-0            
            ch[m,o]~dbern(pp[m,o,ZZ[m,secid[o]]])
            }
        }
    #######################################################################
    #
    #  DERIVED PARAMETERS
    #
    #######################################################################
    for(ii in 1:M)
        {
        type[ii,1]<-equals(Z[ii],1)# N RESIDENTS
        type[ii,2]<-equals(Z[ii],2)# N MIGRANT
        }
    Ntype[1]<-sum(type[,1]) # RESIDENT
    Ntype[2]<-sum(type[,2]) # MIGRANT
   for(tt in 1:(nprim-1))
        {
        for(m in 1:M)
            {          
            N[m,tt,1]<-equals(ZZ[m,tt],1)
            N[m,tt,2]<-equals(ZZ[m,tt],2)
            N[m,tt,3]<-equals(ZZ[m,tt],3)
            }
        ## STATE SPECIFIC ABUNDANCE ESTIMATES
        Nhat[1,tt]<-sum(N[,tt,1])
        Nhat[2,tt]<-sum(N[,tt,2])
        Nhat[3,tt]<-sum(N[,tt,3]) 
        
        ## TRANSITION MATRICES (TIME VARIANT)
        ### RESIDENT LIFE HISTORY
        tmat[1,1,1,tt]<-1*S[tt]
        tmat[1,2,1,tt]<-0
        tmat[1,3,1,tt]<-1-S[tt]
        tmat[2,1,1,tt]<-0
        tmat[2,2,1,tt]<-0
        tmat[2,3,1,tt]<-0
        tmat[3,1,1,tt]<-0
        tmat[3,2,1,tt]<-0
        tmat[3,3,1,tt]<-1
        ### MIGRANT LIFE HISTORY
        tmat[1,1,2,tt]<-S[tt]*(1-gammaP[tt]) # in -> in
        tmat[1,2,2,tt]<-S[tt]*gammaP[tt] # in-> out
        tmat[1,3,2,tt]<-1-S[tt] # in-> dead
        tmat[2,1,2,tt]<-S[tt]*gammaPP[tt]
        tmat[2,2,2,tt]<-S[tt]*(1-gammaPP[tt])
        tmat[2,3,2,tt]<-1-S[tt]
        tmat[3,1,2,tt]<-0
        tmat[3,2,2,tt]<-0
        tmat[3,3,2,tt]<-1        
        }# tt

    #######################################################################
    #
    #  PRIORS
    #
    #######################################################################
    
    ## PROBABLITY OF BEING A RESIDENT OR A MIGRANT LIFE FORM
    a~dunif(0,1)
    phi[1]<-a
    phi[2]<-1-a 
    
    ## PROBABILITY OF BEING INSIDE OR OUT FOR MIGRATORY FISH
    b~dunif(0,1)    
    ini[1,1]<-1     # RESIDENT FISH ARE ALWAYS IN
    ini[1,2]<-0     # RESIDENT FISH CANNOT BE OUT
    ini[2,1]<-b     # PROBABILITY OF BEING IN STUDY AREA AT T=1 
    ini[2,2]<-1-b   # PROBABILITY OF BEING OUTSIDE STUDY AREA AT T=1
    
    # VARY BETWEEN PRIMARY OCCASIONS
    ## SURVIVAL
    beta0~dnorm(0,0.37)     # INTERCEPT
    beta1~dnorm(0,0.37)     # EFFECT OF X    
    for(dd in 1:D)
        {
        ### DAILY SURVIVAL
        logit(Y[dd])<- beta0 + beta1*X[dd]
        lnS[dd]<- log(Y[dd])
        }
    for(int in 1:(nprim-1))
        {
        S[int]<-exp(sum(lnS[int_str[int]:int_end[int]]))
        }
    ## GAMMA
    a_G~dnorm(0,0.37)
    sigma_G~dunif(0,1)
    tau_G <- pow(sigma_G, -2)    
    for(jj in 1:nprim)
        {
        ### GAMMA
        lo_G[jj]~dnorm(a_G,tau_G)
        logit(gamma[jj])<-lo_G[jj]
        gammaP[jj]<-gamma[jj]
        gammaPP[jj]<-gamma[jj]     
        }
    ## CAPTURE PROBABILITY  
    lo_p~dnorm(0,0.37)
    sigma~dunif(0,1)
    tau <- pow(sigma, -2)    
    for(kk in 1:nocc)
        {
        qq[kk]~dnorm(lo_p,tau)
        }
    }

