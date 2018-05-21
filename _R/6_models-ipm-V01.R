
## MODEL 03: INTEGRATED POPULATION MODEL FORMULATION

mod<-function()
    { 
    # ESTIMATE DAILY ABUNDANCE FROM CMR
    for(primocc in 1:nprime)
        {
        for(inda in 1:na)
            {
            Z[inda,primocc]~dbern(omega[primocc])
            for(secocc in 1:nocc[primocc])
                {
                a[inda,secocc,primocc]~dbern(Z[inda,primocc]*p[primocc,secocc])
                }
            }
        }
    for(primoccb in 2:nprime)
        {
        for(indb in 1:nb[primoccb])
            {
            for(secoccb in 1:nocc[primoccb])
                {
                b[indb,secoccb,primoccb]~dbern(p[primoccb,secoccb])
                }
            }
        }    
        
    # PRIORS & DERIVED
    for(i in 1:nprime)
        {
        
        Nhat[i]<- sum(Z[,i])+nb[i]
        #N1[i]~dunif(10,155)
        #N_lat[i]<-round(N1[i])
        omega[i]~dunif(0,1)
        for(j in 1:12)
            {
            p[i,j]~dunif(0,1)
            p0[i,j]<- 1-p[i,j]
            }
        }

    }
    