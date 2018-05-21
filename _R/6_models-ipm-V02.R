


mod<-function()
    { 
    mu[1]<- N1
    estAdults[1] ~ dpois(mu[1])
    for(i in 2:D)
        {
        mu[i] <- max(0,min(175,estAdults[i-1]+(r[i]+ pl[i]*estAdults[i-1])))
        estAdults[i] ~ dpois(mu[i])        
        r[i]<- exp(abeta[1]+abeta[2]*X[i,3])
        logit(pl[i])<- bbeta[1]+bbeta[2]*X[i,3]
        }

    for(ii in 1:nprime)
        {
        ncap2[ii]<- ncap[ii]+nb[ii]
        ncap[ii]~dbin(Pcap[ii],estAdults[tt[ii]])        
        } 
    
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
        cN~dunif(10,175)  
        N1<-round(cN)       
    for(i in 1:nprime)
        { 

        Nhat[i]<- sum(Z[,i])+nb[i]
        omega[i]~dunif(0,1)
        for(j in 1:12)
            {
            p[i,j]~dunif(0,1)
            p0[i,j]<- 1-p[i,j]
            }
        Pcap[i]<- 1-prod(p0[i,1:nocc[i]])
        }
    for(iii in 1:2)
        {
        abeta[iii]~dnorm(0, 0.0001)
        bbeta[iii]~dnorm(0,0.37)
        }
    }
    