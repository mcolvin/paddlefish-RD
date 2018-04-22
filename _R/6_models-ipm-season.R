
## MODEL 03: INTEGRATED POPULATION MODEL FORMULATION

mod<-function()
    {
    pcap[1]<- 1-exp(sum(log_p0[1,1:noccs[1]])) #PROBABILIYT OF CAPTURE
    ncap[1]~dbin(pcap[1],N_lat[1]) 
    for(iii in 1:3)
        {
        a[iii]~dnorm(0, 0.0001)
        b[iii]~dnorm(0,0.37)
        }
    for(i in 2:D)
        {
        N_lat[i]~dpois(mu1[i]);T(0,160)## MAKE SURE THIS IS LARGER THAN THE PRIOR!
        mu1[i]<-N_lat[i-1]+(r[i]+ pl[i]*N_lat[i-1])
        r[i]<- exp(a[1]+a[2]*X[i,3]+a[3]*X[i,5]) #c(-6,0.02,-0.04)
        logit(pl[i])<- b[1]+b[2]*X[i,3]+b[3]*X[i,5]
        }
        
    N[1]<- sum(Z[,1])   
    for(ii in 2:nprim)
        {
        N[ii]<- sum(Z[,ii])  
        pcap[ii]<- 1-exp(sum(log_p0[ii,1:noccs[ii]])) #PROBABILIYT OF CAPTURE
        ncap[ii]~dbin(pcap[ii],N_lat[tt[ii]])        
        NNtest[ii]<- ncap[ii]/pcap[ii]        
        }    
    # ESTIMATE ABUNDANCE FROM CMR
    for(i in 1:M)
        {
        for(j in 1:nprim)
            {
            ip[i,j]<-omega[j]#*(1-Z_known[i,j])+Z_known[i,j]# INCLUSION PROBABILITY OMEGA OR 1, 1 = ACOUSTIC
            Z[i,j]~dbern(ip[i,j])            
            }
        for(k in 1:nocc)
            {
            ch[i,k]~dbern(Z[i,secid[k]]*p[secid[k],occId[k]])
            }
        }    
        
    # PRIORS
    N1[1]~dunif(10,155)
    N_lat[1]<-round(N1[1])   
    for(i in 1:nprim)
        {

        omega[i]~dunif(0,1)
        for(j in 1:12)
            {
            p[i,j]~dunif(0,1)
            p0[i,j]<- 1-p[i,j]
            log_p0[i,j]<- log(p0[i,j])
            }
        }

    }
    