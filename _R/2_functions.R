mod_RD<- function()
	{
        
	###################################################################	
	# PRIORS
    ###################################################################
    lo_omega~dnorm(0,0.37)# INCLUSION PROBABILITY (LOG ODDS)
    lo_gamma~dnorm(0,0.37)# PROBABLITY OF MOVING  (LOG ODDS)
    lo_phi~dnorm(0,0.37) # log odds phi
    lo_p~dnorm(0,0.37) # log odds p
        
    for(d in 1:10)
        {        
        logit(omega[d])<- lo_omega
        logit(S[d])<- lo_phi
        logit(gammaPrime[d])<- lo_gamma
        logit(gammaPrimePrime[d])<- lo_gamma
        logit(p[d])<-lo_p
        lambda[d] <- (1-gammaPrime[d])/(gammaPrimePrime[d]-gammaPrime[d]+1) 
        }

    for(k in 1:10)
        {
        #alive_i[i,t_] <- step(z[i,t_]-3) # check alive or not
        #Nin_i[i,t_] <- equals(z[i,t_],4) # count if i is within study area
        
        # recruitment process from 'eigenvector decomposition'
        # recruitment ratio, or long-term prob of being inside     

        
        # TRANSITION MATRIX
        # 1 =not yet in population;2=dead;3=offsite;4=onsite (only observable state)
        # transition are from the column --> rows

        tmat[1,1,k]<- 1-omega[k] # not in population yet
        tmat[2,1,k]<- 0
        tmat[3,1,k]<- omega[k]*lambda[k]
        tmat[4,1,k]<- omega[k]*(1-lambda[k])
        
        # DEAD
        tmat[1,2,k]<-0
        tmat[2,2,k]<-1 # STAY DEAD
        tmat[3,2,k]<-0
        tmat[4,2,k]<-0
        # OFFSITE
        tmat[1,3,k]<-0
        tmat[2,3,k]<-1-S[k] # DIES 
        tmat[3,3,k]<-0.7#gammaPrime[k]*omega[k] # outside -> outside
        tmat[4,3,k]<-0.7#(1-gammaPrime[k])*omega[k] # outside -> inside
        # ONSITE
        tmat[1,4,k]<- 0
        tmat[2,4,k]<- 1-S[k]
        tmat[3,4,k]<- gammaPrimePrime[k]*omega[k] # inside -> outside
        tmat[4,4,k]<- (1-gammaPrimePrime[k])*omega[k] # inside-> inside
        }


    ###################################################################
    # THE MODEL    
    ###################################################################    
    for(m in 1:M)
        {
        z[m,1]~ dcat(tmat[1:4,1,1]) 
        for(j in 1:T2[1])
            {
            y[m,j,1] ~ dbern(p[1]*equals(z[m,1],4)) 
            }    
        for(i in 2:T)
            {
            z[m,i] ~ dcat(tmat[1:4, z[m,i-1],i])
            for(j in 1:T2[i])
                {
                y[m,j,i] ~ dbern(p[i]*equals(z[m,i],4)) 
                } 
            }
        }        
    }
   
    
    
sim_ch<-function(inputs,...)
    {library(rowr)
    ## SET UP TIME INTERRVALS
    ## FOR PROGRAM MARK
    ends<-cumsum(inputs$nsec) # last sampling occasion
    occs<- rep(0,sum(inputs$nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing

    # SET UP SURVIVAL
    S<-occs
    S[which(S==1)]<-inputs$phi
    S[which(S==0)]<-1
    
    # SET UP MOVEMENT
    ## USES GAMMA DOUBLE PRIME PROBABILITY OF BEING IN OUTSIDE STUDY AREA
    ## probability of temporarily emigrating from the observable sample during
    ## an interval is the same as the probability of staying away
    GammaDblPrime<-occs    
    GammaDblPrime[which(GammaDblPrime==1)]<-inputs$gam_d_prime
    GammaDblPrime[which(GammaDblPrime==0)]<-unlist(lapply(1:inputs$nprim,
        function(x) {rep(inputs$gam_d_prime2[x],inputs$nsec[x]-1)}))

    # SET UP RECRUITMENT
    f<- occs
    f[which(f==1)]<-inputs$f
    f[which(f==0)]<-0
    
    
    
    
    
    
    ## MAKE THE SUPER POPULATION AT T=1
    Z<- matrix(0,inputs$n,sum(inputs$nsec))
    Z_inn<-Z
    Z_out<-Z
    Z[,1]<-1   
    Z_inn[,1]<-rbinom(nrow(Z),1, 1-inputs$gam_d_prime[1])   
    Z_out[,1]<-1-Z_inn[,1]  
    
    ## SIMULATE POPULATION DYNAMICS
    ### SIMULATES PRIMARY AND SECONDARY OCCASIONS...
    for(i in 2:ncol(Z))
        {
        ## SURVIVAL
        Z[,i]<- rbinom(n=nrow(Z),
            size=1,
            prob=S[i-1]*Z[,i-1])
        ## MOVEMENT: RANDOM
        if(GammaDblPrime[i-1]==0)
            {
            Z_inn[,i]<- Z_inn[,i-1]*Z[,i]
            }
        if(GammaDblPrime[i-1]>0)
            {
            Z_inn[,i]<- rbinom(nrow(Z),1,1-GammaDblPrime[i-1])*Z[,i]
            }
        Z_out[,i]<- (1-Z_inn[,i])*Z[,i]      
        
        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*f[i-1])
        app<- matrix(0,R,ncol(Z))
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)
        app[]<-0
        Z_inn<- rbind(Z_inn,app)
        }

        #colSums(Z)
        #colSums(Z_inn)
        #colSums(Z_out)
       #colSums(Z_inn)/colSums(Z)
        #colSums(Z_out)/colSums(Z)

        
    if(3==4)
        {
        ## SIMULATE MOVEMENT: markovian
        ## NOTE...... RANDOM DOES NOT DEPEND ON THE PREVIOUS
        ## LOCATION, I HAVE BEEN IMPLEMENTING MARKOVIAN BECAUSE
        ## GAMMAS DEPEND ON PREVIOUS LOCATION.
        occ<- rep(1:inputs$nprim,inputs$nsec)
        for(i in 1:inputs$nprim)
            {
            if(i==1)
                {
                Z_inn<-as.matrix(c(rep(1,inputs$n_inn),
                    rep(0,inputs$n-inputs$n_inn)),ncol=1)
                Z_out<- 1-Z_inn             
                }
            if(i>1)
                {
                ## MOVEMENT OUT OF STUDY AREA
                innToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_d_prime[i]*Z[,i]*Z_inn[,ncol(Z_inn)-1])
                innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,ncol(Z_inn)-1])
                ## REMAINING OUT OF STUDY AREA
                outToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_prime[i]*Z[,i]*Z_out[,ncol(Z_inn)])
                outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,ncol(Z_inn)])
                Z_inn<-cbind(Z_inn,innToInn+outToInn)
                Z_out<-cbind(Z_out,outToOut+innToOut)
                }
            for(j in 2:inputs$nsec[i])
                {
                ## MOVEMENT OUT OF STUDY AREA
                innToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_d_prime2[i]*Z[,i]*Z_inn[,ncol(Z_inn)])
                innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,ncol(Z_inn)])
                ## REMAINING OUT OF STUDY AREA
                outToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_prime2[i]*Z[,i]*Z_out[,ncol(Z_inn)])
                outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,ncol(Z_inn)])
                
                Z_inn<-cbind(Z_inn,innToInn+outToInn)
                Z_out<-cbind(Z_out,outToOut+innToOut)          
                }
            }
        }   

    ## SIMULATE CAPTURE HISTORIES
    ch<- matrix(0,nrow(Z),ncol(Z))
    p<- rep(inputs$p,inputs$nsec)
    for(i in 1:sum(inputs$nsec))
        {
        ch[,i]<-rbinom(nrow(Z),1,p[i]*Z_inn[,i])
        }
       
    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]
    ## NEED THIS FOR RMARK
    # prep data for processing
    ch1<- data.frame(ch=apply(ch,1,paste0,collapse=""),
        freq=1,stringsAsFactors=FALSE)
    ch_array<- 
    out<-(list(ch=ch1,
        ch_mat=ch,
        occs=occs,
        nprim=inputs$nprim,
        nsec=inputs$nsec,
        Z=Z,
        trueN=colSums(Z_inn[,c(which(occs==1),length(occs))]),
        Z_inn=Z_inn,
        Z_out=Z_out))
    return(out)
    }
    