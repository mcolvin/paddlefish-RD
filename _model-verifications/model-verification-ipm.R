

Z<- matrix(0,130,100)
omega<- 0.1
gprime<-0.3
g2prime<- 0.3
S<- 0.95
p<- 0.3

Z[,1]<- 1
ZZ<-Z
ZZ[,1]<-rbinom(nrow(Z),1,0.4) ## 1 = onsite



for(i in 2:100)
    {
    N<- c(N,N[i-1]+rpois(1,g*N[i-1])-rpois(1,l*N[i-1]))
    
    }
    