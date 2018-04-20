



load("_output/ests.Rdata")
plot(N[tt],out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat[tt],out$BUGSoutput$mean$N);abline(0,1)
plot(out$BUGSoutput$mean$N_lat,type='l',ylim=c(0,150))
points(N,type='l',col="red");abline(v=tt)


## POLYGON PLOT 
upr<-out$BUGSoutput$summary
upr<-upr[grep("N_lat",rownames(upr)),c(1,2,3,7)]
estdat<- as.Date("2016-02-10")+dat$X$day
plot(estdat,out$BUGSoutput$mean$N_lat,type='n',ylim=c(0,80))
y<-c(upr[,3],rev(upr[,4]))
x<-c(estdat,rev(estdat))
polygon(x,y,col="lightgrey",border='lightgrey')
points(estdat,upr[,1],type='l',ylim=c(0,150),lwd=3)

points(as.Date("2016-02-10")+ipmdat$tt,Nhat[,1],pch=19)

par(new = T)
plot(estdat,ipmdat$X[,3],type='l',col='red')

## N estimates
Nhat<-out$BUGSoutput$summary
Nhat<-Nhat[grep("N\\[",rownames(Nhat)),c(1,2,3,7)]
plot(ipmdat$tt,Nhat[,1],ylim=c(0,120),pch=19)
segments(ipmdat$tt,Nhat[,3],ipmdat$tt,Nhat[,4])




upr<-out$BUGSoutput$summary
coeffs<-upr[grep("b",rownames(upr)),c(1,2,3,7)]
coeffs<-rbind(coeffs,upr[grep("a",rownames(upr)),c(1,2,3,7)])
coeffs
