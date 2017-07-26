figures<- function(n){

if(n==1)
    {
    poolReciever_ours<-subset(poolReciever, !(is.na(ID )))
    xx<- dcast(poolReciever_ours, ID+date~"detect", value.var="ID",length)
    plot(ID~date,xx,type='n',pch=20,cex=0.9,yaxt='n',ylab="",xlab="")
    axis(2, at=c(1:50),labels=substr(tags$transmitter,10,14),las=1,cex.axis=0.7)
    ids<- unique(xx$ID)
    for(i in 1:length(ids))
        {
        points(ID~date, xx, subset=ID==ids[i],col=i,type='b',pch=20,cex=0.9,lwd=1.25)
        }
    }
if(n==2)
    {
    yy<-lapply(1:length(outp),function(xx)
        {

        if(!(is.null(outp[[xx]]$BUGSoutput$mean$N)))
            {
            out<- data.frame(
                id=xx,
                N=outp[[xx]]$BUGSoutput$mean$N,
                LCI=outp[[xx]]$BUGSoutput$summary[1,c('2.5%')],
                UCI=outp[[xx]]$BUGSoutput$summary[1,c('97.5%')],
                year=outp[[xx]]$meta$year,
                doy=outp[[xx]]$meta$doy,
                nsets=outp[[xx]]$meta$nsets)
            }
        })
    yy<- do.call(rbind, yy)	
    yy$year<- yy$year+yy$doy/365
    plot(N~year,yy,ylim=c(0,300),las=1,
        ylab="Estimated abundance",
        xlab="Year",pch=19)
    segments(x0=yy$year,x1=yy$year, y0=yy$LCI , y1=yy$UCI)
    }

}