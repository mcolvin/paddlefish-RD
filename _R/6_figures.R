figures<- function(n){
unique(xx$id)
if(n==1)
    {
    xx<- dcast(poolReciever_all, 
        transmitter+date~"detect", 
        value.var="transmitter",length)
    xx<- merge(xx,tags,by="transmitter",all.x=TRUE)
    xx<-xx[order(xx$transmitter,xx$date),]
    plot(tagNumber~date,xx,type='n',pch=20,cex=0.9,
        yaxt='n',ylab="",xlab="")
    ids<- unique(xx$tagNumber)        
    axis(2, at=c(1:49),labels=tags$ids,
        las=1,cex.axis=0.7)
    for(i in 1:length(ids))
        {
        points(tagNumber~date, xx, subset=tagNumber==ids[i],
            col=i,type='b',pch=20,cex=0.9,lwd=1.25)
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