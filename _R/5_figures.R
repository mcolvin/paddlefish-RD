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

    if(n==3)
        {
        nox_nwr<- readOGR("_dat/gis-coverages", "NoxNWR")
        lakes<- readOGR("_dat/gis-coverages", "NoxNWR_lakes")
        states<- readOGR("_dat/gis-coverages", "States_MS_AL")
        bigRiv<- readOGR("_dat/gis-coverages", "NoxRiver_TenTom")
        lilRiv<- readOGR("_dat/gis-coverages", "Nox_River")



        # BIG PICTURE
        plot(states,col="white", border="black",axes=TRUE)
        plot(bigRiv, col="blue", add=TRUE)
        plot(nox_nwr, col="red",border='red',add=TRUE)

        # FINE SCALE PICTURE
        # PLOT REFUGE POLYGON
        dev.new(width=7, height=7)
        plot(nox_nwr,col="grey",border="grey",
            axes=TRUE,ylab="Latitude",xlab="Longitude")
        # ADD BLUFF AND LOKAFOMA
        plot(lakes,add=TRUE,col='black')
        # ADD NOXUBEE RIVER
        plot(lilRiv,add=TRUE,col='black')

        # add a point
        points(-88.776647,33.290829,col="red",pch=19)
        # add another point
        points(-88.823760,33.301485,col="red",pch=19)


        map.scale(x=-88.83, y=33.4, ratio=FALSE, relwidth=0.2)
        arrows(x0=-88.7, y0=33.38 ,x1=-88.7 ,y1=33.4,length=0.15,lwd=2)
        text(-88.7,33.38,"N",pos=1)
        
        }
    if(n==5)
        {
                
        plot(TempC~dt,stageTemp,subset=Site_ID=="Oktoc Spillway",type='l')

        plot(SensorDepthM~dt,stageTemp,subset=Site_ID=="Oktoc Spillway",type='l',ylim=c(0,10))
        points(SensorDepthM~dt,stageTemp,subset=Site_ID=="Mid-Oktoc Stage",type='l')
        points(SensorDepthM~dt,stageTemp,subset=Site_ID=="Roberts Road",type='l',col='red')
        points(SensorDepthM~dt,stageTemp,subset=Site_ID=="Beer Bottle Tree",type='l',col='red')
        points(SensorDepthM~dt,stageTemp,subset=Site_ID=="Oktoc Spillway",type='l',col='red')

                
        }
}