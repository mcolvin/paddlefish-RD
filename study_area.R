
setwd("C:/Users/mcolvin/Documents/projects/Paddlefish/") # you will need to modify this here...
setwd("...../Paddlefish/") 

# here down should work if you have the working directory right

# packages
library(sp)
library(rgdal)
library(maps)
library(maptools)



nox_nwr<- readOGR("./gis-coverages", "NoxNWR")
lakes<- readOGR("./gis-coverages", "NoxNWR_lakes")
states<- readOGR("./gis-coverages", "States_MS_AL")
bigRiv<- readOGR("./gis-coverages", "NoxRiver_TenTom")
lilRiv<- readOGR("./gis-coverages", "Nox_River")



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