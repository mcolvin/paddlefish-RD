
comm<- odbcConnectAccess2007("_dat/Paddlefish Database.accdb")

effort<- sqlQuery(comm,"SELECT [Paddlefish Effort Data].occasionId, [Paddlefish Effort Data].ID AS id, [Paddlefish Effort Data].Date AS [date], [Paddlefish Effort Data].set_number, [Paddlefish Effort Data].effort, [Paddlefish Effort Data].n_paddlefish
FROM [Paddlefish Effort Data];")    
meta<- sqlFetch(comm, "Transmitter Numbers")
meta$Implantation<-as.Date(meta$"Implantation Date")

## FORMAT AND CLEAN UP EFFORT DATA
effort$doy<-as.numeric(format(effort$date, "%j"))
effort$year<-as.numeric(format(effort$date, "%Y"))
effort$date<-as.Date(effort$date)

duration<- max(effort$date)-min(effort$date)

dates<-min(effort$date)+c(0:duration)

state_matrix<-matrix(1,nrow=length(dates),ncol=nrow(meta))





## ASSIGN VALUES PRE IMPLANTATION AS -99
rowId<-1
state_matrix[dates<meta[1,]$Implantation,1]<- -99 ## columns and value
head(state_matrix)



for(i in 1:nrow(meta))
    {
    state_matrix[dates<meta[i,]$Implantation,i]<- -99 ## columns and value
   # state_matrix[dates>meta[i,]$Failure,i]<- 4 ## columns and value
    }