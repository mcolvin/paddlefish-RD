setwd("C:/Users/Chelsea/Desktop")
library(RODBC)

## database
com<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Chelsea/Desktop/Paddlefish Database.accdb")
      #odbcClose(com)


effort<- sqlQuery(com,"SELECT [Paddlefish Effort Data].occasionId, [Paddlefish Effort Data].ID AS id, [Paddlefish Effort Data].Date AS [date], [Paddlefish Effort Data].set_number, [Paddlefish Effort Data].effort, [Paddlefish Effort Data].n_paddlefish
                
                FROM [Paddlefish Effort Data];")   

meta<- sqlFetch(com, "Transmitter Numbers")
meta$Implantation<-as.Date(meta$"Implantation Date")

## FORMAT AND CLEAN UP EFFORT DATA

  effort$doy<-as.numeric(format(effort$date, "%j"))
  effort$year<-as.numeric(format(effort$date, "%Y"))
  effort$date<-as.Date(effort$date)
  
  duration<- max(effort$date)-min(effort$date)
  
  dates<-min(effort$date)+c(0:duration)
  
  state_matrix<-matrix(1,nrow=length(dates),ncol=nrow(meta))

  
  ## NAMING ROWS AND COLUMNS
  transmitter<-as.character(meta$Transmitter)
  colnames(state_matrix)<-transmitter #COLUMNS = FISH id
  
  dates<-as.character(dates)
  rownames(state_matrix)<-dates #ROWS = DATES
  
  
  ## ASSIGNING STATE 2 FOR MOVERS TO NOXUBEE
  
  #state_matrix[dates>="2017-04-05","A69-1303-11417"]<-2
  state_matrix[dates>="2017-03-11","A69-1303-11541"]<-2  
  state_matrix[dates>="2017-06-25","A69-1303-11542"]<-2 
  state_matrix[dates>="2017-04-05","A69-1303-11544"]<-2
  state_matrix[dates>="2017-04-06","A69-9001-20196"]<-2
  state_matrix[dates>="2017-04-06","A69-9001-20200"]<-2 #moved into Noxubee in 2016, coded  below, returned to the pool, and then left again in 2017 and hasn't been detected
  state_matrix[dates>="2017-04-06","A69-9001-20203"]<-2
  state_matrix[dates>="2017-03-15","A69-9001-20208"]<-2
  state_matrix[dates>="2018-03-16","A69-1303-11529"]<-2
  
  #weird fish that came back to the pool
  state_matrix[dates>="2017-03-15" & dates<="2017-07-15" ,"A69-1303-11413"]<-2
  state_matrix[dates>="2016-04-15" & dates<="2016-05-07" ,"A69-9001-20200"]<-2
  state_matrix[dates>="2017-04-05" & dates<="2017-05-20" ,"A69-1303-11417"]<-2  
  
  
  ## ASSIGNING STATE 3 TO TRANSLOCATION FISH
  state_matrix[dates>="2018-01-12","A69-1303-11415"]<-2 #1
  state_matrix[dates>="2018-01-12","A69-9001-20199"]<-2 #2
  state_matrix[dates>="2018-01-12","A69-1303-11538"]<-2 #3
  state_matrix[dates>="2018-01-13","A69-1303-11530"]<-2 #4
  state_matrix[dates>="2018-01-15","A69-9001-20202"]<-2 #5
  state_matrix[dates>="2018-01-15","A69-1303-11547"]<-2 #6
  state_matrix[dates>="2018-01-25" & dates<="2018-03-20","A69-1303-11531"]<-2 #7  #11531 was translocated upstream and returned to pool on 3/20/18
  state_matrix[dates>="2018-01-25","A69-1303-11545"]<-2 #8
  
  state_matrix["2018-01-12","A69-1303-11415"]<-3 #1
  state_matrix["2018-01-12","A69-9001-20199"]<-3 #2
  state_matrix["2018-01-12","A69-1303-11538"]<-3 #3
  state_matrix["2018-01-13","A69-1303-11530"]<-3 #4
  state_matrix["2018-01-15","A69-9001-20202"]<-3 #5
  state_matrix["2018-01-15","A69-1303-11547"]<-3 #6
  state_matrix["2018-01-25","A69-1303-11531"]<-3 #7
  state_matrix["2018-01-25","A69-1303-11545"]<-3 #8
  
  
## ASSIGN VALUES PRE IMPLANTATION AS -99

  rowId<-1
  state_matrix[dates<meta[1,]$Implantation,1]<- -99 ## columns and value
  head(state_matrix)

  for(i in 1:nrow(meta))
  {
  state_matrix[dates<meta[i,]$Implantation,i]<- -99 ## columns and value
  state_matrix[dates>meta[i,]$Failure_date,i]<- 4 ## columns and value
  }    
  

  

  
  

  
  