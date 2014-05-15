
# Hydrovol
#
# Computes volumes and max discharge for hydrographs given the discharge time series and 
# the begin and end dates and times of the hydrographs
#

# dfQ<- dfQ# data frame with Q and time
# Q<-"Q"# name of column in dfQ with Q
# time<-"pdate"# name of column in dfQ with POSIXct time
# df.dates <- dfFortran# data frame with begin and end dates/times in POSIXct format
# bdate<-"bpdate"# begin date in POSIXct
# edate<-"epdate"# end date in POSIXct
# volume<-"event.vol"# name of resulting volume variable
# Qmax<-"Qmax"# name of Qmax variable
# duration<-"Eduration"# name of resulting duration variable

Hydrovol <- function(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration"){
  
  # Compute volumes and max for each hydrograph defined in the df.dates dataframe
  event.vol <- numeric()
  event.max <- numeric()
  for (i in 1:nrow(df.dates)){
    
    #Determine rows with range of times from last time step before hydrograph to one time step
    #after hydrograph ends and subset that time series
    begin.row <- max(which(dfQ[,time]<=df.dates[i,bdate]))
    end.row <- min(which(dfQ[,time]>=df.dates[i,edate]))
    subdfQ <- dfQ[begin.row:end.row,]
    
    sub.rows <- nrow(subdfQ)
    if(sub.rows<3) {
      event.vol[i] <- NA
      event.max[i] <- NA
      next
    }
    
    #Estimate begining Q
    if(subdfQ[1,time] != df.dates[i,bdate]){
      Q1 <- subdfQ[1,Q]
      Q2 <- subdfQ[2,Q]
      time1 <- subdfQ[1,time]
      time2 <- subdfQ[2,time]
      stime <- df.dates[i,bdate]
      qest <- (Q2-Q1)*(as.numeric(difftime(stime,time1)))/(as.numeric(difftime(time2,time1)))+Q1
      
      subdfQ[1,Q] <- qest
      subdfQ[1,time] <- df.dates[i,bdate]
    }
    
    #Estimate ending Q
    if(subdfQ[sub.rows,time] != df.dates[i,edate]){
      Q1 <- subdfQ[(sub.rows-1),Q]
      Q2 <- subdfQ[sub.rows,Q]
      time1 <- subdfQ[(sub.rows-1),time]
      time2 <- subdfQ[sub.rows,time]
      stime <- df.dates[i,edate]
      qest <- (Q2-Q1)*(as.numeric(difftime(stime,time1)))/(as.numeric(difftime(time2,time1)))+Q1
      
      subdfQ[sub.rows,Q] <- qest
      subdfQ[sub.rows,time] <- df.dates[i,edate]
    }
    
    #sum individual volumes
    Volumes <- numeric()
    for (j in 2:sub.rows){
      meanQ <- mean(subdfQ[c(j-1,j),Q])
      timeGapSecs <- difftime(subdfQ[j,time],subdfQ[(j-1),time],units="secs")
      Volumes[j] <- meanQ*timeGapSecs
    }
    
    
    event.vol[i] <- sum(Volumes,na.rm=T)
    event.max[i] <- max(subdfQ[,Q])
  }
  
  Eduration <- as.numeric(difftime(df.dates[,edate],df.dates[,bdate],units="hours"))
  df.dates2 <- cbind(df.dates,data.frame(event.vol=event.vol,Qmax=event.max,duration=Eduration))
  colnames(df.dates2) <- c(names(df.dates),volume,Qmax,duration)
  return(df.dates2)
}


