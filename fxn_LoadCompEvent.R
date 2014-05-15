#Computation of loadings for individual discrete samples

#Input: sample information including concentrations and instantateous dates and times for
#each sample, a time series for instantaneous flow 


LoadCompEvent <- function(df.samples,Conc,sample.time,Conc2liters,
                          df.Q,Q,Q.time,Q2liters,
                          df.events,event.bdate,event.edate,
                          volume="volume",Qmax="Qmax",duration="duration"){
  source("fxn_Hydrovol.R")
  

  numEvents <- dim(df.events)[1]
  event.loads <- numeric()
  
  for (j in 1:numEvents){
    # j<-1
    event.begin <- df.events[j,event.bdate]
    event.end <- df.events[j,event.edate]
    sub.df.samples <- subset(df.samples,df.samples[,sample.time]>event.begin & 
                               df.samples[,sample.time]<event.end)
    sub.df.samples$event <- j
    n <- dim(sub.df.samples)[1]
    for (i in 1:n){
      
      #Determine begin and end dates and times for individual samples
      #
      if (i == 1){
        
        #begin time for first sample is beginning of event from events file
        bpdate <- event.begin
        epdate <- mean(sub.df.samples[,sample.time][i:(i+1)])
      }else{bpdate <- c(bpdate,mean(sub.df.samples[,sample.time][(i-1):i]))
      }
      if(i==n){
        #estimate end time for last sample to be 1/2 of the time between the
        #last sample and the penultimate sample added to the last sample
        epdate <- c(epdate,event.end)
      }else{
        if(i!=1){
          epdate <- c(epdate,mean(sub.df.samples[,sample.time][i:(i+1)]))
        }
      }
    }
    
    sub.df.samples <- cbind(sub.df.samples,bpdate)
    sub.df.samples <- cbind(sub.df.samples,epdate)
    sub.df.samples <- Hydrovol(dfQ=df.Q,Q=Q,time=Q.time,
                           df.dates=sub.df.samples,bdate="bpdate",edate="epdate",
                           volume="volume",Qmax="Qmax",duration="duration")
    if(j==1) {df.samples.hydro <- sub.df.samples
              }else df.samples.hydro <- rbind(df.samples.hydro,sub.df.samples)
  }

    #NEED TO SORT OUT ADDING OUTPUT FROM THE ROUTINE ABOVE TO THE RETURN DATA FRAME
    
    
  df.samples.hydro$loads <- (df.samples.hydro[,Conc] * Conc2liters) * (df.samples.hydro$volume * Q2liters)
  event.loads <- aggregate(loads~event,data=df.samples.hydro,sum)[2]
  event.volumes <- aggregate(volume~event,data=df.samples.hydro,sum)[2]
  mean.conc <- event.loads/(event.volumes*Q2liters)/Conc2liters
  
  names(event.loads) <- paste("Load_",Conc,sep="")
  names(mean.conc) <- paste("FW_Mean_",Conc,sep="")
  
  df.load <- cbind(df.events,data.frame(load=event.loads,concentration=mean.conc))
  return(df.load)
}

