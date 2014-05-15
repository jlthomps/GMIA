#Computation of loadings for individual discrete samples

#Input: sample information including concentrations and instantateous dates and times for
#each sample, a time series for instantaneous flow 


LoadInstantaneous <- function(df.samples,Conc,sample.time,Conc2liters,df.Q,Q,Q.time,Q2liters){
  source("fxn_Hydrovol.R")
  
  n <- dim(df.samples)[1]
  
  
  for (i in 1:n){
    
    #Determine begin and end dates and times for individual samples
    #
    if (i == 1){
      #estimate begin time for first sample to be 1/2 of the time between the
      #1st and second samples, subtracted from the 1st sample time
      bpdate <- df.samples[i,sample.time] - difftime(df.samples[(i+1),sample.time],df.samples[i,sample.time],units="secs")/2
      epdate <- mean(df.samples[,sample.time][i:(i+1)])
    }else{bpdate <- c(bpdate,mean(df.samples[,sample.time][(i-1):i]))
    }
    if(i==n){
      #estimate end time for last sample to be 1/2 of the time between the
      #last sample and the penultimate sample added to the last sample
      epdate <- c(epdate,df.samples[i,sample.time] + difftime(df.samples[,sample.time][i],df.samples[,sample.time][i-1],units="secs")/2)
    }else{
      if(i!=1){
      epdate <- c(epdate,mean(df.samples[,sample.time][i:(i+1)]))
      }
    }
  }

df.samples <- cbind(df.samples,bpdate)
df.samples <- cbind(df.samples,epdate)
df.samples <- Hydrovol(dfQ=df.Q,Q=Q,time=Q.time,df.dates=df.samples,bdate="bpdate",edate="epdate",volume="volume",Qmax="Qmax",duration="duration")
loads <- (df.samples[,Conc] * Conc2liters) * (df.samples$volume * Q2liters)

df.samples <- cbind(df.samples,loads)
return(df.samples)
}


# Test function

#df.samples<-df.samples,
Conc<-"Conc"
sample.time<-"pdate"
Conc2liters<-10
#df.Q<-df.Q
Q<-"VALUE"
Q.time<-"pdate"
Q2liters<-28.3168




df.samples <- read.csv("sample.dates.test.csv")
df.samples$pdate <- as.POSIXct(df.samples$date,format="%m/%d/%Y %H:%M")

df.Q <- read.delim("Fortran_example_Q-12-OUT.RDB")
df.Q$pdate <- as.POSIXct(paste(df.Q$YEAR,df.Q$MONTH,df.Q$DAY,sep="-")) + df.Q$MINUTE*60


test <- LoadInstantaneous(df.samples=df.samples,Conc="Conc",sample.time="pdate",Conc2liters=10,df.Q=df.Q,Q="VALUE",Q.time="pdate",Q2liters=28.3168)

