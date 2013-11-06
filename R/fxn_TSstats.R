
# Compute various stats for time series data over a period of time
# Originally scripted for NOAA Great Lakes model from GDP for given set of dates and 
# time periods, but could be used for any time series. File format must include the 
# POSIX formatted date (yyyy-mm-ddThh:mm:ssZ), and then columns of values with the 
# time series data
#
#df = dataframe with unit values values and date/time in POSIX
#date = name of POSIX date column 
#varnames=column with unit values in df
#dates = dataframe with sample dates
#starttime = "psdate"       # Column in sample dates data fram with dates in POSIX format
                            # used for extracting summary data from dates dataframe
#times = c(0.5,1,2,3,5,10)  # durations to use in computation of specified stats
#units = "hours"            # units of hours variable. Can be any of the following:
                            # "minutes","min","mins","hours","hr","hrs","day","days","week","weeks"
                            # default is hours
#stats.return=c("mean","max","min","median","sum")  
                            # specification of stats to apply
                            # to the time series data. Current
                            # options include mean, max, min, 
                            # median, sum, difference, nearest, and nearprev
                            # difference is the latest minus the first value
                            # nearest is the closest value in time to starttime
                            # nearprev is the closest value previous to starttime
                            # nearest and nearprev require a 0 in the times vector

#Usage: 
#TSstats(df="GDPmean",date="date",varname="unitvaluename",
#        dates="dates file",starttime="date",
#        times=c(0.5,1,2,3,5,10),units="hrs",
#        stats.return=c("mean"))


#read date with format mm/dd/yy hh:mm (use %Y if the format includes YYYY instead of yy)
#koepkeSM$date <- strptime(koepkeSM$Date,"%m/%d/%y %H:%M")

#read date with format mm/dd/yyyy hh:mm
#cedardates$psdate <- strptime(cedardates$Startdate,"%m/%d/%Y %H:%M")
#cedardates$parfdate <- strptime(cedardates$Enddate,"%m/%d/%Y %H:%M")

#Subset the data by begin and end date (can also assign to a df if you like)
#then define min mean median and max for the subset. Do this for all date periods
#in the file.

TSstats <- function(df,                     #Unit values file
                    date="date",            #Date column in POSIX format in unit values file
                    varnames,                #Column name with unit values
                    dates,                  #File with sample dates
                    starttime="psdate",     #Column in sample dates file with dates in POSIX format
                    times=c(1,2),           #Vector to define desired processing times
                                            #Zero indicates then nearest or nearest previous value
                                            #Default is hours, but can be specified
                                            #using "units" variable
                    units="hours",          #Units of times vector. Can be any of the following:
                                            #"minutes","min","mins","hours","hr","hrs","day","days","week","weeks"
                    stats.return=c("mean")) #Options include "mean","max","min",
                                            #"median","sum","sd","maxdiff","difference",
                                            #"nearest","nearprev"
                                            #maxdiff is the maximum value minus the minimum value for the time period
                                            #difference is the latest minus the first value
                                            #nearest is the closest value in time
                                            #nearprev is the closest value previous to the specified time
                                            #nearest and nearprev require a 0 in the times vector
                    {


#Initialize Statistical processes 
stats.names <- c("mean","max","min","median","sum","sd","maxdiff","difference","nearest","nearprev")
stats.get <- data.frame(row.names=1)
stats.get[,stats.names[1:length(stats.names)]] <- FALSE
nstats <- length(stats.return)
stats.get[,stats.return[1:nstats]] <- TRUE
stats.return <- names(stats.get[which(stats.get[1,]==TRUE)])

#Convert times to hours
unit.options <- c("minutes","min","mins","hours","hr","hrs","day","days","week","weeks")
hour.conversions <- c(rep(1/60,3),rep(1,3),24,24,rep(24*7,2))
timeHrs <- times*hour.conversions[unit.options==units]

#initialize varsum vector
maxrows <- nrow(dates)
varstats=data.frame(row.names=1:maxrows)
resultname=vector(mode="character")

varcols <- which(names(df)==varnames)

# Compute stats for all identified variables (columns)
for(k in 1:length(varcols)){
  varname <- names(df)[varcols[k]]

# compute the  stats for all identified durations
for(j in 1:length(timeHrs)) {      
dates$parfdate <- dates[,starttime] - timeHrs[j]*60*60

# Compute antecedent stats for specified periods for each date in the sample dates file
for (i in 1:maxrows){

  if(timeHrs[j]>0){
  subdata <- df[which(df[,"date"]>= dates[i,"parfdate"]
  & df[,"date"] < dates[i,starttime]),]

  if(stats.get[,"mean"]) varstats[i,"mean"] <- mean(subdata[,varname],na.rm=T)
  if(stats.get[,"max"]) varstats[i,"max"] <- max(subdata[,varname],na.rm=T)
  if(stats.get[,"min"]) varstats[i,"min"] <- min(subdata[,varname],na.rm=T)
  if(stats.get[,"median"]) varstats[i,"median"] <- median(subdata[,varname],na.rm=T)
  if(stats.get[,"sum"]) varstats[i,"sum"] <- sum(subdata[,varname],na.rm=T)
  if(stats.get[,"sd"]) varstats[i,"sd"] <- sd(subdata[,varname],na.rm=T)
  if(stats.get[,"maxdiff"]) varstats[i,"maxdiff"] <- max(subdata[,varname],na.rm=T)-min(subdata[,varname],na.rm=T)
  if(stats.get[,"difference"]) varstats[i,"difference"] <- subdata[nrow(subdata),varname] - subdata[1,varname]
  }
  if(timeHrs[j]==0){
    if(stats.get[,"nearprev"]) {
      subdata <- df[which(df[,"date"] < dates[i,starttime]),]
      varstats[i,"nearprev"] <- (subdata[nrow(subdata),varname])
    }
    
    if(stats.get[,"nearest"]){
      time.diffs <- as.numeric(difftime(dates[i,starttime],df[,starttime]))
      nearest.data <- which(abs(time.diffs)==min(abs(time.diffs)))[1]
      varstats[i,"nearest"] <- df[nearest.data,varname]
    }
  }

}

if(timeHrs[j]==0){
  statsnames <- which((stats.return[1:nstats]== "nearprev") 
                      | (stats.return[1:nstats] == "nearest"))
  stats.return[statsnames]
} else{
  statsnames <- which((stats.return[1:nstats]!= "nearprev") 
                      & (stats.return[1:nstats] != "nearest"))
}
rm(resultname)
if(length(statsnames)>0) {
if(times[j]>0){
 resultname <- paste(varname,"_",stats.return[statsnames],times[j],sep="")
}else resultname <- paste(varname,"_",stats.return[statsnames],sep="")

names(varstats) <- resultname
dates <- cbind(dates,varstats)
varstats <- varstats[,-(1:length(varstats))]
}
}
}
return(dates)
}