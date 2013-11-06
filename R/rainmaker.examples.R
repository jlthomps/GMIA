RDB <- RDB_example2

RDB2 <- RMprep(RDB,prep.type=1,date.type=4,tz="CST6CDT")

RDB3 <- subset(RDB2,VALUE>-1)

event.list <- RMevents(df=RDB3,ieHr=6,rainthresh=0.2,rain="VALUE")

events.0.2 <- event.list[[1]]

getwd()
setwd("D:/SRCData/R/Rainmaker")

RMevents.plot(RDB3,date="pdate",rain="VALUE",df.events=events.0.2,sdate="StartDate","EndDate",depth= "rain",plot.buffer=2,site.name="Example Site For Troy")


intensities <- RMIntense(RDB3,date="pdate",rain="VALUE",events.0.2,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,15,30))

ARFrain <- RMarf(df=RDB3,date="pdate",rain="VALUE",df.events=intensities,sdate="StartDate",days=c(1,3,5),varnameout="ARF")

#RMeventsSamples is a script that defines rainfall for specific sampling periods.