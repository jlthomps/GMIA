gmia_application <- read.csv("M:/NonPoint Evaluation/gmia/R/report.event.table.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_application$StartDate <- as.POSIXct(gmia_application$Event.Start.Date,format="%m/%d/%Y")
gmia_application$EndDate <- as.POSIXct(gmia_application$Event.End.Date,format="%m/%d/%Y")

source("M:/NonPoint Evaluation/gmia/R/fxn_Hydrovol.R")
hydrovol_data <- Hydrovol(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
hydrovol_data$vol.liters <- hydrovol_data$event.vol*28.31685
hydrovol_data$stormnum <- gmia_storms$Storm.ID

storm_qwdata <- read.delim("M:/NonPoint Evaluation/gmia/R/outfallQW.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
storm_qwdata <- storm_qwdata[which(nchar(storm_qwdata$SAMPLE_END_DT)>0),]
storm_qwdata$SAMPLE_START_DT <- as.POSIXct(storm_qwdata$SAMPLE_START_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$SAMPLE_END_DT <- as.POSIXct(storm_qwdata$SAMPLE_END_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$RECORD_NO <- as.numeric(storm_qwdata$RECORD_NO)
storm_qwdata <- merge(storm_qwdata,gmia_storms,by.x=c("SAMPLE_START_DT"),by.y="Sample.Start.Date")

data_merge <- merge(hydrovol_data,storm_qwdata[which(storm_qwdata$PARM_CD==310),c("Storm.ID","RESULT_VA")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","BODconc")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PARM_CD==335),c("Storm.ID","RESULT_VA")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","BODconc","CODconc")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PARM_CD==91080),c("Storm.ID","RESULT_VA")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","BODconc","CODconc","PGconc")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PARM_CD==91075),c("Storm.ID","RESULT_VA")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","BODconc","CODconc","PGconc","EGconc")
data_merge$BODload <- data_merge$vol.liters*data_merge$BODconc/1000000
data_merge$CODload <- data_merge$vol.liters*data_merge$CODconc/1000000
data_merge$PGload <- data_merge$vol.liters*data_merge$PGconc/1000000
data_merge$EGload <- data_merge$vol.liters*data_merge$EGconc/1000000
data_merge$EGPGload <- data_merge$EGload+data_merge$PGload

daily_match <- mergeNearest(data_merge,dates.left="bpdate",right=daily_summ,dates.right="date",max.diff="36 hours")
daily_match <- daily_match[,c("stormnum","bpdate","date")]
daily_match_end <- mergeNearest(data_merge,dates.left="epdate",right=daily_summ,dates.right="date",max.diff="36 hours")
daily_match_end <- daily_match_end[,c("stormnum","epdate","date")]

storms_merge <- merge(daily_match,daily_match_end,by=c("stormnum"))
storms_merge <- merge(data_merge,storms_merge,by.x=c("stormnum"),by.y=c("stormnum"))
storms_merge$date.x <- as.POSIXct(storms_merge$date.x,format="%Y-%m-%d")
storms_merge$date.y <- as.POSIXct(storms_merge$date.y,format="%Y-%m-%d")
daily_summ$date <- as.POSIXct(daily_summ$date,format="%Y-%m-%d")

noreps <- nrow(storms_merge)
for (i in 1:noreps) {
  begin <- which(daily_summ$date==storms_merge$date.x[i],arr.ind=TRUE)
  end <- which(daily_summ$date==storms_merge$date.y[i],arr.ind=TRUE)
  storms_merge$mean_temp[i] <- mean(daily_summ$TEMP[begin:end],na.rm=TRUE)
  storms_merge$max_temp[i] <- max(daily_summ$TEMP[begin:end],na.rm=TRUE)
  storms_merge$min_temp[i] <- min(daily_summ$TEMP[begin:end],na.rm=TRUE)
  storms_merge$prcp_sum[i] <- sum(as.numeric(daily_summ$PRCP[begin:end]),na.rm=TRUE)
  storms_merge$dwpt_mean[i] <- mean(daily_summ$DEWP[begin:end],na.rm=TRUE)
  storms_merge$snowdp_sum[i] <- sum(as.numeric(daily_summ$SNDP[begin:end]),na.rm=TRUE)
}

data_merge <- merge(storms_merge,gmia_application[,c(1,3:7)],by.x=c("stormnum"),by.y=c("Event.number"),all.x=TRUE)
data_merge <- data_merge[,c(1:3,5:7,12:16,21:31)]
colnames(data_merge) <- c("stormnum","bpdate","epdate","Qmax","Eduration","vol.liters","BODload","CODload","PGload","EGload","EGPGload","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","deice_Eduration","mmprcp_deice","prcp_desc","kgGlycol","glycolOUT")
data_merge$deice_Eduration <- as.numeric(data_merge$deice_Eduration)
data_merge$mmprcp_deice <- as.numeric(data_merge$mmprcp_deice)
data_merge$kgGlycol <- as.numeric(gsub(",","",data_merge$kgGlycol))
data_merge$glycolOUT <- as.numeric(gsub(",","",data_merge$glycolOUT))
data_merge$decYear <- paste(strftime(data_merge$bpdate,"%Y"),".",as.POSIXlt(data_merge$bpdate)$yday+1,sep="")
data_merge$sinDY <- sin(as.numeric(data_merge$decYear)*2*pi)
data_merge$cosDY <- cos(as.numeric(data_merge$decYear)*2*pi)
data_sub <- data_merge
# use only those storms w/ application precip data
data_sub <- data_sub[which(!is.na(data_sub$mmprcp_deice)),]
# use only those storms that are not snowmelt
data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
# use only those storms that are snowmelt
data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]

data_sub$remark <- ""
data_sub$decYear <- as.numeric(data_sub$decYear)