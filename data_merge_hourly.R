gmia_storms <- read.csv("M:/NonPoint Evaluation/gmia/R/loading.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_storms <- gmia_storms[which(gmia_storms$Site=="OUT"),c(2,5:7)]
gmia_storms$Sample.Start.Date <- as.POSIXct(gmia_storms$Sample.Start.Date,format="%m/%d/%Y %H:%M")
gmia_storms$Sample.End.Date <- as.POSIXct(gmia_storms$Sample.End.Date,format="%m/%d/%Y %H:%M")

gmia_application <- read.csv("M:/NonPoint Evaluation/gmia/R/report.event.table.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_application$datetime <- as.POSIXct(gmia_application$Date,format="%m/%d/%Y")

source("M:/NonPoint Evaluation/gmia/R/fxn_Hydrovol.R")
hydrovol_data <- Hydrovol(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
hydrovol_data$vol.liters <- hydrovol_data$event.vol*28.31685
hydrovol_data$stormnum <- gmia_storms$Storm.ID

storm_qwdata <- read.delim("M:/NonPoint Evaluation/gmia/R/outfallQW.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
storm_qwdata <- storm_qwdata[which(nchar(storm_qwdata$SAMPLE_END_DT)>0),]
storm_qwdata$SAMPLE_START_DT <- as.POSIXct(storm_qwdata$SAMPLE_START_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$SAMPLE_END_DT <- as.POSIXct(storm_qwdata$SAMPLE_END_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$RECORD_NO <- as.numeric(storm_qwdata$RECORD_NO)
storm_qwdata <- merge(storm_qwdata,gmia_storms,by.x=c("RECORD_NO"),by.y="recordNo")

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

hourly_match <- mergeNearest(data_merge,dates.left="bpdate",right=hourly_sub,dates.right="datetime",max.diff="2 hours")
hourly_match <- hourly_match[,c("stormnum","bpdate","Date","Time")]
hourly_match_end <- mergeNearest(data_merge,dates.left="epdate",right=hourly_sub,dates.right="datetime",max.diff="2 hours")
hourly_match_end <- hourly_match_end[,c("stormnum","epdate","Date","Time")]
hourly_match$datetime <- as.POSIXct(paste(hourly_match$Date,formatC(hourly_match$Time,width=4,format="d",flag="0"),' '),format="%Y%m%d %H%M")
hourly_match_end$datetime <- as.POSIXct(paste(hourly_match_end$Date,formatC(hourly_match_end$Time,width=4,format="d",flag="0"),' '),format="%Y%m%d %H%M")
hourly_match <- findInterval(data_merge$bpdate,sort(hourly_sub$datetime))
hourly_match_df <- data.frame(hourly_match,data_merge$stormnum,stringsAsFactors=FALSE)
hourly_match_df_sub <- hourly_match_df[which(hourly_match>0),]
hourly_match_end <- findInterval(data_merge$epdate,sort(hourly_sub$datetime))
hourly_match_end_df <- data.frame(hourly_match_end,data_merge$stormnum,stringsAsFactors=FALSE)
hourly_match_end_df_sub <- hourly_match_end_df[which(hourly_match_end>0),]

storms_merge <- merge(hourly_match_df,hourly_match_end_df,by=c("data_merge.stormnum"))
storms_merge <- merge(hourly_match,hourly_match_end,by=c("stormnum"))
storm_merge <- merge(data_merge,storms_merge,by.x=c("stormnum"),by.y=c("stormnum"))

noreps <- nrow(storm_merge)
for (i in 1:noreps) {
  begin <- which(hourly_sub$datetime==storm_merge$datetime.x[i],arr.ind=TRUE)
  end <- which(hourly_sub$datetime==storm_merge$datetime.y[i],arr.ind=TRUE)
  storm_merge$mean_temp[i] <- mean(hourly_sub$temp[begin:end],na.rm=TRUE)
  storm_merge$max_temp[i] <- max(hourly_sub$temp[begin:end],na.rm=TRUE)
  storm_merge$min_temp[i] <- min(hourly_sub$temp[begin:end],na.rm=TRUE)
  storm_merge$prcp_sum[i] <- sum(as.numeric(hourly_sub$precip1dpth[begin:end]),na.rm=TRUE)
  storm_merge$snowwteq_sum[i] <- sum(as.numeric(hourly_sub$snowwteq[begin:end]),na.rm=TRUE)
  storm_merge$snowacc_max[i] <- max(as.numeric(hourly_sub$snowacc1dpth[begin:end]))
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
data_sub <- data_sub[which(!is.na(data_sub$mmprcp_deice)),]
data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$remark <- ""
data_sub$decYear <- as.numeric(data_sub$decYear)