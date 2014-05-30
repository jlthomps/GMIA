
#gmia_application <- read.csv("C:/Users/jlthomps/Desktop/git/GMIA/report.event.table.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_application <- read.csv("/Users/jlthomps/GMIA/glycol_applications_and_storm_loads.csv",header=TRUE,stringsAsFactors=FALSE)

gmia_application$Event.Start.Date <- as.POSIXct(gmia_application$Start.Date,format="%m/%d/%Y")
gmia_application$Event.End.Date <- as.POSIXct(gmia_application$End.Date,format="%m/%d/%Y")
gmia_application <- gmia_application[,c(1,4:7,14:15,35:36)]
for (i in 1:nrow(gmia_application)) {
  if (length(unlist(strsplit(gmia_application$Duration..hours.[i],":")))==3) {
    gmia_application$duration[i] <- as.numeric(unlist(strsplit(gmia_application$Duration..hours.[i],":"))[1])+(as.numeric(unlist(strsplit(gmia_application$Duration..hours.[i],":"))[2])/60)+(as.numeric(unlist(strsplit(gmia_application$Duration..hours.[i],":"))[3])/3600)
  } else if (length(unlist(strsplit(gmia_application$Duration..hours.[i],":")))==2) {
    gmia_application$duration[i] <- as.numeric(unlist(strsplit(gmia_application$Duration..hours.[i],":"))[1])+(as.numeric(unlist(strsplit(gmia_application$Duration..hours.[i],":"))[2])/60)
  } else { gmia_application$duration[i] <- as.numeric(gmia_application$Duration..hours.[i])}
}
colnames(gmia_application) <- c("StormId","durationHours","precipDesc","OUTglycolGal","OUTglycolKg","CGglycolGal","CGglycolKg","Event.Start.Date","Event.End.Date","duration")

library(GSHydroTools)
hydrovol_data <- Hydrovol(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
hydrovol_data$vol.liters <- hydrovol_data$event.vol*28.31685
hydrovol_data$stormnum <- gmia_storms$Storm.ID

#storm_qwdata <- read.delim("C:/Users/jlthomps/Desktop/git/GMIA/outfallresult.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,comment.char="#",colClasses=rep("character",16))
storm_qwdata <- read.delim("/Users/jlthomps/GMIA/outfallresult.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,comment.char="#",colClasses=rep("character",16))

storm_qwdata <- storm_qwdata[which(nchar(storm_qwdata$EDATE)>0),]
storm_qwdata$SAMPLE_START_DT <- as.POSIXct(paste(storm_qwdata$BDATE,storm_qwdata$BTIME,sep=" "),format="%Y%d%m %H%M")
storm_qwdata$SAMPLE_END_DT <- as.POSIXct(paste(storm_qwdata$EDATE,storm_qwdata$ETIME,sep=" "),format="%Y%d%m %H%M")
storm_qwdata$RECORD_NO <- as.numeric(storm_qwdata$SAMPL)
storm_qwdata$StormId <- storm_qwdata$SCMFL

#####################OPTIONAL
# qw_samples <- unique(storm_qwdata[,c("RECORD_NO","SAMPLE_START_DT","SAMPLE_END_DT")])
# gmia_storms$record_no <- gmia_storms$Site
# gmia_storms$record_no1 <- gmia_storms$Site
# gmia_storms$record_no2 <- gmia_storms$Site
# gmia_storms$record_no3 <- gmia_storms$Site
# 
# for (i in 1:nrow(gmia_storms)) {
#   begin.date <- floor_date(gmia_storms$Sample.Start.Date[i],unit="day")
#   end.date <- floor_date(gmia_storms$Sample.End.Date[i],unit="day")
#   qwSub <- qw_samples[which(floor_date(qw_samples$SAMPLE_START_DT,unit="day")==begin.date),]
#   qwSub1 <- qw_samples[which(floor_date(qw_samples$SAMPLE_START_DT,unit="day")==end.date),]
#   qwSub2 <- qw_samples[which(floor_date(qw_samples$SAMPLE_END_DT,unit="day")==begin.date),]
#   qwSub3 <- qw_samples[which(floor_date(qw_samples$SAMPLE_END_DT,unit="day")==end.date),]
#   if (nrow(qwSub)==1) {
#     gmia_storms$record_no[i] <- qwSub$RECORD_NO
#   } else {gmia_storms$record_no[i] <- NA}
#   if (nrow(qwSub1)==1) {
#     gmia_storms$record_no1[i] <- qwSub1$RECORD_NO
#   } else {gmia_storms$record_no1[i] <- NA}
#   if (nrow(qwSub2)==1) {
#     gmia_storms$record_no2[i] <- qwSub2$RECORD_NO
#   } else {gmia_storms$record_no2[i] <- NA}
#   if (nrow(qwSub3)==1) {
#     gmia_storms$record_no3[i] <- qwSub3$RECORD_NO
#   } else {gmia_storms$record_no3[i] <- NA}
# }
# gmia_storms$record_no <- if (floor_date(storm_qwdata$SAMPLE_START_DT)==floor_date(gmia_storms$Sample.Start.Date)) storm_qwdata$RECORD_NO
# ###################################

gmia_storms$StormId <- paste(gmia_storms$Site,gmia_storms$Storm.ID,sep="-")
storm_qwdata <- merge(storm_qwdata,gmia_storms,by="StormId")

data_merge <- merge(hydrovol_data,storm_qwdata[which(storm_qwdata$PCODE=='00310'),c("StormId","VALUE","REMRK","Storm.ID")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","StormId","BODconc","BODrmk")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PCODE=='00335'),c("Storm.ID","VALUE","REMRK")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","StormId","BODconc","BODrmk","CODconc","CODrmk")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PCODE=='91080'),c("Storm.ID","VALUE","REMRK")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","StormId","BODconc","BODrmk","CODconc","CODrmk","PGconc","PGrmk")
data_merge <- merge(data_merge,storm_qwdata[which(storm_qwdata$PCODE=='91075'),c("Storm.ID","VALUE","REMRK")],by.x=c("stormnum"),by.y=c("Storm.ID"),all.x=TRUE)
colnames(data_merge) <- c("stormnum","bpdate","epdate","event.vol","Qmax","Eduration","vol.liters","StormId","BODconc","BODrmk","CODconc","CODrmk","PGconc","PGrmk","EGconc","EGrmk")
data_merge$BODload <- data_merge$vol.liters*as.numeric(data_merge$BODconc)/1000000
data_merge$CODload <- data_merge$vol.liters*as.numeric(data_merge$CODconc)/1000000
data_merge$PGload <- data_merge$vol.liters*as.numeric(data_merge$PGconc)/1000000
data_merge$EGload <- data_merge$vol.liters*as.numeric(data_merge$EGconc)/1000000
#data_merge$EGPGload <- data_merge$EGload+data_merge$PGload
for (i in 1:nrow(data_merge)) {
  data_merge$snow[i] <- as.numeric(daily_summ$SNDP[which(daily_summ$YEARMODA==as.numeric(strftime(round(data_merge$epdate[i],units="days"),format="%Y%m%d")))])-as.numeric(daily_summ$SNDP[which(daily_summ$YEARMODA==as.numeric(strftime(round(data_merge$bpdate[i],units="days"),format="%Y%m%d")))]) 
}

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
#storms_merge <- merge(hourly_match,hourly_match_end,by=c("stormnum"))
storm_merge <- merge(data_merge,storms_merge,by.x=c("stormnum"),by.y=c("data_merge.stormnum"))

noreps <- nrow(storm_merge)
for (i in 1:noreps) {
  hourly_subT <- hourly_sub[which(hourly_sub$datetime<=storm_merge$epdate[i] & hourly_sub$datetime>=storm_merge$bpdate[i]),]
  storm_merge$mean_temp[i] <- mean(as.numeric(hourly_subT$temp),na.rm=TRUE)
  storm_merge$max_temp[i] <- max(as.numeric(hourly_subT$temp),na.rm=TRUE)
  storm_merge$min_temp[i] <- min(as.numeric(hourly_subT$temp),na.rm=TRUE)
  storm_merge$prcp_sum[i] <- sum(as.numeric(hourly_subT$precip1dpth),na.rm=TRUE)
  storm_merge$snowwteq_sum[i] <- sum(as.numeric(hourly_subT$snowwteq),na.rm=TRUE)
  storm_merge$snowacc_max[i] <- max(as.numeric(hourly_subT$snowacc1dpth))
}

data_merge <- merge(storm_merge,gmia_application[,c(1,3:7,10)],by.x=c("stormnum"),by.y=c("StormId"),all.x=TRUE)
data_merge <- data_merge[,c(1:3,5:7,10,12,14,16:21,24:35)]
colnames(data_merge) <- c("stormnum","bpdate","epdate","Qmax","Eduration","vol.liters","BODrmk","CODrmk","PGrmk","EGrmk","BODload","CODload","PGload","EGload","snow_depth","mean_temp","max_temp","min_temp","prcp_sum","snowwteq_sum","snowacc_max","prcp_desc","OUTgalGlycol","OUTkgGlycol","CGgalGlycol","CGkgGlycol","deice_Eduration")
data_sub <- data_merge
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)
