gmia_storms_app <- read.csv("M:/NonPoint Evaluation/gmia/R/glycol_applications_and_storm_loads.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_storms_app <- gmia_storms_app[,c(2:4)]
gmia_storms$Sample.Start.Date <- as.POSIXct(gmia_storms$Sample.Start.Date,format="%m/%d/%Y %H:%M")
gmia_storms$Sample.End.Date <- as.POSIXct(gmia_storms$Sample.End.Date,format="%m/%d/%Y %H:%M")

inst_disch <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall00060.rdb",asDateTime=TRUE)
inst_disch$datetime <- as.POSIXct(paste(inst_disch$DATE,inst_disch$TIME,sep=' '),format="%Y%m%d %H%M%S")

daily_disch <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall00060DV.rdb",asDateTime=TRUE)
daily_disch$datetime <- as.POSIXct(daily_disch$DATE,format="%Y%m%d")

daily_ice <- daily_disch[which(daily_disch$TYPE=="E"),]
inst_disch <- merge(daily_ice,inst_disch,by=c("DATE"),all.y=TRUE)
inst_disch <- inst_disch[which(is.na(inst_disch$TYPE)),]

dfq <- inst_disch[,c(12,17)]
colnames(dfq) <- c("Q","pdate")
dfq$Q <- as.numeric(dfq$Q)
gmia_storms <- gmia_storms[which(gmia_storms$Sample.End.Date<=max(dfq$pdate) & gmia_storms$Sample.Start.Date>=min(dfq$pdate)),]
df.dates <- gmia_storms[,c(2:3)]
colnames(df.dates) <- c("bpdate","epdate")

hydrovol_match <- findInterval(df.dates$bpdate-60*24*60,sort(dfq$pdate))
hydrovol_match_end <- findInterval(df.dates$epdate+60*24*60,sort(dfq$pdate))
norep <- length(hydrovol_match)
for (i in 1:norep) {
  beginpt <- ifelse(hydrovol_match[i]>hydrovol_match_end[i-1] || i==1,hydrovol_match[i],hydrovol_match_end[i-1]+1)
  dfQ_temp <- dfq[beginpt:hydrovol_match_end[i],]
  if (i==1) {dfQ <- dfQ_temp} else {dfQ <- rbind(dfQ,dfQ_temp) }
}