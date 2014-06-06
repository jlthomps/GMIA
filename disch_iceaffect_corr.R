
inst_disch <- getRDB1Data("C:/Users/jlthomps/Desktop/git/GMIA/outfalluv.rdb",asDateTime=TRUE)
#inst_disch <- getRDB1Data("/Users/jlthomps/GMIA/outfalluv.rdb",asDateTime=TRUE)

inst_disch$datetime <- as.POSIXct(inst_disch$DATETIME,format="%Y%m%d%H%M%S")
inst_disch$DATE <- substr(inst_disch$DATETIME,1,8)

daily_disch <- getRDB1Data("C:/Users/jlthomps/Desktop/git/GMIA/outfalldv.rdb",asDateTime=TRUE)
#daily_disch <- getRDB1Data("/Users/jlthomps/GMIA/outfall00060DV.rdb",asDateTime=TRUE)

daily_disch$datetime <- as.POSIXct(daily_disch$DATE,format="%Y%m%d")

daily_ice <- daily_disch[which(daily_disch$TYPE=="E"),]
inst_disch <- merge(daily_ice,inst_disch,by=c("DATE"),all.y=TRUE)
inst_disch$VALUE.y <- ifelse(is.na(inst_disch$TYPE),inst_disch$VALUE.y,inst_disch$VALUE.x)

dfq <- inst_disch[,c(12,17)]
colnames(dfq) <- c("Q","pdate")
dfq$Q <- as.numeric(dfq$Q)
gmia_storms <- read.csv("C:/Users/jlthomps/Desktop/git/GMIA/loading.csv",header=TRUE,stringsAsFactors=FALSE)
#gmia_storms <- read.csv("/Users/jlthomps/GMIA/loading.csv",header=TRUE,stringsAsFactors=FALSE)
#gmia_storms <- gmia_storms[which(gmia_storms$Site=="LK"),]
gmia_storms$Sample.Start.Date <- as.POSIXct(gmia_storms$Sample.Start.Date,format="%m/%d/%Y %H:%M")
gmia_storms$Sample.End.Date <- as.POSIXct(gmia_storms$Sample.End.Date,format="%m/%d/%Y %H:%M")
gmia_storms <- gmia_storms[which(gmia_storms$Sample.End.Date<=max(dfq$pdate) & gmia_storms$Sample.Start.Date>=min(dfq$pdate)),]
df.datesOUT <- gmia_storms[which(gmia_storms$Site=="OUT"),c(3:4)]
colnames(df.datesOUT) <- c("bpdate","epdate")
df.datesCG <- gmia_storms[which(gmia_storms$Site=="CG"),c(3:4)]
colnames(df.datesCG) <- c("bpdate","epdate")
df.datesLK <- gmia_storms[which(gmia_storms$Site=="LK"),c(3:4)]
colnames(df.datesLK) <- c("bpdate","epdate")
df.dates <- rbind(df.datesOUT,df.datesCG,df.datesLK)

hydrovol_match <- findInterval(df.datesOUT$bpdate-60*24*60,sort(dfq$pdate))
hydrovol_match_end <- findInterval(df.datesOUT$epdate+60*24*60,sort(dfq$pdate))
norep <- length(hydrovol_match)
for (i in 1:norep) {
  beginpt <- ifelse(hydrovol_match[i]>hydrovol_match_end[i-1] || i==1,hydrovol_match[i],hydrovol_match_end[i-1]+1)
  dfQ_temp <- dfq[beginpt:hydrovol_match_end[i],]
  if (i==1) {dfQOUT <- dfQ_temp} else {dfQOUT <- rbind(dfQOUT,dfQ_temp) }
}

hydrovol_match <- findInterval(df.datesCG$bpdate-60*24*60,sort(dfq$pdate))
hydrovol_match_end <- findInterval(df.datesCG$epdate+60*24*60,sort(dfq$pdate))
norep <- length(hydrovol_match)
for (i in 1:norep) {
  beginpt <- ifelse(hydrovol_match[i]>hydrovol_match_end[i-1] || i==1,hydrovol_match[i],hydrovol_match_end[i-1]+1)
  dfQ_temp <- dfq[beginpt:hydrovol_match_end[i],]
  if (i==1) {dfQCG <- dfQ_temp} else {dfQCG <- rbind(dfQCG,dfQ_temp) }
}

hydrovol_match <- findInterval(df.datesLK$bpdate-60*24*60,sort(dfq$pdate))
hydrovol_match_end <- findInterval(df.datesLK$epdate+60*24*60,sort(dfq$pdate))
norep <- length(hydrovol_match)
for (i in 1:norep) {
  beginpt <- ifelse(hydrovol_match[i]>hydrovol_match_end[i-1] || i==1,hydrovol_match[i],hydrovol_match_end[i-1]+1)
  dfQ_temp <- dfq[beginpt:hydrovol_match_end[i],]
  if (i==1) {dfQLK <- dfQ_temp} else {dfQLK <- rbind(dfQLK,dfQ_temp) }
}
