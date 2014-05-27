library(dataRetrieval)
library(USGSwsBase)

daily_summ <- read.csv("C:/Users/jlthomps/Desktop/git/GMIA/global_daily_summary_CDO2400066445620.txt",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
#daily_summ <- read.csv("/Users/jlthomps/GMIA/global_daily_summary_CDO2400066445620.txt",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
daily_summ$FRSHTT <- sprintf("%06d",daily_summ$FRSHTT)
daily_summ$date <- strptime(daily_summ$YEARMODA,format="%Y%m%d")
daily_summ$max_flg <- ifelse(substr(daily_summ$MAX,nchar(daily_summ$MAX),nchar(daily_summ$MAX)+1)=='*','*','')
daily_summ$MAX <- ifelse(daily_summ$max_flg=='*',as.numeric(substr(daily_summ$MAX,1,nchar(daily_summ$MAX)-1)),as.numeric(daily_summ$MAX))
daily_summ$min_flg <- ifelse(substr(daily_summ$MIN,nchar(daily_summ$MIN),nchar(daily_summ$MIN)+1)=='*','*','')
daily_summ$MIN <- ifelse(daily_summ$min_flg=='*',as.numeric(substr(daily_summ$MIN,1,nchar(daily_summ$MIN)-1)),as.numeric(daily_summ$MIN))
daily_summ$precip_flg <- ifelse(!is.numeric(substr(daily_summ$PRCP,nchar(daily_summ$PRCP),nchar(daily_summ$PRCP)+1)),substr(daily_summ$PRCP,nchar(daily_summ$PRCP),nchar(daily_summ$PRCP)+1))
daily_summ$PRCP <- ifelse(!is.na(daily_summ$precip_flg),as.numeric(substr(daily_summ$PRCP,1,nchar(daily_summ$PRCP)-1)),as.numeric(daily_summ$PRCP))
daily_summ$TEMP <- ifelse(daily_summ$TEMP==9999.9,'',daily_summ$TEMP)
daily_summ$DEWP <- ifelse(daily_summ$DEWP==9999.9,'',daily_summ$DEWP)
daily_summ$SLP <- ifelse(daily_summ$SLP==9999.9,'',daily_summ$SLP)
daily_summ$STP <- ifelse(daily_summ$STP==9999.9,'',daily_summ$STP)
daily_summ$VISIB <- ifelse(daily_summ$VISIB==999.9,'',daily_summ$VISIB)
daily_summ$WDSP <- ifelse(daily_summ$WDSP==999.9,'',daily_summ$WDSP)
daily_summ$MXSPD <- ifelse(daily_summ$MXSPD==999.9,'',daily_summ$MXSPD)
daily_summ$GUST <- ifelse(daily_summ$GUST==999.9,'',daily_summ$GUST)
daily_summ$MAX <- ifelse(daily_summ$MAX==9999.9,'',daily_summ$MAX)
daily_summ$MIN <- ifelse(daily_summ$MIN==9999.9,'',daily_summ$MIN)
daily_summ$PRCP <- ifelse(daily_summ$PRCP==99.99,'',daily_summ$PRCP)
daily_summ$SNDP <- ifelse(daily_summ$SNDP==999.9,'',daily_summ$SNDP)
daily_summ$fog <- substr(daily_summ$FRSHTT,1,1)
daily_summ$rain <- substr(daily_summ$FRSHTT,2,2)
daily_summ$snow <- substr(daily_summ$FRSHTT,3,3)
daily_summ$hail <- substr(daily_summ$FRSHTT,4,4)
daily_summ$thunder <- substr(daily_summ$FRSHTT,5,5)
daily_summ$tornado <- substr(daily_summ$FRSHTT,6,6)

hourly_data <- read.delim("C:/Users/jlthomps/Desktop/git/GMIA/hourly_global.txt",header=FALSE,stringsAsFactors=FALSE,skip=2,strip.white=TRUE,comment.char="",sep=",")
#hourly_data <- read.delim("/Users/jlthomps/GMIA/hourly_global.txt",header=FALSE,stringsAsFactors=FALSE,skip=2,strip.white=TRUE,comment.char="",sep=",")

hourly_sub <- hourly_data[,c(1:8,22:23,24:25,26:27,28:49,130:135,136:151,162:189)]
colnames(hourly_sub) <- c("site_name","USAF","NCDC","Date","Time","I","Type","QCP","temp","tempQ","dewpt","dewptq","atmpr","atmprq",
                          "precip1hr","precip1dpth","precip1cond","precip1q","precip2hr","precip2dpth","precip2cond","precip2q",
                          "precip3hr","precip3dpth","precip3cond","precip3q","precip4hr","precip4dpth","precip4cond","precip4q",
                          "prcpmnthdpth","prcpmnthcond","precipmnthq","prcphstdur","prcphstchar","prcphstq","snowdpth","snowdpthcond","snowdpthq","snowwteq",
                          "snowwteqcond","snowwteqq","snowacc1hrs","snowacc1dpth","snowacc1cond","snowacc1q","snowacc2hrs","snowacc2dpth","snowacc2cond","snowacc2q",
                          "snowacc3hrs","snowacc3dpth","snowacc3cond","snowacc3q","snowacc4hrs","snowacc4dpth","snowacc4cond","snowacc4q",
                          "prcpmin1min","prcpmin1dpth","prcpmin1cond","prcpmin1q","prcpmin2min","prcpmin2dpth","prcpmin2cond","prcpmin2q",
                          "prcpmin3min","prcpmin3dpth","prcpmin3cond","prcpmin3q","prcpmin4min","prcpmin4dpth","prcpmin4cond","prcpmin4q",
                          "prcp15min1mm","prcp15min1cond","prcp15min1q","prcp15min2mm","prcp15min2cond","prcp15min2q",
                          "prcp15min3mm","prcp15min3cond","prcp15min3q","prcp15min4mm","prcp15min4cond","prcp15min4q")
hourly_sub$temp <- ifelse(hourly_sub$temp==9999,'',hourly_sub$temp)
hourly_sub$temp <- ifelse(hourly_sub$temp==999.9,'',hourly_sub$temp)
hourly_sub$dewpt <- ifelse(hourly_sub$dewpt==9999,'',hourly_sub$dewpt)
hourly_sub$dewpt <- ifelse(hourly_sub$dewpt==999.9,'',hourly_sub$dewpt)
hourly_sub$atmpr <- ifelse(hourly_sub$atmpr==99999,'',hourly_sub$atmpr)
hourly_sub$precip1hr <- ifelse(hourly_sub$precip1hr==99,'',hourly_sub$precip1hr)
hourly_sub$precip1dpth <- ifelse(hourly_sub$precip1dpth==999.9,'',hourly_sub$precip1dpth)
hourly_sub$precip2hr <- ifelse(hourly_sub$precip2hr==99,'',hourly_sub$precip2hr)
hourly_sub$precip2dpth <- ifelse(hourly_sub$precip2dpth==999.9,'',hourly_sub$precip2dpth)
hourly_sub$precip3hr <- ifelse(hourly_sub$precip3hr==99,'',hourly_sub$precip3hr)
hourly_sub$precip3dpth <- ifelse(hourly_sub$precip3dpth==999.9,'',hourly_sub$precip3dpth)
hourly_sub$precip4hr <- ifelse(hourly_sub$precip4hr==99,'',hourly_sub$precip4hr)
hourly_sub$precip4dpth <- ifelse(hourly_sub$precip4dpth==999.9,'',hourly_sub$precip4dpth)
hourly_sub$prcpmnthdpth <- ifelse(hourly_sub$prcpmnthdpth==9999.9,'',hourly_sub$prcpmnthdpth)
hourly_sub$prcphstdur <- ifelse(hourly_sub$prcphstdur==9,'',hourly_sub$prcphstdur)
hourly_sub$snowdpth <- ifelse(hourly_sub$snowdpth==9,'',hourly_sub$snowdpth)
hourly_sub$snowwteq <- ifelse(hourly_sub$snowwteq==9,'',hourly_sub$snowwteq)
hourly_sub$snowacc1hrs <- ifelse(hourly_sub$snowacc1hrs==99,'',hourly_sub$snowacc1hrs)
hourly_sub$snowacc1dpth <- ifelse(hourly_sub$snowacc1dpth==999,'',hourly_sub$snowacc1dpth)
hourly_sub$snowacc2hrs <- ifelse(hourly_sub$snowacc2hrs==99,'',hourly_sub$snowacc2hrs)
hourly_sub$snowacc2dpth <- ifelse(hourly_sub$snowacc2dpth==999,'',hourly_sub$snowacc2dpth)
hourly_sub$snowacc3hrs <- ifelse(hourly_sub$snowacc3hrs==99,'',hourly_sub$snowacc3hrs)
hourly_sub$snowacc3dpth <- ifelse(hourly_sub$snowacc3dpth==999,'',hourly_sub$snowacc3dpth)
hourly_sub$snowacc4hrs <- ifelse(hourly_sub$snowacc4hrs==99,'',hourly_sub$snowacc4hrs)
hourly_sub$snowacc4dpth <- ifelse(hourly_sub$snowacc4dpth==999,'',hourly_sub$snowacc4dpth)
hourly_sub$prcpmin1min <- ifelse(hourly_sub$prcpmin1min==99,'',hourly_sub$prcpmin1min)
hourly_sub$prcpmin1dpth <- ifelse(hourly_sub$prcpmin1dpth==999.9,'',hourly_sub$prcpmin1dpth)
hourly_sub$prcpmin2min <- ifelse(hourly_sub$prcpmin2min==99,'',hourly_sub$prcpmin2min)
hourly_sub$prcpmin2dpth <- ifelse(hourly_sub$prcpmin2dpth==999.9,'',hourly_sub$prcpmin2dpth)
hourly_sub$prcpmin3min <- ifelse(hourly_sub$prcpmin3min==99,'',hourly_sub$prcpmin3min)
hourly_sub$prcpmin3dpth <- ifelse(hourly_sub$prcpmin3dpth==999.9,'',hourly_sub$prcpmin3dpth)
hourly_sub$prcpmin4min <- ifelse(hourly_sub$prcpmin4min==99,'',hourly_sub$prcpmin4min)
hourly_sub$prcpmin4dpth <- ifelse(hourly_sub$prcpmin4dpth==999.9,'',hourly_sub$prcpmin4dpth)
hourly_sub$prcp15min1mm <- ifelse(hourly_sub$prcp15min1mm==999.9,'',hourly_sub$prcp15min1mm)
hourly_sub$prcp15min2mm <- ifelse(hourly_sub$prcp15min2mm==999.9,'',hourly_sub$prcp15min2mm)
hourly_sub$prcp15min3mm <- ifelse(hourly_sub$prcp15min3mm==999.9,'',hourly_sub$prcp15min3mm)
hourly_sub$prcp15min4mm <- ifelse(hourly_sub$prcp15min4mm==999.9,'',hourly_sub$prcp15min4mm)
hourly_sub$datetime <- as.POSIXct(paste(hourly_sub$Date,formatC(hourly_sub$Time,width=4,format="d",flag="0"),' '),format="%Y%m%d %H%M")

# daily_summSub <- daily_summ[which(daily_summ$date<strptime("1991-01-01","%Y-%m-%d")),]
# hourly_subSub <- hourly_sub[which(floor_date(hourly_sub$datetime,unit="day")<max(floor_date(daily_summSub$date))),]

#daily_summOne <- daily_summ[which(daily_summ$date<strptime("1990-10-02","%Y-%m-%d")),]
#hourly_test <- hourly_sub[1:24,]
#hourly_test$date <- floor_date(hourly_test$datetime,unit="day")
#hourly_subOne <- hourly_sub[which(floor_date(hourly_sub$datetime,unit="day")<=max(floor_date(daily_summOne$date,unit="day"))),]
#hourly_subOne <- hourly_subOne[,c(1:5,)]

# test hourly_sub daily aggregation vs daily_summ
# hourly_agg_temp <- aggregate(as.numeric(hourly_subSub$temp)*1.8+32,list(hourly_subSub$Date),mean,na.rm=TRUE)
# hourly_agg_dewpt <- aggregate(as.numeric(hourly_subSub$dewpt)*1.8+32,list(hourly_subSub$Date),mean,na.rm=TRUE)
# hourly_agg_precip <- aggregate(as.numeric(hourly_subSub$precip1dpth)/25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_precip2 <- aggregate(as.numeric(hourly_subSub$precip2dpth)/25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_precip3 <- aggregate(as.numeric(hourly_subSub$precip3dpth)/25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_precip4 <- aggregate(as.numeric(hourly_subSub$precip4dpth)/25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_snowwteq <- aggregate(as.numeric(hourly_subSub$snowwteq)/25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_snowacc1dpth <- aggregate(as.numeric(hourly_subSub$snowacc1dpth)*25.4,list(hourly_subSub$Date),sum,na.rm=TRUE)
# hourly_agg_snowaccmax <- aggregate(as.numeric(hourly_subSub$snowacc1dpth)/25.4,list(hourly_subSub$Date),max,na.rm=TRUE)
# 
# 
# colnames(hourly_agg_precip) <- c("Date","sumPrecip")
# colnames(hourly_agg_precip2) <- c("Date","sumPrecip2")
# colnames(hourly_agg_precip3) <- c("Date","sumPrecip3")
# colnames(hourly_agg_precip4) <- c("Date","sumPrecip4")
# precip_checkSub <- merge(hourly_agg_precip,daily_summSub[c("YEARMODA","PRCP")],by.x="Date",by.y="YEARMODA")
# precip_checkSub <- merge(hourly_agg_precip2,precip_checkSub,by.x="Date",by.y="Date")
# precip_checkSub <- merge(hourly_agg_precip3,precip_checkSub,by.x="Date",by.y="Date")
# precip_checkSub <- merge(hourly_agg_precip4,precip_checkSub,by.x="Date",by.y="Date")
