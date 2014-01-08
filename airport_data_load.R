library(dataRetrieval)
library(USGSwsBase)
siteNumber <- '040871475'
ParameterCd <- '00060'
StartDate <- '1997-10-01'
EndDate <- '2012-10-01'

daily_summ <- read.csv("M:/NonPoint Evaluation/gmia/R/global_daily_summary_CDO2400066445620.txt",header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
daily_summ$FRSHTT <- sprintf("%06d",daily_summ$FRSHTT)
daily_summ$date <- as.Date(as.character(daily_summ$YEARMODA),format="%Y%m%d")
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

hourly_data <- read.delim("M:/NonPoint Evaluation/gmia/R/hourly_global.txt",header=FALSE,stringsAsFactors=FALSE,skip=2,strip.white=TRUE,comment.char="",sep=",")
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
hourly_sub$dewpt <- ifelse(hourly_sub$dewpt==9999,'',hourly_sub$dewpt)
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
# hourly_sub$prcpmin1min <- ifelse(hourly_sub$prcpmin1min==99,'',hourly_sub$prcpmin1min)
# hourly_sub$prcpmin1dpth <- ifelse(hourly_sub$prcpmin1dpth==999.9,'',hourly_sub$prcpmin1dpth)
# hourly_sub$prcpmin2min <- ifelse(hourly_sub$prcpmin2min==99,'',hourly_sub$prcpmin2min)
# hourly_sub$prcpmin2dpth <- ifelse(hourly_sub$prcpmin2dpth==999.9,'',hourly_sub$prcpmin2dpth)
# hourly_sub$prcpmin3min <- ifelse(hourly_sub$prcpmin3min==99,'',hourly_sub$prcpmin3min)
# hourly_sub$prcpmin3dpth <- ifelse(hourly_sub$prcpmin3dpth==999.9,'',hourly_sub$prcpmin3dpth)
# hourly_sub$prcpmin4min <- ifelse(hourly_sub$prcpmin4min==99,'',hourly_sub$prcpmin4min)
# hourly_sub$prcpmin4dpth <- ifelse(hourly_sub$prcpmin4dpth==999.9,'',hourly_sub$prcpmin4dpth)
# hourly_sub$prcp15min1mm <- ifelse(hourly_sub$prcp15min1mm==999.9,'',hourly_sub$prcp15min1mm)
# hourly_sub$prcp15min2mm <- ifelse(hourly_sub$prcp15min2mm==999.9,'',hourly_sub$prcp15min2mm)
# hourly_sub$prcp15min3mm <- ifelse(hourly_sub$prcp15min3mm==999.9,'',hourly_sub$prcp15min3mm)
# hourly_sub$prcp15min4mm <- ifelse(hourly_sub$prcp15min4mm==999.9,'',hourly_sub$prcp15min4mm)
hourly_sub$datetime <- as.POSIXct(paste(hourly_sub$Date,formatC(hourly_sub$Time,width=4,format="d",flag="0"),' '),format="%Y%m%d %H%M")

#test hourly_sub daily aggregation vs daily_summ
hourly_agg_temp <- aggregate(hourly_sub$temp,list(hourly_sub$Date),mean)
hourly_agg_dewpt <- aggregate(hourly_sub$dewpt,list(hourly_sub$Date),mean)
hourly_agg_precip <- aggregate(as.numeric(hourly_sub$precip1dpth),list(hourly_sub$Date),sum)
hourly_agg_snowwteq <- aggregate(as.numeric(hourly_sub$snowwteq),list(hourly_sub$Date),sum)
hourly_agg_snowacc1dpth <- aggregate(as.numeric(hourly_sub$snowacc1dpth),list(hourly_sub$Date),sum)
hourly_agg_snowaccmax <- aggregate(as.numeric(hourly_sub$snowacc1dpth),list(hourly_sub$Date),max)

inst_disch <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall00060.rdb",asDateTime=TRUE)
inst_disch$datetime <- as.POSIXct(paste(inst_disch$DATE,inst_disch$TIME,sep=' '),format="%Y%m%d %H%M%S")

daily_disch <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall00060DV.rdb",asDateTime=TRUE)
daily_disch$datetime <- as.POSIXct(daily_disch$DATE,format="%Y%m%d")

gmia_storms <- read.csv("M:/NonPoint Evaluation/gmia/R/loading.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_storms <- gmia_storms[which(gmia_storms$Site=="OUT"),c(2,5:7)]
#gmia_storms <- unique(gmia_storms[,c(2,5:6)])
gmia_storms$Sample.Start.Date <- as.POSIXct(gmia_storms$Sample.Start.Date,format="%m/%d/%Y %H:%M")
gmia_storms$Sample.End.Date <- as.POSIXct(gmia_storms$Sample.End.Date,format="%m/%d/%Y %H:%M")

gmia_application <- read.csv("M:/NonPoint Evaluation/gmia/R/report.event.table.csv",header=TRUE,stringsAsFactors=FALSE)
gmia_application$datetime <- as.POSIXct(gmia_application$Date,format="%m/%d/%Y")

daily_ice <- daily_disch[which(daily_disch$TYPE=="E"),]
inst_disch <- merge(daily_ice,inst_disch,by=c("DATE"),all.y=TRUE)
inst_disch$VALUE.y <- ifelse(is.na(inst_disch$TYPE),inst_disch$VALUE.y,inst_disch$VALUE.x)

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

source("M:/NonPoint Evaluation/gmia/R/fxn_Hydrovol.R")
hydrovol_data <- Hydrovol(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
hydrovol_data$vol.liters <- hydrovol_data$event.vol*28.31685
#hydrovol_data <- hydrovol_data[order(hydrovol_data$bpdate),]
#gmia_storms <- gmia_storms[order(gmia_storms$Sample.Start.Date),]
hydrovol_data$stormnum <- gmia_storms$Storm.ID

storm_qwdata <- read.delim("M:/NonPoint Evaluation/gmia/R/outfallQW.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
storm_qwdata <- storm_qwdata[which(nchar(storm_qwdata$SAMPLE_END_DT)>0),]
storm_qwdata$SAMPLE_START_DT <- as.POSIXct(storm_qwdata$SAMPLE_START_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$SAMPLE_END_DT <- as.POSIXct(storm_qwdata$SAMPLE_END_DT,format="%d-%b-%y %H:%M:%S")
storm_qwdata$RECORD_NO <- as.numeric(storm_qwdata$RECORD_NO)
storm_qwdata <- merge(storm_qwdata,gmia_storms,by.x=c("RECORD_NO"),by.y="recordNo")
# temp <- mergeNearest(storm_qwdata,dates.left="SAMPLE_START_DT",right=hydrovol_data,dates.right="bpdate",max.diff="8 hours")
# temp2 <- mergeNearest(storm_qwdata,dates.left="SAMPLE_END_DT",right=hydrovol_data,dates.right="epdate",max.diff="8 hours")
# storm_qwdata <- merge(storm_qwdata,temp[,c("RECORD_NO","PARM_CD","stormnum")],by=c("RECORD_NO","PARM_CD"))
# storm_qwdata <- merge(storm_qwdata,temp2[,c("RECORD_NO","PARM_CD","stormnum")],by=c("RECORD_NO","PARM_CD"))
#storm_qwdata <- storm_qwdata[order(storm_qwdata$SAMPLE_START_DT),]

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
#hourly_match <- findInterval(data_merge$bpdate,sort(hourly_sub$datetime))
#hourly_match_df <- data.frame(hourly_match,data_merge$stormnum,stringsAsFactors=FALSE)
#hourly_match_df_sub <- hourly_match_df[which(hourly_match>0),]
#hourly_match_end <- findInterval(data_merge$epdate,sort(hourly_sub$datetime))
#hourly_match_end_df <- data.frame(hourly_match_end,data_merge$stormnum,stringsAsFactors=FALSE)
#hourly_match_end_df_sub <- hourly_match_end_df[which(hourly_match_end>0),]

#storms_merge <- merge(hourly_match_df,hourly_match_end_df,by=c("data_merge.stormnum"))
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

data_merge <- merge(storm_merge,gmia_application[,c(1,4:7)],by.x=c("stormnum"),by.y=c("Event.number"),all.x=TRUE)
data_merge <- data_merge[,c(1:3,5,12:16,25:28,31:34)]
colnames(data_merge) <- c("stormnum","bpdate","epdate","Qmax","BODload","CODload","PGload","EGload","EGPGload","mean_temp","max_temp","min_temp","prcp_sum","mmprcp_deice","prcp_desc","kgGlycol","glycolOUT")
data_merge$mmprcp_deice <- as.numeric(data_merge$mmprcp_deice)
data_merge$kgGlycol <- as.numeric(data_merge$kgGlycol)
data_merge$glycolOUT <- as.numeric(data_merge$glycolOUT)
data_merge$decYear <- paste(strftime(data_merge$bpdate,"%Y"),".",as.POSIXlt(data_merge$bpdate)$yday+1,sep="")
data_merge$sinDY <- sin(as.numeric(data_merge$decYear)*2*pi)
data_merge$cosDY <- cos(as.numeric(data_merge$decYear)*2*pi)
data_sub <- data_merge
data_sub$remark <- ""
data_sub$decYear <- as.numeric(data_sub$decYear)

DTMaxCols <- na.omit(data_sub)
DTMaxRows <- data_sub[,colSums(is.na(data_sub)) <= nrow(data_sub)*0.5] 
DTMaxRows <- na.omit(DTMaxRows)

#List columns removed to maximize rows:
names(data_sub)[!(names(DTMaxCols) %in% names(data_sub))]
names(data_sub)[!(names(DTMaxRows) %in% names(data_sub))]
setdiff(data_sub$num,DTMaxRows$num)
setdiff(data_sub$num,DTMaxCols$num)
#Choose which to use: DTMaxCols or DTMaxRows:
DT <- data_sub[,c("TPLoad",names(DTMaxCols))]
data_sub <- na.omit(DT)

DT <- data_sub[,c("TPLoad",names(DTMaxRows))]
data_sub <- na.omit(DT)



# set necessary site information and inputs to step-wise regression
library(USGSwsQWSR)
#keepCens <- keepAll[-which(keepAll %in% "TPLoad")]
#data_sub_cens <- importQW(data_sub,keepCens,"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub_cens <- importQW(data_sub,c("Qmax","mean_temp","max_temp","min_temp","prcp_sum","mmprcp_deice","kgGlycol"),"BODload","remark","",0.005,"User","tons","Unk","","00310","BODLoading")
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub_cens <- importQW(data_sub,c("Qmax","mean_temp","max_temp","min_temp","prcp_sum","mmprcp_deice","kgGlycol"),"CODload","remark","",0.005,"User","tons","Unk","","00335","CODLoading")


siteName <- "Outfall"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
# name of value column in data_sub_cens object
investigateResponse <- "BODLoading"
investigateResponse <- "CODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA/",siteName,sep="")

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(data_sub_cens,investigateResponse)
predictVariableScatterPlots(data_sub_cens,investigateResponse)
dev.off()
##########################################################

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
colnames(steps) <- c("step","BIC","Deviance","Resid.Dev","Resid.Df","Correlation","Slope","RMSE","PRESS","scope","response")


#Save plotSteps to file:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGLRI(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################
