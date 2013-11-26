library(dataRetrieval)
siteNumber <- '040871475'
ParameterCd <- '00045'
StartDate <- '1997-10-01'
EndDate <- '2012-10-01'
#outfall_precip <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall7_precip.rdb",asDateTime=TRUE)
outfall_temp <- getRDB1Data("M:/NonPoint Evaluation/gmia/R/outfall7comp.rdb",asDateTime=TRUE)
#outfall_precip$datetime <- as.POSIXct(paste(outfall_precip$DATE,outfall_precip$TIME,sep=" "),format="%Y%m%d %H%M%S")
outfall_temp$datetime <- as.POSIXct(paste(outfall_temp$DATE,outfall_temp$TIME, sep=" "),format="%Y%m%d %H%M%S")
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
#GHCN_daily <- read.csv("D:/JLTData/gmia/GHCN_daily_170728.csv",header=TRUE,stringsAsFactors=FALSE)
#HRLY_PRECIP <- read.csv("M:/NonPoint Evaluation/gmia/R/hourly_precip_170692.csv",header=TRUE,stringsAsFactors=FALSE)
#HRLY_PRECIP$Date <- as.POSIXct(HRLY_PRECIP$DATE,format="%Y%m%d %H:%M")
hourly_data <- read.delim("M:/NonPoint Evaluation/gmia/R/hourly_global.txt",header=FALSE,stringsAsFactors=FALSE,skip=2,strip.white=TRUE,comment.char="",sep=",")
hourly_names <- read.delim("M:/NonPoint Evaluation/gmia/R/hourly_global.txt",header=FALSE,stringsAsFactors=FALSE,nrows=2,strip.white=TRUE,comment.char="",sep=" ")
gmia_loads <- read.csv("M:/NonPoint Evaluation/gmia/R/loading.csv",header=TRUE,stringsAsFactors=FALSE)
outLoads <- gmia_loads[which(gmia_loads$Site=='OUT'),]
cargoLoads <- gmia_loads[which(gmia_loads$Site=='CG'),]
lukeLoads <- gmia_loads[which(gmia_loads$Site=='LK'),]
infallLoads <- gmia_loads[which(gmia_loads$Site=='US'),]

outLoads$Start <- as.POSIXct(outLoads$Sample.Start.Date,format="%m-%d-%Y %H:%M")
outLoads$End <- as.POSIXct(outLoads$Sample.End.Date,format="%m-%d-%Y %H:%M")

storm_vol_load_Start <- aggregate(outLoads$Start, list(outLoads$Storm.ID), FUN = min)
colnames(storm_vol_load_Start) <- c("num","Start")
storm_vol_load_End <- aggregate(outLoads$End, list(outLoads$Storm.ID), FUN = max)
colnames(storm_vol_load_End) <- c("num","End")

storm_vol_load_load <- aggregate(outLoads$EG.PG..kg, list(outLoads$Storm.ID), FUN = sum)
colnames(storm_vol_load_load) <- c("num","TPLoad")
storm_vol_load_merge <- merge(storm_vol_load_load, storm_vol_load_Start, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_End, by = "num")
storm_vol_load_merge$start_match <- storm_vol_load_merge$Start-60

storms_list <- storm_vol_load_merge[,c(3,4)]
storms_list$Start <- storms_list$Start - (120*60)
storms_list$num <- c(1:nrow(storms_list))
norows <- nrow(storm_vol_load_merge)
noreps <- nrow(HRLY_PRECIP)
HRLY_PRECIP$stormnum <- -9
for (i in 1:noreps) {
  for (j in 1:norows) {
    HRLY_PRECIP$stormnum[i] <- ifelse(as.numeric(HRLY_PRECIP$Date[i]-storms_list$Start[j])*as.numeric(storms_list$End[j]-HRLY_PRECIP$Date[i])>=0,storms_list$num[j],HRLY_PRECIP$stormnum[i])
  }
}

precip_match <- findInterval(storm_vol_load_merge$start_match,sort(outfall_precip$datetime))
stormnum <- storm_vol_load_merge$num
precip_match_df <- data.frame(precip_match,stormnum,stringsAsFactors=FALSE)
precip_match_df_sub <- precip_match_df[which(precip_match>0),]
precip_match_df_sub$value <- outfall_precip$VALUE[precip_match_df_sub$precip_match]
colnames(precip_match_df_sub) <- c("precip_match","stormnum","precip_value")

temp_match <- findInterval(storm_vol_load_merge$start_match,sort(outfall_temp$datetime))
stormnum <- storm_vol_load_merge$num
temp_match_df <- data.frame(temp_match,stormnum,stringsAsFactors=FALSE)
temp_match_df_sub <- temp_match_df[which(temp_match>0),]
temp_match_df_sub$value <- outfall_temp$VALUE[temp_match_df_sub$temp_match]
colnames(temp_match_df_sub) <- c("temp_match","stormnum","temp_value")

weather_match <- findInterval(storm_vol_load_merge$start_match,sort(daily_summ$date))
stormnum <- storm_vol_load_merge$num
weather_match_df <- data.frame(weather_match,stormnum,stringsAsFactors=FALSE)
weather_match_df_sub <- weather_match_df[which(weather_match>0),]
weather_match_df_sub$temp <- daily_summ$TEMP[weather_match_df_sub$weather_match]
weather_match_df_sub$dewp <- daily_summ$DEWP[weather_match_df_sub$weather_match]
weather_match_df_sub$slp <- daily_summ$SLP[weather_match_df_sub$weather_match]
weather_match_df_sub$stp <- daily_summ$STP[weather_match_df_sub$weather_match]
weather_match_df_sub$visib <- daily_summ$VISIB[weather_match_df_sub$weather_match]

colnames(weather_match_df_sub) <- c("weather_match","stormnum","temp","dewp","slp","stp","visib")

data_merge <- merge(storm_vol_load_merge,precip_match_df_sub,by.x="num",by.y="stormnum",all.x=TRUE)
data_merge <- merge(data_merge,temp_match_df_sub,by.x="num",by.y="stormnum",all.x=TRUE)
data_merge <- merge(data_merge,weather_match_df_sub,by.x="num",by.y="stormnum",all.x=TRUE)

data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",as.POSIXlt(data_merge$End)$yday+1,sep="")
data_merge$sinDY <- sin(as.numeric(data_merge$decYear)*2*pi)
data_merge$cosDY <- cos(as.numeric(data_merge$decYear)*2*pi)
data_sub <- data_merge
data_sub$remark <- ""
data_sub$precip_value <- as.numeric(data_sub$precip_value)
data_sub$temp_value <- as.numeric(data_sub$temp_value)
data_sub$stp <- as.numeric(data_sub$stp)
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
data_sub_cens <- importQW(data_sub,c("precip_value","temp_value","temp","dewp","slp","stp","visib","sinDY","cosDY","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","91080","TPLoading")
siteName <- "Outfall"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
# name of value column in data_sub_cens object
investigateResponse <- "TPLoading"
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










gmia_hourly_global <- read.delim("D:/JLTData/gmia/hourly_global.txt",stringsAsFactors=FALSE)