
setwd("/Users/jlthomps/Desktop/git/GMIA")
load("testAbs.RData")
load("FinalAbsDf.RData")

finalcols <- colnames(FinalAbsDf)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
finalcolsOUT <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou"))]
finalcolsCG <- finalcols[which(substr(finalcols,1,2) %in% c("CG"))]
finalcolsLK <- finalcols[which(substr(finalcols,1,2) %in% c("LK"))]
finalcolsOA <- finalcols[which(substr(finalcols,1,2) %in% c("OA"))]
finalcolsALL <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA"))]
FinalAbsDf$meanAbs <- rowMeans(FinalAbsDf[,finalcolsALL])
FinalAbsDf$meanAbsOUT <- rowMeans(FinalAbsDf[,finalcolsOUT])
FinalAbsDf$meanAbsCG <- rowMeans(FinalAbsDf[,finalcolsCG])
FinalAbsDf$meanAbsLK <- rowMeans(FinalAbsDf[,finalcolsLK])
FinalAbsDf$meanAbsOA <- rowMeans(FinalAbsDf[,finalcolsOA])
FinalAbsDf[FinalAbsDf<0] <- NA
FinalAbsDf$minAbs <- do.call(pmin,c(FinalAbsDf[,finalcolsALL],na.rm=TRUE))
FinalAbsDf[is.na(FinalAbsDf)] <- min(FinalAbsDf$minAbs)

pathToSave <- "/Users/jlthomps/Documents/R/GMIA"
absDf <- FinalAbsDf[,c(49:54)]
WaveCol <- "Wavelength"
titleSize <- 1.1
mainTitle <- "GMIA Absorbance Plot"
xlim <- c(min(FinalAbsDf$Wavelength),max(FinalAbsDf$Wavelength))
ylim <- c(0,.1)
pdf(paste(pathToSave,"/","plotAbs.pdf",sep=""))
plot(absDf[,WaveCol],absDf[,2],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,.03))
points(absDf[,WaveCol],absDf[,3],type="l",col="green")
points(absDf[,WaveCol],absDf[,4],type="l",col="red")
points(absDf[,WaveCol],absDf[,5],type="l",col="black")
points(absDf[,WaveCol],absDf[,6],type="l",col="pink")
legend("topright",c("All","OUT","CG","LK","OAK"),col=c("blue","green","red","black","pink"),lty=c(1,1,1,1,1))
dev.off()

absDf <- FinalAbsDf[,c(1:49)]
nsteps <- 48
pdf(paste(pathToSave,"/","PlotAbsSamples.pdf",sep=""))
for (i in 1:nsteps) {  
  par(tcl=0.3)
  plot(absDf[,WaveCol],absDf[,i],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main=paste("Absorbance plot ",finalcolsALL[i],sep=""))
}
dev.off()

temp <- testAbs$GRnumber
temp2 <- substr(testAbs$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(pattern="_2",temp))+8)
temp3 <- temp
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  temp3[i] <- b[1]
}

testAbs$date <- temp2
testAbs$ProjectID <- temp3
testAbs$datetime <- strptime(testAbs$date,format="%Y%m%d")
testAbsGMIA <- testAbs[substr(testAbs$ProjectID,1,2) %in% c("OU","Ou","CG","LK","US","OAK"),]

testAbsOAK <- testAbs[grep("OAK-",testAbs$ProjectID),]
testAbsOAK <- testAbsOAK[which(paste(testAbsOAK$ProjectID,testAbsOAK$date,sep="")!="OAK-S10720140225"),]

testAbsOUT <- testAbs[substr(testAbs$ProjectID,1,2) %in% c("OU","Ou"),]
testAbsOUT <- testAbsOUT[-grep("-R",testAbsOUT$ProjectID),]
testAbsOUT <- testAbsOUT[which(paste(testAbsOUT$ProjectID,testAbsOUT$date,sep="")!="OUT-S10720140225"),]
testAbsOUT <- testAbsOUT[which(paste(testAbsOUT$ProjectID,testAbsOUT$date,sep="")!="OUT-S107G20140225"),]
testAbsOUT <- testAbsOUT[which(substr(testAbsOUT$GRnumber,1,18)!="OUT-S110G_Group003"),]
testAbsOUT$ProjectID <- gsub('Out','OUT',testAbsOUT$ProjectID)

testAbsCG <- testAbs[grep("CG-",testAbs$ProjectID),]
testAbsCG <- testAbsCG[which(paste(testAbsCG$ProjectID,testAbsCG$date,sep="")!="CG-S10720140225"),]
testAbsCG <- testAbsCG[which(testAbsCG$ProjectID!="CG-Q23C"),]

testAbsLK <- testAbs[grep("LK-",testAbs$ProjectID),]
testAbsLK <- testAbsLK[which(testAbsLK$ProjectID!="LK-Q23C"),]
testAbsLK <- testAbsLK[-grep("-R",testAbsLK$ProjectID),]
testAbsLK <- testAbsLK[which(paste(testAbsLK$ProjectID,testAbsLK$date,sep="")!="LK-S10720140225"),]
testAbsLK <- testAbsLK[which(paste(testAbsLK$ProjectID,testAbsLK$date,sep="")!="LK-S107G20140225"),]

testAbsWorking <- rbind(testAbsOUT,testAbsCG)
testAbsWorking <- rbind(testAbsWorking,testAbsLK)
testAbsWorking <- rbind(testAbsWorking,testAbsOAK)

library(dataRetrieval)
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
COD2014$startDatetime <- strptime(COD2014$startDate,"%m/%d/%Y %H:%M")
COD2014$endDatetime <- strptime(COD2014$endDate,"%m/%d/%Y %H:%M")
WTempOUT <- readNWISuv("040871475","00010",startDate="2013-12-01",endDate="",tz="")
WTempLK <- readNWISuv("040871488","00010",startDate="2013-12-01",endDate="",tz="")
WTempCG <- readNWISuv("040871476","00010",startDate="2013-12-01",endDate="",tz="")
WTempOAK <- readNWISuv("040872015","00010",startDate="2013-12-01",endDate="",tz="")

COD2014 <- COD2014[which(!is.na(COD2014$startDatetime)),]
COD2014[which(nchar(COD2014$endDate)==0),]$endDatetime <- COD2014[which(nchar(COD2014$endDate)==0),]$startDatetime+900
COD2014[which(nchar(COD2014$endDate)==0),]$startDatetime <- COD2014[which(nchar(COD2014$endDate)==0),]$startDatetime-900

COD2014CG <- COD2014[which(COD2014$Site=="CG"),]
library(USGSHydroTools)
COD2014CGTemp <- TSstormstats(WTempCG,date="dateTime","X_00010_00011",COD2014CG,starttime="startDatetime",endtime="endDatetime",stats.return="mean",out.varname="temp")
COD2014OUT <- COD2014[which(COD2014$Site=="OUT"),]
COD2014OUTTemp <- TSstormstats(WTempOUT,date="dateTime","X_00010_00011",COD2014OUT,starttime="startDatetime",endtime="endDatetime",stats.return="mean",out.varname="temp")
COD2014LK <- COD2014[which(COD2014$Site=="LK"),]
COD2014LKTemp <- TSstormstats(WTempLK,date="dateTime","X_00010_00011",COD2014LK,starttime="startDatetime",endtime="endDatetime",stats.return="mean",out.varname="temp")
COD2014ALL <- rbind(COD2014CGTemp,COD2014OUTTemp)
COD2014ALL <- rbind(COD2014ALL,COD2014LKTemp)

sag <- read.csv("SagVals.csv",stringsAsFactors=FALSE)
colSubsetString <- "Gr"
dataSummary <- testAbsWorking
grnum <- "GRnumber"
source("/Users/jlthomps/Desktop/git/GMIA/getSagJT.R")
GMIASag <- getSagJT(FinalAbsDf,WaveCol,sag,colSubsetString,dataSummary,"GRnumber")

dataMerge <- merge(COD2014ALL,GMIASag,by="ProjectID")


library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge <- dataMerge[which(!is.na(dataMerge$temp_mean)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
#dataMerge <- transform(dataMerge,app600=(A617+A620+A623+A626+A629+A632+A635+A638+A641+A644)) 
#dataMerge <- transform(dataMerge,app500=(A488+A491+A494+A497+A500+A503+A506+A509))
#dataMerge <- transform(dataMerge,app420=(A416+A419+A422+A425+A428+A431+A434))
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate","startDatetime","endDatetime"))]
data_sub <- dataMerge[,keepCols]
#data_sub <- dataMerge[,c("remark","COD","decYear","A788","A731","A722","A719","A716","A713","A587","A506","A503","A500","A428","A242","A239")]
data_sub$Site <- ifelse(data_sub$Site=='CG','2',ifelse(data_sub$Site=='LK','1',ifelse(data_sub$Site=='OAK','3','4')))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIAwithTemp"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- siteINFO$station_nm
# name of value column in data_sub_cens object
investigateResponse <- "CODcens"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
kitchenSink <- createFullFormula(data_sub_cens[,2:172],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################


