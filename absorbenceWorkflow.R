# library(USGSAqualogFormatting)
# source("C:/Users/jlthomps/Desktop/git/GMIA/formatAbsSamplesJT.R")
# FinalAbsDf <- formatAbsSamplesJT(dateLower='20130930',dateUpper='20140918',Type='All',Project='GMIA')
# #FinalAbsDfSESQ <- formatAbsSamplesJT(dateLower='20130930',dateUpper='20140918',Type='All',Project='SESQA')
# # had to do some fooling around in function b/c of missing files in 20140121b
# # also added Project ID to name so can differentiate OUT/LK/CG
# 
# testnames <- colnames(FinalAbsDf)
# testnames <- gsub("USGS","Group",testnames)
# colnames(FinalAbsDf) <- testnames
# #wavs <- c(251,254,257)
# testnames <- testnames[1:274]
# test <- data.frame(testnames,stringsAsFactors=FALSE)
# colnames(test) <- "GRnumber"
# wavs <- unique(FinalAbsDf$Wavelength)
# wavs <- wavs[which(wavs<=700)]
# library(USGSHydroOpt)
# testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
# finalcols <- colnames(FinalAbsDf)
# finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
# FinalAbsDf <- FinalAbsDf[,finalcols]
# setwd("C:/Users/jlthomps/Desktop/git/GMIA/")
# write.csv(testAbs,file="testAbs.csv")
# write.csv(FinalAbsDf,file="FinalAbsDf.csv")
# save(testAbs,file="testAbs.RData")
# save(FinalAbsDf,file="FinalAbsDf.RData")

setwd("/Users/jlthomps/Desktop/git/GMIA")
load("testAbs.RData")
load("FinalAbsDf.RData")

temp <- testAbs$GRnumber
temp2 <- substr(testAbs$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(pattern="_2",temp))+13)
temp3 <- temp
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  temp3[i] <- b[1]
}

testAbs$date <- substr(temp2,nchar(temp2)-7,nchar(temp2))
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
testAbsWorking <- testAbsWorking[-which(substr(testAbsWorking$GRnumber,1,1)=='Q'),]

grnumsIn <- unique(testAbsWorking$GRnumber)
#grnumsIn <- grnumsIn[-which(substr(grnumsIn,1,1)=='Q')]
grnumsIn <- c(grnumsIn,"Wavelength")
FinalAbsDf <- FinalAbsDf[,grnumsIn]
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
colsKeep <- c('Wavelength','meanAbs','meanAbsCG','meanAbsOUT','meanAbsOA','meanAbsLK')
absDf <- FinalAbsDf[,colsKeep]
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

absDf <- FinalAbsDf[,c(1:105)]
nsteps <- 104
pdf(paste(pathToSave,"/","PlotAbsSamples.pdf",sep=""))
for (i in 1:nsteps) {  
  par(tcl=0.3)
  plot(absDf[,WaveCol],absDf[,i],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main=paste("Absorbance plot ",finalcolsALL[i],sep=""))
}
dev.off()

pdf(paste(pathToSave,"/","PlotAbsSamplesSubset.pdf",sep=""))
parOriginal <- par(no.readonly = TRUE)

sample1 <- "OUT-S105_Group002GMIA0006_2013/20131223"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 105")
legend("topright",c("Acetate 1430","Ethylene Glycol <10","Propylene Glycol 1100","Formate <25"))
par(parOriginal)
sample1 <- "Out-S106_Group001GMIA0008_2014/20140121"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 106")
legend("topright",c("Acetate 490","Ethylene Glycol <10","Propylene Glycol 330","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S107_Group002GMIA0003_2014/20140227"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 107")
legend("topright",c("Acetate 461","Ethylene Glycol <10","Propylene Glycol 340","Formate <2.5"))
par(parOriginal)
sample1 <- "CG-S106_Group001GMIA0006_2014/20140121"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 106")
legend("topright",c("Acetate 622","Ethylene Glycol 37","Propylene Glycol 2500","Formate <2.5"))
par(parOriginal)
sample1 <- "CG-S107_Group002GMIA0008_2014/20140227"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 107")
legend("topright",c("Acetate 125","Ethylene Glycol 32","Propylene Glycol 1500","Formate 4.66"))
par(parOriginal)
sample1 <- "CG-S111_Group002GMIA0008_2014/20141201"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 111")
legend("topright",c("Acetate 864","Ethylene Glycol 100","Propylene Glycol 1400","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S114A_Group002GMIA0001_20150305"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 114")
legend("topright",c("Acetate 136","Ethylene Glycol 23","Propylene Glycol 77","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S113_Group002GMIA0005_20150106"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 113")
legend("topright",c("Acetate 179","Ethylene Glycol <10","Propylene Glycol 1400","Formate <2.5"))
par(parOriginal)
dev.off()


library(USGSHydroOpt)
source("/Users/jlthomps/Desktop/git/GMIA/getExpResidJT.R")
wavelength <- 491
rangeReg <- c(419,602)
rangeGap <- c(461,521)
colsAbs <- unique(testAbsWorking$GRnumber)
#colsAbs <- colsAbs[-which(substr(colsAbs,1,1)=='Q')]
colsAbs <- c(colsAbs,"Wavelength")
dataAbs <- FinalAbsDf[,colsAbs]
waveCol <- "Wavelength"
colSubsetString <- "Gr"
dataSummary <- testAbsWorking
grnum <- "GRnumber"
testdfOpt <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)

wavelength <- 629
rangeReg <- c(560,698)
rangeGap <- c(605,662)
dataSummary <- testdfOpt
testdfOpt2 <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- cols2[1:158]
cols2 <- c(cols2,"Resids490","Resids630")
colnames(testdfOpt2) <- cols2

wavelength <- 422
rangeReg <- c(377,497)
rangeGap <- c(404,452)
dataSummary <- testdfOpt2
testdfOpt3 <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- c(cols2,"Resids422")
colnames(testdfOpt3) <- cols2

sag <- read.csv("SagVals.csv",stringsAsFactors=FALSE)
colSubsetString <- "Gr"
dataSummary <- testdfOpt3
grnum <- "GRnumber"
source("/Users/jlthomps/Desktop/git/GMIA/getSagJT.R")
GMIASag <- getSagJT(FinalAbsDf,WaveCol,sag,colSubsetString,dataSummary,"GRnumber")

#library(dataRetrieval)
#setwd("C:/Users/jlthomps/GMIA")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
#dataMerge <- transform(dataMerge,app600=(A617+A620+A623+A626+A629+A632+A635+A638+A641+A644)) 
#dataMerge <- transform(dataMerge,app500=(A488+A491+A494+A497+A500+A503+A506+A509))
#dataMerge <- transform(dataMerge,app420=(A416+A419+A422+A425+A428+A431+A434))
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
#data_sub <- dataMerge[,c("remark","COD","decYear","A788","A731","A722","A719","A716","A713","A587","A506","A503","A500","A428","A242","A239")]
data_sub$Site <- ifelse(data_sub$Site=='CG','2',ifelse(data_sub$Site=='LK','1',ifelse(data_sub$Site=='OAK','3','4')))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA"
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
kitchenSink <- createFullFormula(data_sub_cens[,2:174],investigateResponse)

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

# dataMergeOUTCG <- dataMerge[which(dataMerge$Site %in% c("CG","OUT")),]
# data_sub <- dataMergeOUTCG[,c("remark","COD","decYear","A788","A785","A782","A773","A770","A767","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A506","A503","A500","A491","A488","A485","A482","A479","A476","A473","A293","A290","A287","A284","A281","A263","A260","A257","A254","A251","A248","A245","A242","A239","Sag239_242.x","Sag248_254.x","Sag251_257.x","Sag257_263.x","Sag281_287.x","Sag287_293.x","Sag473_479.x","Sag482_488","Sag485_491","Sag497_503","Sag500_506","Sag518_524","Sag530_536","Sag542_548","Sag545_551","Sag611_617","Sag617_623","Sag629_635","Sag635_641","Sag641_647","Sag650_656","Sag656_662","Sag767_773","Sag782_788")]
# data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
# siteName <- "GMIAOUTCG"
# siteNo <- '040871475'
# siteINFO <-  getNWISSiteInfo(siteNo)
# siteINFO$station.nm <- siteINFO$station_nm
# # name of value column in data_sub_cens object
# investigateResponse <- "CODcens"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")
# 
# #################################################################################################
# #Kitchen sink:
# predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
# predictVariables <- predictVariables[which(predictVariables != "datetime")]
# predictVariables <- predictVariables[which(predictVariables != "decYear")]
# kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)
# 
# returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
#                                "BIC", #Other option is "AIC"
#                                transformResponse)
# 
# steps <- returnPrelim$steps
# modelResult <- returnPrelim$modelStuff
# modelReturn <- returnPrelim$DT.mod
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotStepsGMIA(steps,data_sub_cens,transformResponse)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
# analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
# dev.off()
# 
# #################################################################################################
# 
# ##########################################################
# #Save steps to file:
# fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
# write.table(steps, fileToSave, row.names=FALSE, sep=",") 
# ##########################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################
# 
# dataMergeOUT <- dataMerge[which(dataMerge$Site=="OUT"),]
# data_sub <- dataMergeOUT[,c("remark","COD","decYear","A788","A785","A782","A773","A770","A767","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A506","A503","A500","A491","A488","A485","A482","A479","A476","A473","A293","A290","A287","A284","A281","A263","A260","A257","A254","A251","A248","A245","A242","A239","Sag239_242.x","Sag248_254.x","Sag251_257.x","Sag257_263.x","Sag281_287.x","Sag287_293.x","Sag473_479.x","Sag482_488","Sag485_491","Sag497_503","Sag500_506","Sag518_524","Sag530_536","Sag542_548","Sag545_551","Sag611_617","Sag617_623","Sag629_635","Sag635_641","Sag641_647","Sag650_656","Sag656_662","Sag767_773","Sag782_788")]
# data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
# siteName <- "GMIAOUT"
# siteNo <- '040871475'
# siteINFO <-  getNWISSiteInfo(siteNo)
# siteINFO$station.nm <- siteINFO$station_nm
# # name of value column in data_sub_cens object
# investigateResponse <- "CODcens"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")
# 
# #################################################################################################
# #Kitchen sink:
# predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
# predictVariables <- predictVariables[which(predictVariables != "datetime")]
# predictVariables <- predictVariables[which(predictVariables != "decYear")]
# kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)
# 
# returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
#                                "BIC", #Other option is "AIC"
#                                transformResponse)
# 
# steps <- returnPrelim$steps
# modelResult <- returnPrelim$modelStuff
# modelReturn <- returnPrelim$DT.mod
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotStepsGMIA(steps,data_sub_cens,transformResponse)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
# analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
# dev.off()
# 
# #################################################################################################
# 
# ##########################################################
# #Save steps to file:
# fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
# write.table(steps, fileToSave, row.names=FALSE, sep=",") 
# ##########################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################
# 
# dataMergeCG <- dataMerge[which(dataMerge$Site=="CG"),]
# data_sub <- dataMergeCG[,c("remark","COD","decYear","A788","A785","A782","A773","A770","A767","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A506","A503","A500","A491","A488","A485","A482","A479","A476","A473","A293","A290","A287","A284","A281","A263","A260","A257","A254","A251","A248","A245","A242","A239","Sag239_242.x","Sag248_254.x","Sag251_257.x","Sag257_263.x","Sag281_287.x","Sag287_293.x","Sag473_479.x","Sag482_488","Sag485_491","Sag497_503","Sag500_506","Sag518_524","Sag530_536","Sag542_548","Sag545_551","Sag611_617","Sag617_623","Sag629_635","Sag635_641","Sag641_647","Sag650_656","Sag656_662","Sag767_773","Sag782_788")]
# data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
# siteName <- "GMIACG"
# siteNo <- '040871475'
# siteINFO <-  getNWISSiteInfo(siteNo)
# siteINFO$station.nm <- siteINFO$station_nm
# # name of value column in data_sub_cens object
# investigateResponse <- "CODcens"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")
# 
# #################################################################################################
# #Kitchen sink:
# predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
# predictVariables <- predictVariables[which(predictVariables != "datetime")]
# predictVariables <- predictVariables[which(predictVariables != "decYear")]
# kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)
# 
# returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
#                                "BIC", #Other option is "AIC"
#                                transformResponse)
# 
# steps <- returnPrelim$steps
# modelResult <- returnPrelim$modelStuff
# modelReturn <- returnPrelim$DT.mod
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotStepsGMIA(steps,data_sub_cens,transformResponse)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
# analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
# dev.off()
# 
# #################################################################################################
# 
# ##########################################################
# #Save steps to file:
# fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
# write.table(steps, fileToSave, row.names=FALSE, sep=",") 
# ##########################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################
# 
# dataMergeLK <- dataMerge[which(dataMerge$Site=="LK"),]
# data_sub <- dataMergeLK[,c("remark","COD","decYear","A788","A785","A782","A773","A770","A767","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A506","A503","A500","A491","A488","A485","A482","A479","A476","A473","A293","A290","A287","A284","A281","A263","A260","A257","A254","A251","A248","A245","A242","A239","Sag239_242.x","Sag248_254.x","Sag251_257.x","Sag257_263.x","Sag281_287.x","Sag287_293.x","Sag473_479.x","Sag482_488","Sag485_491","Sag497_503","Sag500_506","Sag518_524","Sag530_536","Sag542_548","Sag545_551","Sag611_617","Sag617_623","Sag629_635","Sag635_641","Sag641_647","Sag650_656","Sag656_662","Sag767_773","Sag782_788")]
# data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
# siteName <- "GMIALK"
# siteNo <- '040871475'
# siteINFO <-  getNWISSiteInfo(siteNo)
# siteINFO$station.nm <- siteINFO$station_nm
# # name of value column in data_sub_cens object
# investigateResponse <- "CODcens"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")
# 
# #################################################################################################
# #Kitchen sink:
# predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
# predictVariables <- predictVariables[which(predictVariables != "datetime")]
# predictVariables <- predictVariables[which(predictVariables != "decYear")]
# kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)
# 
# returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
#                                "BIC", #Other option is "AIC"
#                                transformResponse)
# 
# steps <- returnPrelim$steps
# modelResult <- returnPrelim$modelStuff
# modelReturn <- returnPrelim$DT.mod
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotStepsGMIA(steps,data_sub_cens,transformResponse)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
# analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
# dev.off()
# 
# #################################################################################################
# 
# ##########################################################
# #Save steps to file:
# fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
# write.table(steps, fileToSave, row.names=FALSE, sep=",") 
# ##########################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################
# 
# dataMergeOAK <- dataMerge[which(dataMerge$Site=="OAK"),]
# data_sub <- dataMergeOAK[,c("remark","COD","decYear","A788","A785","A782","A773","A770","A767","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A506","A503","A500","A491","A488","A485","A482","A479","A476","A473","A293","A290","A287","A284","A281","A263","A260","A257","A254","A251","A248","A245","A242","A239","Sag239_242.x","Sag248_254.x","Sag251_257.x","Sag257_263.x","Sag281_287.x","Sag287_293.x","Sag473_479.x","Sag482_488","Sag485_491","Sag497_503","Sag500_506","Sag518_524","Sag530_536","Sag542_548","Sag545_551","Sag611_617","Sag617_623","Sag629_635","Sag635_641","Sag641_647","Sag650_656","Sag656_662","Sag767_773","Sag782_788")]
# data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
# siteName <- "GMIAOAK"
# siteNo <- '040871475'
# siteINFO <-  getNWISSiteInfo(siteNo)
# siteINFO$station.nm <- siteINFO$station_nm
# # name of value column in data_sub_cens object
# investigateResponse <- "CODcens"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")
# 
# #################################################################################################
# #Kitchen sink:
# predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
# predictVariables <- predictVariables[which(predictVariables != "datetime")]
# predictVariables <- predictVariables[which(predictVariables != "decYear")]
# kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)
# 
# returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
#                                "BIC", #Other option is "AIC"
#                                transformResponse)
# 
# steps <- returnPrelim$steps
# modelResult <- returnPrelim$modelStuff
# modelReturn <- returnPrelim$DT.mod
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotStepsGMIA(steps,data_sub_cens,transformResponse)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
# analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
# dev.off()
# 
# #################################################################################################
# 
# ##########################################################
# #Save steps to file:
# fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
# write.table(steps, fileToSave, row.names=FALSE, sep=",") 
# ##########################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################
# 
