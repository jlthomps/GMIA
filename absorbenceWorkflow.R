library(USGSAqualogFormatting)
source("C:/Users/jlthomps/Desktop/git/GMIA/formatAbsSamplesJT.R")
FinalAbsDf <- formatAbsSamplesJT(dateLower='20130930',dateUpper='20140918',Type='All',Project='GMIA')
#FinalAbsDfSESQ <- formatAbsSamplesJT(dateLower='20130930',dateUpper='20140918',Type='All',Project='SESQA')
# had to do some fooling around in function b/c of missing files in 20140121b
# also added Project ID to name so can differentiate OUT/LK/CG

testnames <- colnames(FinalAbsDf)
testnames <- gsub("USGS","Group",testnames)
colnames(FinalAbsDf) <- testnames
#wavs <- c(251,254,257)
testnames <- testnames[1:115]
test <- data.frame(testnames,stringsAsFactors=FALSE)
colnames(test) <- "GRnumber"
#testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
wavs <- unique(FinalAbsDf$Wavelength)
library(USGSHydroOpt)
testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
setwd("C:/Users/jlthomps/Desktop/git/GMIA/")
write.csv(testAbs,file="testAbs.csv")
write.csv(FinalAbsDf,file="FinalAbsDf.csv")


# Open up testAbs.RData workspace and start here

temp <- testAbs$GRnumber
temp2 <- substr(testAbs$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(pattern="_2",temp))+8)
temp3 <- temp
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  #c <- length(b)
  temp3[i] <- b[1]
}

testAbs$date <- temp2
testAbs$ProjectID <- temp3
testAbs$datetime <- strptime(testAbs$date,format="%Y%m%d")
testAbsGMIA <- testAbs[substr(testAbs$ProjectID,1,2) %in% c("OU","Ou","CG","LK","US"),]
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

#Glycol2014 <- read.csv(file="2014Glycol.csv",stringsAsFactors=FALSE)
#Glycol2014$ProjectID <- paste(Glycol2014$Site,Glycol2014$Storm,sep="-")
#dataMerge <- merge(Glycol2014,testAbsOUTCG,by="ProjectID")

library(dataRetrieval)
#OutCOD <- getNWISqwData('040871475','00335','2013-09-30','2014-10-01',expanded=TRUE)
#CgCOD <- getNWISqwData('040871476','00335','2013-09-30','2014-10-01',expanded=TRUE)
#LkCOD <- getNWISqwData('040871488','00335','2013-09-30','2014-10-01',expanded=TRUE)
setwd("C:/Users/jlthomps/Desktop/git/GMIA")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,testAbsWorking,by="ProjectID")

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
dataMerge <- transform(dataMerge,app600=(A617+A620+A623+A626+A629+A632+A635+A638+A641+A644)) 
dataMerge <- transform(dataMerge,app500=(A488+A491+A494+A497+A500+A503+A506+A509))
dataMerge <- transform(dataMerge,app420=(A416+A419+A422+A425+A428+A431+A434))
data_sub <- dataMerge[,c("remark","COD","decYear","app600","app500","app420","A800","A797","A794","A791","A788","A785","A782","A779","A776","A773","A770","A767","A764","A761","A758","A755","A752","A749","A746","A743","A740","A737","A734","A731","A728","A725","A722","A719","A716","A713","A710","A707","A704","A701","A698","A695","A692","A689","A686","A683","A680","A677","A674","A671","A668","A665","A662","A659","A656","A653","A650","A647","A644","A641","A638","A635","A632","A629","A626","A623","A620","A617","A614","A611","A608","A605","A602","A599","A596","A593","A590","A587","A584","A581","A578","A575","A572","A569","A566","A563","A560","A557","A554","A551","A548","A545","A542","A539","A536","A533","A530","A527","A524","A521","A518","A515","A512","A509","A506","A503","A500","A497","A494","A491","A488","A485","A482","A479","A476","A473","A470","A467","A464","A461","A458","A455","A452","A449","A446","A443","A440","A437","A434","A431","A428","A425","A422","A419","A416","A413","A410","A407","A404","A401","A398","A395","A392","A389","A386","A383","A380","A377","A374","A371","A368","A365","A362","A359","A356","A353","A350","A347","A344","A341","A338","A335","A332","A329","A326","A323","A320","A317","A314","A311","A308","A305","A302","A299","A296","A293","A290","A287","A284","A281","A278","A275","A272","A269","A266","A263","A260","A257","A254","A251","A248","A245","A242","A239")]
#data_sub <- dataMerge[,c("remark","COD","decYear","A788","A731","A722","A719","A716","A713","A587","A506","A503","A500","A428","A242","A239")]

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA"
siteNo <- '040871475'
siteINFO <-  getNWISSiteInfo(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CODcens"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")

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
#colnames(steps) <- c("step","BIC","Deviance","Resid.Dev","Resid.Df","Correlation","Slope","RMSE","PRESS","scope","response")


#Save plotSteps to file:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
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

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  

# Probably want to save data at this point:
fileToSave <- paste(pathToSave, "regressionData.csv",sep="/")
write.table(data_sub, fileToSave, row.names=FALSE, sep=",")

