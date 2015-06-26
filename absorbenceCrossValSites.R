# Cross-validation of COD regression using 3/4 sites
load("FinalAbsData.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="CG"),]

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","randNum","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsCG.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsCG.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsCG.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryCG.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##regression with no seasonality
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="CG"),]
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsNoSeasonCG.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsNoSeasonCG.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsNoSeasonCG.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryNoSeasonCG.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

# Cross-validation of COD regression using 3/4 sites
load("FinalAbsData.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="OUT"),]

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","randNum","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsOUT.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsOUT.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsOUT.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryOUT.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##regression with no seasonality
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="OUT"),]
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsNoSeasonOUT.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsNoSeasonOUT.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsNoSeasonOUT.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryNoSeasonOUT.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

# Cross-validation of COD regression using 3/4 sites
load("FinalAbsData.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="LK"),]

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","randNum","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsLK.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsLK.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsLK.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryLK.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##regression with no seasonality
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="LK"),]
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsNoSeasonLK.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsNoSeasonLK.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsNoSeasonLK.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryNoSeasonLK.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

# Cross-validation of COD regression using 3/4 sites
load("FinalAbsData.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="OAK"),]

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","randNum","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsOAK.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsOAK.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsOAK.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryOAK.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##regression with no seasonality
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(dataMerge$Site!="OAK"),]
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","COD","DOCResult","logCOD","logDOC"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","remark","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA_crossVal"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
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
kitchenSink <- createFullFormula(data_sub_cens[,-1],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsNoSeasonOAK.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsNoSeasonOAK.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsNoSeasonOAK.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryNoSeasonOAK.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

