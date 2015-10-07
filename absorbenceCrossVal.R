# Cross-validation of COD regression using 5 random groups
setwd("C:/Users/jlthomps/Desktop/git/GMIA")
load("GMIASagFinal.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE,colClasses=c("character","character","numeric","character","numeric","character","character","character","numeric","character","numeric","character","numeric","character","numeric","character","numeric"))
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
numRow <- nrow(dataMerge)
randNum <- sample(1:5,numRow,replace=T)
dataMerge$randNum <- randNum
exclNum <- sample(1:5,1)
dataMerge <- dataMerge[which(dataMerge$randNum!=exclNum),]

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","DOCResult","logCOD","logDOC","COD","rCOD","rBOD","BOD","rFormate","Formate","rAcetate","Acetate","rEGlycol","Eglycol","rPGlycol","Pglycol","SiteAll","SiteCG","SiteLK","SiteOAK","SiteOUT"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","rCOD","",0.0000002,"User","kg","Unk","","00335","CODcens")
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
predictVariables <- predictVariables[which(predictVariables != "Site")]

logVariables <- names(which(sapply(data_sub_cens[, which(names(data_sub_cens) %in% predictVariables)], function(x) min(as.numeric(x), na.rm = TRUE)) > 0))
predictString <- paste(setdiff(predictVariables,logVariables[which(substr(logVariables,1,1)=="A")]), collapse = " + ")
logString <- as.character(sapply(paste("log(", logVariables[which(substr(logVariables,1,1)=="A")], ")", sep = ""), function(x) x))
logString <- paste(logString, collapse = " + ")
kitchenSink <- paste(predictString, logString, sep = " + ")

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
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
fileName <- paste(pathToSave,"/", investigateResponse,"Summary.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##regression with no seasonality
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
numRow <- nrow(dataMerge)
randNum <- sample(1:5,numRow,replace=T)
dataMerge$randNum <- randNum
exclNum <- sample(1:5,1)
dataMerge <- dataMerge[which(dataMerge$randNum!=exclNum),]
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","DOCResult","logCOD","logDOC","COD","rCOD","rBOD","BOD","rFormate","Formate","rAcetate","Acetate","rEGlycol","Eglycol","rPGlycol","Pglycol","SiteAll","SiteCG","SiteLK","SiteOAK","SiteOUT"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","rCOD","",0.0000002,"User","kg","Unk","","00335","CODcens")

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
predictVariables <- predictVariables[which(predictVariables != "Site")]

logVariables <- names(which(sapply(data_sub_cens[, which(names(data_sub_cens) %in% predictVariables)], function(x) min(as.numeric(x), na.rm = TRUE)) > 0))
predictString <- paste(setdiff(predictVariables,logVariables[which(substr(logVariables,1,1)=="A")]), collapse = " + ")
logString <- as.character(sapply(paste("log(", logVariables[which(substr(logVariables,1,1)=="A")], ")", sep = ""), function(x) x))
logString <- paste(logString, collapse = " + ")
kitchenSink <- paste(predictString, logString, sep = " + ")

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsNoSeason.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsNoSeason.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsNoSeason.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryNoSeason.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

