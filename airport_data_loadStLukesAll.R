
# Run hourly_daily_checks.R
#source('C:/Users/jlthomps/Desktop/git/GMIA/hourly_daily_checks.R')
# Run disch_iceaffect_omit.R or disch_iceaffect_corr.R
#source('C:/Users/jlthomps/Desktop/git/GMIA/disch_iceaffect_corr.R')
# Run data_merge_hourly.R
#source('C:/Users/jlthomps/Desktop/git/GMIA/data_merge_hourly.R')

# Or load previously saved data
#load("~/GMIA/GMIAData.RData")

####BOD
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="LK"),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)
data_sub$OUTCGkgGlycol <- data_sub$OUTkgGlycol+data_sub$CGkgGlycol

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTCGkgGlycol)),]
data_sub <- data_sub[which(data_sub$BODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$BODrmk=="<",data_sub$BODload,0.05)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","",0.005,"User","tons","Unk","","00310","BODLoading")
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","","RpLev","User","tons","Unk","","00310","BODLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("C:/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")
write.table(data_sub,file=paste(pathToSave,"/",siteName,investigateResponse,"Data.txt",sep=""))

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_InitialQQGraphs.pdf",sep=""))
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
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_BOD <- data.frame(data_sub$bpdate,modelReturn$RESID)


####COD
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="LK"),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)
data_sub$OUTCGkgGlycol <- data_sub$OUTkgGlycol+data_sub$CGkgGlycol

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTCGkgGlycol)),]
data_sub <- data_sub[which(data_sub$CODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","",0.005,"User","tons","Unk","","00335","CODLoading")
data_sub$RpLev <- ifelse(data_sub$CODrmk=="<",data_sub$CODload,0.05)

data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","","RpLev","User","tons","Unk","","00335","CODLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "CODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

#pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")
write.table(data_sub,file=paste(pathToSave,"/",siteName,investigateResponse,"Data.txt",sep=""))

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_InitialQQGraphs.pdf",sep=""))
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
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_COD <- data.frame(data_sub$bpdate,modelReturn$RESID)

####EGPG
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="LK"),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)
data_sub$OUTCGkgGlycol <- data_sub$OUTkgGlycol+data_sub$CGkgGlycol

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#EGPGload
data_sub$EGload <- ifelse(data_sub$EGrmk=="<",0,data_sub$EGload)
data_sub$EGPGload <- data_sub$EGload+data_sub$PGload
data_sub$EGPGrmk <- data_sub$PGrmk
data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTCGkgGlycol)),]
data_sub <- data_sub[which(data_sub$EGPGrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$EGPGrmk=="<",data_sub$EGPGload,0.05)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"EGPGload","EGPGrmk","",0.005,"User","tons","Unk","","91080","EGPGLoading")
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"EGPGload","EGPGrmk","","RpLev","User","tons","Unk","","91080","EGPGLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "EGPGLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")
#pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")
write.table(data_sub,file=paste(pathToSave,"/",siteName,investigateResponse,"Data.txt",sep=""))

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_InitialQQGraphs.pdf",sep=""))
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
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_EGPG<- data.frame(data_sub$bpdate,modelReturn$RESID)

pdf(fileName <- paste(pathToSave,"/","AllPcodes","Residuals.pdf",sep=""))
pdf(fileName <- paste(pathToSave,"/","StLukesAllPcodes","Residuals.pdf",sep=""))
par(mfrow=c(2,2))
plot(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"BOD residuals .1",sep=" "))
lines(lowess(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,f=0.1),col="blue")
plot(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"BOD residuals .2",sep=" "))
lines(lowess(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,f=0.2),col="blue")
plot(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"BOD residuals .3",sep=" "))
lines(lowess(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,f=0.3),col="blue")
plot(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"BOD residuals .4",sep=" "))
lines(lowess(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,f=0.4),col="blue")
par(mfrow=c(2,2))
plot(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"COD residuals .1",sep=" "))
lines(lowess(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,f=0.1),col="blue")
plot(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"COD residuals .2",sep=" "))
lines(lowess(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,f=0.2),col="blue")
plot(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"COD residuals .3",sep=" "))
lines(lowess(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,f=0.3),col="blue")
plot(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"COD residuals .4",sep=" "))
lines(lowess(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,f=0.4),col="blue")
par(mfrow=c(2,2))
plot(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"EGPG residuals .1",sep=" "))
lines(lowess(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,f=0.1),col="blue")
plot(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"EGPG residuals .2",sep=" "))
lines(lowess(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,f=0.2),col="blue")
plot(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"EGPG residuals .3",sep=" "))
lines(lowess(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,f=0.3),col="blue")
plot(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"EGPG residuals .4",sep=" "))
lines(lowess(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,f=0.4),col="blue")
dev.off()













dev.off()

data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="LK"),]
data_sub$EGload <- ifelse(data_sub$EGrmk=="<",0,data_sub$EGload)
data_sub$EGPGload <- data_sub$EGload+data_sub$PGload
data_sub$colorPG <- ifelse(data_sub$PGrmk=="<","red","blue")
data_sub$colorEG <- ifelse(data_sub$EGrmk=="<","red","blue")
pdf(fileName <- paste(pathToSave,"/","StLukes","CODvsEGPG.pdf",sep=""))
plot(data_sub$EGPGload,data_sub$CODload,xlab="EGPG",ylab="COD",col=data_sub$colorPG,type="p",main=paste(siteName,"EGPG vs COD",sep=" "))
plot(data_sub$EGload,data_sub$CODload,xlab="EG",ylab="COD",col=data_sub$colorEG,type="p",main=paste(siteName,"EGPG vs COD",sep=" "))
plot(data_sub$PGload,data_sub$CODload,xlab="PG",ylab="COD",col=data_sub$colorPG,type="p",main=paste(siteName,"EGPG vs COD",sep=" "))
dev.off()

data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="LK"),]
for (i in 1:nrow(data_sub)) {
  data_sub$TheorCOD[i] <- sum((as.numeric(data_sub$EGconc[i])*(1.28/1.1)),(as.numeric(data_sub$PGconc[i])*(1.61/1.04)),(as.numeric(data_sub$ACconc[i])*(1.05/1.28)),(as.numeric(data_sub$FMconc[i])*.373),na.rm=TRUE)
}
pdf(fileName <- paste(pathToSave,"/","StLukes","CODvsTheorCOD.pdf",sep=""))
plot(as.numeric(data_sub$CODconc),data_sub$TheorCOD,xlab="COD",ylab="Theoretical COD",col="blue",type="p",main=paste(siteName,"COD vs Theoretical COD",sep=" "))
dev.off()