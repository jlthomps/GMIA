library(dataRetrieval)
library(USGSwsBase)
siteNumber <- '040871475'
ParameterCd <- '00060'
StartDate <- '1997-10-01'
EndDate <- '2012-10-01'

# Run hourly_daily_checks.R
source('C:/Users/jlthomps/Desktop/git/GMIA/hourly_daily_checks.R')
# Run disch_iceaffect_omit.R or disch_iceaffect_corr.R
source('C:/Users/jlthomps/Desktop/git/GMIA/disch_iceaffect_corr.R')
# Run data_merge_hourly.R
source('C:/Users/jlthomps/Desktop/git/GMIA/data_merge_hourly.R')

# Or load previously saved data
load("~/GMIA/GMIAData.RData")

####Pre
data_sub <- data_merge
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
data_sub <- data_sub[which(data_sub$BODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_subPre,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","",0.005,"User","tons","Unk","","00310","BODLoading")
siteName <- "OutfallPre2000"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

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
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
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

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
cat("E/O bias: ",EO_bias)
sink()

#####################################################


####Post
data_sub_cens <- importQW(data_subPost,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","",0.005,"User","tons","Unk","","00310","BODLoading")
siteName <- "OutfallPost2000"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

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

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

####Pre
data_sub <- data_merge
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
data_sub <- data_sub[which(data_sub$BODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_subPre,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","",0.005,"User","tons","Unk","","00310","BODLoading")
siteName <- "OutfallPre2000"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

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
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
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

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_56 <- data.frame(data_sub$bpdate,modelReturn$RESID)
pdf(fileName <- paste(pathToSave,"/",investigateResponse,"allResiduals.pdf",sep=""))
par(mfrow=c(2,2))
plot(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,xlab="Datetime",ylab="BOD Model Residuals",col="red",type="p",main=paste(siteName,"residuals .1",sep=" "))
lines(lowess(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,f=0.1),col="blue")
plot(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,xlab="Datetime",ylab="BOD Model Residuals",col="red",type="p",main=paste(siteName,"residuals .2",sep=" "))
lines(lowess(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,f=0.2),col="blue")
plot(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,xlab="Datetime",ylab="BOD Model Residuals",col="red",type="p",main=paste(siteName,"residuals .3",sep=" "))
lines(lowess(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,f=0.3),col="blue")
plot(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,xlab="Datetime",ylab="BOD Model Residuals",col="red",type="p",main=paste(siteName,"residuals .4",sep=" "))
lines(lowess(resids_56$data_sub.bpdate,resids_56$modelReturn.RESID,f=0.4),col="blue")
dev.off()
