library(dataRetrieval)
library(USGSwsBase)
siteNumber <- '040871488'
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
#load("~/GMIA/GMIAData.RData")

####BOD
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
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$BODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","CGkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","",0.005,"User","tons","Unk","","00310","BODLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("C:/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

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
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_BOD <- data.frame(data_sub$bpdate,modelReturn$RESID)


####COD
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
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$CODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","CGkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","",0.005,"User","tons","Unk","","00335","CODLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "CODLoading"
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

resids_COD <- data.frame(data_sub$bpdate,modelReturn$RESID)

####PG
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
data_sub <- data_sub[which(!is.na(data_sub$PGload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$PGrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","CGkgGlycol","decYear","sinDY","cosDY"),"PGload","PGrmk","",0.005,"User","tons","Unk","","91080","PGLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "PGLoading"
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

resids_PG <- data.frame(data_sub$bpdate,modelReturn$RESID)

####PG
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
data_sub <- data_sub[which(!is.na(data_sub$EGload)),]
data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$EGrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","CGkgGlycol","decYear","sinDY","cosDY"),"EGload","EGrmk","",0.005,"User","tons","Unk","","91075","EGLoading")
siteName <- "StLukesAll"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "EGLoading"
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

resids_EG <- data.frame(data_sub$bpdate,modelReturn$RESID)


# ####EGPG
# data_sub <- data_merge
# #data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
# #data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
# data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
# data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
# data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
# data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)
# 
# # set necessary site information and inputs to step-wise regression
# library(GSqwsr)
# #EGPGload
# data_sub$EGPGload <- data_sub$EGload+data_sub$PGload
# data_sub$EGPGrmk <- pmax(data_sub$EGrmk,data_sub$PGrmk,na.rm=TRUE)
# data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
# data_sub <- data_sub[which(!is.na(data_sub$OUTkgGlycol)),]
#data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
# data_sub <- data_sub[which(data_sub$EGPGrmk!=">"),]
# data_sub$decYear <- getDecYear(data_sub$bpdate)
# data_sub$sinDY <- sin(data_sub$decYear*2*pi)
# data_sub$cosDY <- cos(data_sub$decYear*2*pi)
# #data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
# #data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
# #data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
# data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","CGkgGlycol","decYear","sinDY","cosDY"),"EGPGload","EGPGrmk","",0.005,"User","tons","Unk","","91080","EGPGLoading")
#siteName <- "StLukesAll"
#siteNo <- '040871488'
# siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
# investigateResponse <- "EGPGLoading"
# # choose 'normal' or 'lognormal' distribution for data
# transformResponse <- "lognormal"
# 
# pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")
# 
# ##########################################################
# # Preliminary Assessment Plots:
# # pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
# pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
# plotQQTransforms(data_sub_cens,investigateResponse)
# predictVariableScatterPlots(data_sub_cens,investigateResponse)
# dev.off()
# ##########################################################
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
# #colnames(steps) <- c("step","BIC","Deviance","Resid.Dev","Resid.Df","Correlation","Slope","RMSE","PRESS","scope","response")
# 
# 
# #Save plotSteps to file:
# # source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# # source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
# pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
# plotSteps(steps,data_sub_cens,transformResponse)
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
# # Plot summary plots:
# pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
# resultPlots(data_sub_cens,modelReturn,siteINFO)
# dev.off()
# 
# pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
# resultResidPlots(data_sub_cens,modelReturn,siteINFO)
# dev.off()
# #####################################################
# 
# #####################################################
# # Print summary in console:
# fileName <- paste(pathToSave,"/",investigateResponse,"Summary_2.txt", sep="")
# 
# sink(fileName)
# summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
# #EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
# #cat("E/O bias: ",EO_bias)
# sink()
# 
# #####################################################
# 
# resids_EGPG<- data.frame(data_sub$bpdate,modelReturn$RESID)
resids_EG$param <- "blue"
resids_BOD$param <- "red"
resids_COD$param <- "green"
resids_PG$param <- "pink"
residsAll <- rbind(resids_EG,resids_COD,resids_BOD,resids_PG)

pdf(fileName <- paste(pathToSave,"/","AllPcodes","Residuals.pdf",sep=""))
par(mfrow=c(2,2))
plot(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col=residsAll$param,type="p",main=paste(siteName,"residuals .1",sep=" "))
lines(lowess(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,f=0.1),col="blue")
plot(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col=residsAll$param,type="p",main=paste(siteName,"residuals .2",sep=" "))
lines(lowess(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,f=0.2),col="blue")
plot(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col=residsAll$param,type="p",main=paste(siteName,"residuals .3",sep=" "))
lines(lowess(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,f=0.3),col="blue")
plot(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col=residsAll$param,type="p",main=paste(siteName,"residuals .4",sep=" "))
lines(lowess(residsAll$data_sub.bpdate,residsAll$modelReturn.RESID,f=0.4),col="blue")
legend("center",c("BOD","COD","EG","PG"),pch=c(1,1,1,1),col=c("red","green","blue","pink"))
dev.off()