# Or load previously saved data
load("C:/Users/jlthomps/Desktop/git/GMIA/dataMerge2.RData")
#load("/Users/jlthomps/GMIA/dataMerge.RData")

####BOD
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="CG"),]
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
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$BODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$BODrmk=="<",data_sub$BODload,0.05)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","CGkgGlycol","decYear","sinDY","cosDY"),"BODload","BODrmk","","RpLev","User","tons","Unk","","00310","BODLoading")
siteName <- "CargoAllFinal"
siteNo <- '040871476'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "BODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("C:/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

##########################################################
# Import model parameters from csv file if desired:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(data_sub_cens,investigateResponse,newFormula,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
##########################################################

#Save plotSteps to file:
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_BOD <- data.frame(data_sub$bpdate,modelReturn$RESID)


####COD
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="CG"),]
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
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$CODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$CODrmk=="<",data_sub$CODload,0.05)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","CGkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","","RpLev","User","tons","Unk","","00335","CODLoading")
siteName <- "CargoAllFinal"
siteNo <- '040871476'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "CODLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

#pathToSave <- paste("C:/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

##########################################################
# Import model parameters from csv file if desired:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(data_sub_cens,investigateResponse,newFormula,
                                  "BIC", #Other option is "AIC"
                                  transformResponse)

steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
##########################################################

#Save plotSteps to file:
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_COD <- data.frame(data_sub$bpdate,modelReturn$RESID)

# ####EGPG
data_sub <- data_merge[which(substr(data_merge$StormId,1,2)=="CG"),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE,invert=TRUE),]
#data_sub <- data_sub[grep("melt",data_sub$prcp_desc,fixed=TRUE),]
data_sub$OUTgalGlycol <- as.numeric(data_sub$OUTgalGlycol)
data_sub$OUTkgGlycol <- as.numeric(data_sub$OUTkgGlycol)
data_sub$CGgalGlycol <- as.numeric(data_sub$CGgalGlycol)
data_sub$CGkgGlycol <- as.numeric(data_sub$CGkgGlycol)

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
#EGPGload
data_sub$EGload <- ifelse(data_sub$EGrmk=="<",0,data_sub$EGload)
data_sub$EGPGload <- data_sub$EGload+data_sub$PGload
data_sub$EGPGrmk <- data_sub$PGrmk
data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
data_sub <- data_sub[which(!is.na(data_sub$CGkgGlycol)),]
data_sub <- data_sub[which(data_sub$EGPGrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$EGPGrmk=="<",data_sub$EGPGload,0.05)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]
#data_sub$BODrmk <- ifelse(data_sub$BODrmk==">","",data_sub$BODrmk)
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","CGkgGlycol","decYear","sinDY","cosDY"),"EGPGload","EGPGrmk","","RpLev","User","tons","Unk","","91080","EGPGLoading")
siteName <- "CargoAllFinal"
siteNo <- '040871476'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "EGPGLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

#pathToSave <- paste("C:/Users/jlthomps/Documents/R/GMIA_hourly/",siteName,sep="")

##########################################################
# Import model parameters from csv file if desired:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(data_sub_cens,investigateResponse,newFormula,
                                  "BIC", #Other option is "AIC"
                                  transformResponse)

steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
##########################################################

#Save plotSteps to file:
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
# source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",siteName,investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",siteName,investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",siteName,investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
#EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
#cat("E/O bias: ",EO_bias)
sink()

#####################################################

resids_EGPG <- data.frame(data_sub$bpdate,modelReturn$RESID)

pdf(fileName <- paste(pathToSave,"/","CargoAllFinal","Residuals.pdf",sep=""))
plot(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"BOD residuals .1",sep=" "))
lines(lowess(resids_BOD$data_sub.bpdate,resids_BOD$modelReturn.RESID,f=0.4),col="blue")
plot(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"COD residuals .1",sep=" "))
lines(lowess(resids_COD$data_sub.bpdate,resids_COD$modelReturn.RESID,f=0.4),col="blue")
plot(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,xlab="Datetime",ylab="Model Residuals",col="red",type="p",main=paste(siteName,"EGPG residuals .1",sep=" "))
lines(lowess(resids_EGPG$data_sub.bpdate,resids_EGPG$modelReturn.RESID,f=0.4),col="blue")
dev.off()

residsEGPG02 <- resids_EGPG$modelReturn.RESID[which(resids_EGPG$data_sub.bpdate<=strptime("2002-10-01","%Y-%m-%d"))]
residsEGPG05 <- resids_EGPG$modelReturn.RESID[which(resids_EGPG$data_sub.bpdate>strptime("2002-10-01","%Y-%m-%d")&resids_EGPG$data_sub.bpdate<=strptime("2005-10-01","%Y-%m-%d"))]
residsEGPG07 <- resids_EGPG$modelReturn.RESID[which(resids_EGPG$data_sub.bpdate>strptime("2005-10-01","%Y-%m-%d"))]
residsBOD02 <- resids_BOD$modelReturn.RESID[which(resids_BOD$data_sub.bpdate<=strptime("2002-10-01","%Y-%m-%d"))]
residsBOD05 <- resids_BOD$modelReturn.RESID[which(resids_BOD$data_sub.bpdate>strptime("2002-10-01","%Y-%m-%d")&resids_BOD$data_sub.bpdate<=strptime("2005-10-01","%Y-%m-%d"))]
residsBOD07 <- resids_BOD$modelReturn.RESID[which(resids_BOD$data_sub.bpdate>strptime("2005-10-01","%Y-%m-%d"))]
residsCOD02 <- resids_COD$modelReturn.RESID[which(resids_COD$data_sub.bpdate<=strptime("2002-10-01","%Y-%m-%d"))]
residsCOD05 <- resids_COD$modelReturn.RESID[which(resids_COD$data_sub.bpdate>strptime("2002-10-01","%Y-%m-%d")&resids_COD$data_sub.bpdate<=strptime("2005-10-01","%Y-%m-%d"))]
residsCOD07 <- resids_COD$modelReturn.RESID[which(resids_COD$data_sub.bpdate>strptime("2005-10-01","%Y-%m-%d"))]
residsList <- list(residsEGPG02,residsEGPG05,residsEGPG07,residsBOD02,residsBOD05,residsBOD07,residsCOD02,residsCOD05,residsCOD07)
max_length <- max(sapply(residsList,length))
residsBoxPlot <- sapply(residsList,function(x){c(x,rep(NA,max_length-length(x)))})
residsdf <- data.frame(residsBoxPlot)
colnames(residsdf) <- c("residsEGPG02","residsEGPG05","residsEGPG07","residsBOD02","residsBOD05","residsBOD07","residsCOD02","residsCOD05","residsCOD07")
pdf(fileName <- paste(pathToSave,"/","CargoAllFinal","ResidualsBoxPlot.pdf",sep=""))
boxplot(residsdf,ylab="Model residual",main="Residuals for Cargo split into groups at 10/01/02 and 10/01/05",col=c("red","blue","green","red","blue","green","red","blue","green"),las=2,at=c(1,2,3,5,6,7,9,10,11),par(mar=c(12,5,4,2)+0.1))
dev.off()
