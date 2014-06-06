# Or load previously saved data
load("C:/Users/jlthomps/Desktop/git/GMIA/dataMerge2.RData")
#load("/Users/jlthomps/GMIA/dataMerge.RData")

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

resids_CG <- data.frame(data_sub$bpdate,modelReturn$RESID)

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
data_sub$RpLev <- ifelse(data_sub$CODrmk=="<",data_sub$CODload,0.05)

data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","OUTCGkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","","RpLev","User","tons","Unk","","00335","CODLoading")
siteName <- "StLukesAllFinal"
siteNo <- '040871488'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "CODLoading"
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

resids_LK <- data.frame(data_sub$bpdate,modelReturn$RESID)

####COD
data_sub <- data_merge[which(substr(data_merge$StormId,1,3)=="OUT"),]
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
data_sub <- data_sub[which(data_sub$CODrmk!=">"),]
data_sub$decYear <- getDecYear(data_sub$bpdate)
data_sub$sinDY <- sin(data_sub$decYear*2*pi)
data_sub$cosDY <- cos(data_sub$decYear*2*pi)
data_sub$RpLev <- ifelse(data_sub$CODrmk=="<",data_sub$CODload,0.05)
#data_subPre <- data_sub[which(data_sub$bpdate<strftime("2000-10-01","%Y-%m-%d")),]
#data_subPost <- data_sub[which(data_sub$bpdate>=strftime("2000-10-01","%Y-%m-%d")),]

data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","snow_depth","GHCNDsnowDepth","mean_temp","max_temp","min_temp","prcp_sum","OUTkgGlycol","decYear","sinDY","cosDY"),"CODload","CODrmk","","RpLev","User","tons","Unk","","00335","CODLoading")
siteName <- "OutfallAllFinal"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
investigateResponse <- "CODLoading"
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

resids_OUT <- data.frame(data_sub$bpdate,modelReturn$RESID)

residsCGEarly <- resids_CG$modelReturn.RESID[which(resids_CG$data_sub.bpdate<strptime("2002-10-01","%Y-%m-%d"))]
residsCGLate <- resids_CG$modelReturn.RESID[which(resids_CG$data_sub.bpdate>=strptime("2006-10-01","%Y-%m-%d"))]
residsLKEarly <- resids_LK$modelReturn.RESID[which(resids_LK$data_sub.bpdate<strptime("2002-10-01","%Y-%m-%d"))]
residsLKLate <- resids_LK$modelReturn.RESID[which(resids_LK$data_sub.bpdate>=strptime("2006-10-01","%Y-%m-%d"))]
residsOUTEarly <- resids_OUT$modelReturn.RESID[which(resids_OUT$data_sub.bpdate<strptime("2002-10-01","%Y-%m-%d"))]
residsOUTLate <- resids_OUT$modelReturn.RESID[which(resids_OUT$data_sub.bpdate>=strptime("2006-10-01","%Y-%m-%d"))]

residsList <- list(residsCGEarly,residsCGLate,residsLKEarly,residsLKLate,residsOUTEarly,residsOUTLate)
max_length <- max(sapply(residsList,length))
residsBoxPlot <- sapply(residsList,function(x){c(x,rep(NA,max_length-length(x)))})
residsdf <- data.frame(residsBoxPlot)
colnames(residsdf) <- c("residsCGEarly","residsCGLate","residsLKEarly","residsLKLate","residsOUTEarly","residsOUTLate")
pdf(fileName <- paste(pathToSave,"/","AllSitesFinal","ResidualsBoxPlot.pdf",sep=""))
boxplot(residsdf,ylab="Model residuals",main="Residuals for COD regression models for all sites split into WY 1997-2002 and WY 2006-2011",las=2,at=c(1,2,4,5,7,8),par(mar=c(12,5,4,2)+0.1))
dev.off()

write.table(residsdf,file=paste(pathToSave,"/",siteName,investigateResponse,"boxplotData.txt",sep=""))
