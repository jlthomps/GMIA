library(dataRetrieval)
library(USGSwsBase)
siteNumber <- '040871475'
ParameterCd <- '00060'
StartDate <- '1997-10-01'
EndDate <- '2012-10-01'

# DTMaxCols <- na.omit(data_sub)
# DTMaxRows <- data_sub[,colSums(is.na(data_sub)) <= nrow(data_sub)*0.5] 
# DTMaxRows <- na.omit(DTMaxRows)
# 
# #List columns removed to maximize rows:
# names(data_sub)[!(names(DTMaxCols) %in% names(data_sub))]
# names(data_sub)[!(names(DTMaxRows) %in% names(data_sub))]
# setdiff(data_sub$num,DTMaxRows$num)
# setdiff(data_sub$num,DTMaxCols$num)
# #Choose which to use: DTMaxCols or DTMaxRows:
# DT <- data_sub[,c("TPLoad",names(DTMaxCols))]
# data_sub <- na.omit(DT)
# 
# DT <- data_sub[,c("TPLoad",names(DTMaxRows))]
# data_sub <- na.omit(DT)

# set necessary site information and inputs to step-wise regression
library(USGSwsQWSR)

siteName <- "Outfall_mmprcp_deice"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","mmprcp_deice","kgGlycol"),"BODload","remark","",0.005,"User","tons","Unk","","00310","BODLoading")
investigateResponse <- "BODLoading"
#CODload
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","mmprcp_deice","kgGlycol"),"CODload","remark","",0.005,"User","tons","Unk","","00310","CODLoading")
investigateResponse <- "CODLoading"
#PGload
data_sub <- data_sub[which(!is.na(data_sub$PGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","mmprcp_deice","kgGlycol"),"PGload","remark","",0.005,"User","tons","Unk","","00310","PGLoading")
investigateResponse <- "PGLoading"
#EGload
data_sub <- data_sub[which(!is.na(data_sub$EGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","mmprcp_deice","kgGlycol"),"EGload","remark","",0.005,"User","tons","Unk","","00310","EGLoading")
investigateResponse <- "EGLoading"
#EGPGload
data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","mmprcp_deice","kgGlycol"),"EGPGload","remark","",0.005,"User","tons","Unk","","00310","EGPGLoading")
investigateResponse <- "EGPGLoading"
##################################################
siteName <- "Outfall_noQmax"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"BODload","remark","",0.005,"User","tons","Unk","","00310","BODLoading")
investigateResponse <- "BODLoading"
#CODload
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"CODload","remark","",0.005,"User","tons","Unk","","00310","CODLoading")
investigateResponse <- "CODLoading"
#PGload
data_sub <- data_sub[which(!is.na(data_sub$PGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"PGload","remark","",0.005,"User","tons","Unk","","00310","PGLoading")
investigateResponse <- "PGLoading"
#EGload
data_sub <- data_sub[which(!is.na(data_sub$EGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"EGload","remark","",0.005,"User","tons","Unk","","00310","EGLoading")
investigateResponse <- "EGLoading"
#EGPGload
data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Qmax","Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"EGPGload","remark","",0.005,"User","tons","Unk","","00310","EGPGLoading")
investigateResponse <- "EGPGLoading"
##################################################
siteName <- "Outfall_noQmax_snowmelt"
siteNo <- '040871475'
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
#BODload
data_sub <- data_sub[which(!is.na(data_sub$BODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"BODload","remark","",0.005,"User","tons","Unk","","00310","BODLoading")
investigateResponse <- "BODLoading"
#CODload
data_sub <- data_sub[which(!is.na(data_sub$CODload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"CODload","remark","",0.005,"User","tons","Unk","","00310","CODLoading")
investigateResponse <- "CODLoading"
#PGload
data_sub <- data_sub[which(!is.na(data_sub$PGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"PGload","remark","",0.005,"User","tons","Unk","","00310","PGLoading")
investigateResponse <- "PGLoading"
#EGload
data_sub <- data_sub[which(!is.na(data_sub$EGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"EGload","remark","",0.005,"User","tons","Unk","","00310","EGLoading")
investigateResponse <- "EGLoading"
#EGPGload
data_sub <- data_sub[which(!is.na(data_sub$EGPGload)),]
data_sub <- data_sub[which(!is.na(data_sub$kgGlycol)),]
data_sub_cens <- importQW(data_sub,c("Eduration","mean_temp","max_temp","min_temp","prcp_sum","dwpt_mean","snowdp_sum","kgGlycol"),"EGPGload","remark","",0.005,"User","tons","Unk","","00310","EGPGLoading")
investigateResponse <- "EGPGLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GMIA/",siteName,sep="")

##########################################################
# Preliminary Assessment Plots:
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
colnames(steps) <- c("step","BIC","Deviance","Resid.Dev","Resid.Df","Correlation","Slope","RMSE","PRESS","scope","response")


#Save plotSteps to file:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGLRI(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################
