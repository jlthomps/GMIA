
setwd("/Users/jlthomps/Desktop/git/GMIA")
load("FinalAbsData.RData")
load("CODDOCMerge.RData")
DfFinal$ProjectID <- paste(DfFinal$Site,DfFinal$Storm,sep="-")
dataMerge <- merge(DfFinal,GMIASag,by="ProjectID")

library(GSqwsr)
dataMerge <- dataMerge[which(!is.na(dataMerge$DOCResult)),]
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
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","DOCResult","logCOD","logDOC","COD","Acetate","rEGlycol","Eglycol","rPGlycol","Pglycol","SiteAll","SiteCG","SiteLK","SiteOAK","SiteOUT"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"DOCResult","remark","",0.0000002,"User","kg","Unk","","00681","DOCcens")
siteName <- "GMIAdoc"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
# name of value column in data_sub_cens object
investigateResponse <- "DOCcens"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
kitchenSink <- createFullFormula(data_sub_cens[,-c(1:7)],investigateResponse)

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
fileName <- paste(pathToSave,"/", investigateResponse,"Summary.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

# ##########################################################
# # Import model parameters from csv file if desired:
# pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
# choicesNew <- read.csv(pathToParam)
# newFormula <-createFormulaFromDF(choicesNew)
# ##########################################################
# 
# ##########################################################
# # Or, don't do the stepwise regression, just get the model coefficients using csv file:
# modelReturn <- censReg(paste(investigateResponse," ~ ", newFormula, sep=""), dist=transformResponse, data=data_sub_cens)
# #####################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################

##regression with no seasonality
dataMerge <- merge(DfFinal,GMIASag,by="ProjectID")
dataMerge <- dataMerge[which(!is.na(dataMerge$DOCResult)),]
dataMerge$remark <- ""
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","DOCResult","logCOD","logDOC","COD","Acetate","rEGlycol","Eglycol","rPGlycol","Pglycol","SiteAll","SiteCG","SiteLK","SiteOAK","SiteOUT"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"DOCResult","remark","",0.0000002,"User","kg","Unk","","00681","DOCcens")

siteName <- "GMIAdoc"
siteNo <- '040871475'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- "General Mitchell Airport sites @ Milwaukee WI"
# name of value column in data_sub_cens object
investigateResponse <- "DOCcens"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/",siteName,sep="")

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
kitchenSink <- createFullFormula(data_sub_cens[,-c(1:7)],investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
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

# ##########################################################
# # Generate a csv file to customize model parameters (can do without running kitchen sink):
# choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
# ##########################################################
# 
# ##########################################################
# # Import model parameters from csv file if desired:
# pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
# choicesNew <- read.csv(pathToParam)
# newFormula <-createFormulaFromDF(choicesNew)
# ##########################################################
# 
# ##########################################################
# # Or, don't do the stepwise regression, just get the model coefficients using csv file:
# modelReturn <- censReg(paste(investigateResponse," ~ ", newFormula, sep=""), dist=transformResponse, data=data_sub_cens)
# #####################################################
# 
# #####################################################
# # Print summary in console:
# #source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
# fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
# summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
# #####################################################



