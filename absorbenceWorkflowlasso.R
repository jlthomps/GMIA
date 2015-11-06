setwd("C:/Users/jlthomps/Desktop/git/GMIA")
load("GMIASagFinal.RData")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE,colClasses=c("character","character","numeric","character","numeric","character","character","character","numeric","character","numeric","character","numeric","character","numeric","character","numeric"))
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
dataMerge <- merge(COD2014,GMIASag,by="ProjectID")
# save(dataMerge,file="dataMergeFinal.RData") - file saved 10/7/15

library(GSqwsr)
##regression with no seasonality
dataMerge <- dataMerge[which(!is.na(dataMerge$COD)),]
keepCols <- colnames(dataMerge)
keepCols <- keepCols[-which(keepCols %in% c("ProjectID","Storm","Volume","GRnumber","date","datetime","startDate","endDate"))]
data_sub <- dataMerge[,keepCols]
data_sub$Site <- ifelse(data_sub$Site=='CG',"#009E73",ifelse(data_sub$Site=='LK',"#E69F00",ifelse(data_sub$Site=='OAK',"#0072B2","#CC79A7")))

keepAll <- colnames(data_sub)
keepAll <- keepAll[-which(keepAll %in% c("remark","DOCResult","logCOD","logDOC","COD","rCOD","rBOD","BOD","rFormate","Formate","rAcetate","Acetate","rEGlycol","Eglycol","rPGlycol","Pglycol","SiteAll","SiteCG","SiteLK","SiteOAK","SiteOUT"))]
data_sub_cens <- importQW(data_sub,keep=keepAll,"COD","rCOD","",0.0000002,"User","kg","Unk","","00335","CODcens")
siteName <- "GMIA"
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
predictVariables <- c("A509","A542","A575","A584","A590","A650","A698","Resids490","Sag239_248","Sag281_299","Sag473_491","Sag482_500")

logVariables <- c("A239","A473")
predictString <- paste(predictVariables, collapse = " + ")
logString <- as.character(sapply(paste("log(", logVariables, ")", sep = ""), function(x) x))
logString <- paste(logString, collapse = " + ")
kitchenSink <- paste(predictString, logString, sep = " + ")

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
source("/Users/jlthomps/Desktop/git/GMIA/plotStepsGMIA.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotStepsLasso.pdf",sep=""))
plotStepsGMIA(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeStepsLasso.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_stepsLasso.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Print summary in console:
#source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"SummaryLasso.txt", sep="")
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



