
setwd("/Users/jlthomps/Desktop/git/GMIA")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
DOC2014 <- c("030414.csv","050714.csv","090814.csv","03182014.csv","04242014.csv","20141201b.csv","20141210.csv","lenaker_041214.csv","Lenaker012214.csv","12232013.csv","lenaker121113.csv")
FolderName="GMIA_Corsi-Lenaker"
DfMerge<-COD2014
DfMergeSamps='ProjectID'
for (i in 1:length(DOC2014)) {
  FileName=DOC2014[i]
  FilePath <- paste('//Igsarmewwsscu/SCUData/TOC_REPORTS',FolderName,FileName,sep='/')
  
  DOC <- read.csv(FilePath,stringsAsFactors=FALSE)
  Blankrows <- grep('blanks',DOC[,'SampleName'])
  Blankrows <- c(grep('blank',DOC[,'SampleName']),Blankrows)
  Blankrows <- c(grep('BLANK',DOC[,'SampleName']),Blankrows)
  
  TwentyCheckrows <- grep('20 ppm test',DOC[,'SampleName'])
  TwentyCheckrows <- c(grep('20PPMCHECK',DOC[,'SampleName']),TwentyCheckrows)
  TwentyCheckrows <- c(grep('20 PPM CHECK',DOC[,'SampleName']),TwentyCheckrows)
  
  OneCheckrows <- grep('1 PPM CHECK',DOC[,'SampleName'])
  OneCheckrows <- c(grep('1 ppmtest',DOC[,'SampleName']),OneCheckrows)
  
  Unknownrows <- grep('unknown',DOC[,'SampleName'])
  
  Standardrows <- grep('standards',DOC[,'SampleName'])
  Standardrows <- c(grep('high stds',DOC[,'SampleName']),Standardrows)
  
  AllQA <- sort(c(Blankrows,TwentyCheckrows,OneCheckrows,Unknownrows,Standardrows))
  Df <- DOC[-c(AllQA),]
  Df[,'MeanConc.'] <- as.numeric (Df[,'MeanConc.'])
  DfNew <- data.frame(SampleName=as.character(),DOCResult=as.numeric()) 
  
  if (length(unique(Df[,'SampleName']))>0) {
  for(j in 1:length(unique(Df[,'SampleName']))){
    SampName <- unique(Df[,'SampleName'])[j]
    TempDf <- Df[which(Df$SampleName==SampName),]
    DfNew[,'SampleName'] <- as.character(DfNew[,'SampleName'])
    DfNew[j,'SampleName'] <- SampName
    DfNew[j,'DOCResult'] <- mean(TempDf[,'MeanConc.'])
  }
  }
  
  SummarySamps <- DfMerge[,DfMergeSamps]
  DOCSamps <- DfNew[,'SampleName']
  
  nCom <- which(SummarySamps %in% DOCSamps)
  
  
  if (length(nCom)>0) {
  for (k in 1:length(nCom)){
    DfMerge[nCom[k],'DOCResult'] <- DfNew[,'DOCResult'][which(DfNew[,'SampleName']==DfMerge[nCom[k],DfMergeSamps])]
  }
  DfFinal <- DfMerge
  }
}

DfFinal <- DfFinal[which(!is.na(DfFinal$COD) & !is.na(DfFinal$DOCResult)),]
DfFinal$Site <- ifelse(DfFinal$Site=='CG',2,ifelse(DfFinal$Site=='LK',1,ifelse(DfFinal$Site=='OAK',3,4)))
DfFinal$logCOD <- log10(DfFinal$COD)
DfFinal$logDOC <- log10(DfFinal$DOCResult)

pdf("CODvsDOC.pdf")
parOriginal <- par(no.readonly = TRUE)

plot(DfFinal$logDOC,DfFinal$logCOD,pch=DfFinal$Site,type="p",main="COD vs DOC for Runoff Events at GMIA USGS sites, 2013-2014",xlab="log(DOC)",ylab="log(COD)")  
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC)",sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(1,2,3,4))
par(parOriginal)
plot(DfFinal$DOCResult,DfFinal$COD,pch=DfFinal$Site,type="p",main="COD vs DOC for Runoff Events at GMIA USGS sites, 2013-2014",xlab="DOC",ylab="COD")  
lmFormula <- "COD ~ DOCResult"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
abline(lmfit, col="red")
mtext(paste("COD = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"DOC",sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(1,2,3,4))
par(parOriginal)
dev.off()
