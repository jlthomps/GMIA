# Deicer runs in //igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data/2014/20141216/
# save as DeicerAbsDf.RData in git/GMIA
setwd("C:/Users/jlthomps/Desktop/git/GMIA/")
load("C:/Users/jlthomps/Desktop/git/GMIA/DeicerAbsDf.RData")

testnames <- colnames(FinalAbsDf)
testnames <- gsub("USGS","Group",testnames)
colnames(FinalAbsDf) <- testnames
testnames <- testnames[1:27]
test <- data.frame(testnames,stringsAsFactors=FALSE)
colnames(test) <- "GRnumber"
wavs <- unique(FinalAbsDf$Wavelength)
library(USGSHydroOpt)
testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
finalcols <- colnames(FinalAbsDf)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("Cl","Ki","UC","Cr","So","Wa"))]
FinalAbsDf <- FinalAbsDf[,finalcols]
FinalAbsDf[FinalAbsDf<0] <- 0

write.csv(FinalAbsDf,file="DeicerFinalAbsDf.csv")
save(FinalAbsDf,file="FinalAbsDf.RData")
temp <- strsplit(colnames(FinalAbsDf),"_")
tempdf <- rbind(temp[[1]],temp[[2]],temp[[3]],temp[[4]],temp[[5]],temp[[6]],temp[[7]],temp[[8]],temp[[9]],temp[[10]],temp[[11]],temp[[12]])
tempdf <- tempdf[,1]

pdf("DeicerPlotsTypeI.pdf")
plot(FinalAbsDf$Wavelength,FinalAbsDf[,2],lwd=1.5,col="hotpink",ylim=c(0,15),type="l",main="Deicer Optical Runs Type I",xlab="Wavelength",ylab="Absorbence")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,5],lwd=1.5,col="green",ylim=c(0,15),type="l",xlab="",ylab="")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,7],lwd=1.5,col="purple",ylim=c(0,15),type="l",xlab="",ylab="")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,9],lwd=1.5,col="turquoise",ylim=c(0,15),type="l",xlab="",ylab="")  
legend("topright",lwd=c(3,3,3,3),col=c("hotpink","green","purple","turquoise"),legend=tempdf[c(2,5,7,9)])
dev.off()

pdf("DeicerPlotsTypeIV.pdf")
plot(FinalAbsDf$Wavelength,FinalAbsDf[,1],lwd=1.5,col="blue",ylim=c(0,30),type="l",main="Deicer Optical Runs Type IV",xlab="Wavelength",ylab="Absorbence")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,3],lwd=1.5,col="orange",ylim=c(0,30),type="l",xlab="",ylab="")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,4],lwd=1.5,col="yellowgreen",ylim=c(0,30),type="l",,xlab="",ylab="")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,8],lwd=1.5,col="red",ylim=c(0,30),type="l",,xlab="",ylab="")  
legend("topright",lwd=c(3,3,3,3),col=c("blue","orange","yellowgreen","red"),legend=tempdf[c(1,3,4,8)])
dev.off()

pdf("DeicerPlotsOther.pdf")
plot(FinalAbsDf$Wavelength,FinalAbsDf[,10],lwd=1.5,col="olivedrab",ylim=c(0,10),type="l",main="Deicer Optical Runs",xlab="Wavelength",ylab="Absorbence")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,11],lwd=1.5,col="coral",ylim=c(0,10),type="l",xlab="",ylab="")  
legend("topright",lwd=c(3,3),col=c("olivedrab","coral"),legend=tempdf[c(10,11)])
dev.off()

pdf("DeicerPlotsMaxflight.pdf")
plot(FinalAbsDf$Wavelength,FinalAbsDf[,6],lwd=1.5,col="yellowgreen",ylim=c(0,125),type="l",main="Deicer Optical Runs Maxflight",xlab="Wavelength",ylab="Absorbence")  
par(new=T)
plot(FinalAbsDf$Wavelength,FinalAbsDf[,12],lwd=1.5,col="navy",ylim=c(0,125),type="l",xlab="",ylab="")  
legend("topright",lwd=c(3,3),col=c("yellowgreen","navy"),legend=tempdf[c(6,12)])
dev.off()
