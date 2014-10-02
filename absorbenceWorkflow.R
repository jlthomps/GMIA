data <- formatAbsSamples(dateLower='20130930',dateUpper='20140918',Type='All',Project='GMIA')
# had to do some fooling around in function b/c of missing files in 20140121
# also added Project ID to name so can differentiate OUT/LK/CG

dateRangeFiles<-dateRangeFiles[c(1:21,23:73)]
dateLower='20130930'
dateUpper='20140918'
Type='All'
Project='GMIA'


testnames <- colnames(FinalAbsDf)
testnames <- gsub("USGS","Group",testnames)
colnames(FinalAbsDf) <- testnames
#wavs <- c(251,254,257)
testnames <- testnames[1:93]
test <- data.frame(testnames,stringsAsFactors=FALSE)
colnames(test) <- "GRnumber"
#testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
wavs <- unique(FinalAbsDf$Wavelength)
testAbs <- getAbs(FinalAbsDf,"Wavelength",wavs,"Group",test,"GRnumber")
write.csv(testAbs,file="testAbs.csv")
write.csv(FinalAbsDf,file="FinalAbsDf.csv")


# Open up testAbs.RData workspace and start here

temp <- testAbs$GRnumber
temp2 <- substr(testAbs$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(pattern="_2",temp))+8)
temp3 <- temp
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  c <- length(b)
  temp3[i] <- b[c]
}

testAbs$date <- temp2
testAbs$ProjectID <- temp3
testAbs$datetime <- strptime(testAbs$date,format="%Y%m%d")
testAbsOUT <- testAbs[grep("OUT-",testAbs$ProjectID),]
testAbsCG <- testAbs[grep("CG-",testAbs$ProjectID),]
testAbsLK <- testAbs[grep("LK-",testAbs$ProjectID),]

Glycol2014 <- read.csv(file="2014Glycol.csv",stringsAsFactors=FALSE)
Glycol2014$ProjectID <- paste(Glycol2014$Site,Glycol2014$Storm,sep="-")
dataMerge <- merge(Glycol2014,testAbsOUT,by="ProjectID")
library(GSqwsr)
dataMerge$decYear <- getDecYear(dataMerge$datetime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge$remark <- ""
dataMerge <- transform(dataMerge,app600=(A617+A620+A623+A626+A629+A632+A635+A638+A641+A644)/10) 
dataMerge <- transform(dataMerge,app500=(A488+A491+A494+A497+A500+A503+A506+A509)/8)
dataMerge <- transform(dataMerge,app420=(A416+A419+A422+A425+A428+A431+A434)/7)
data_sub <- dataMerge[,c("datetime","remark","EG","decYear","sinDY","cosDY","app600","app500","app420")]
data_sub_cens <- importQW(data_sub,c("decYear","sinDY","cosDY","app600","app500","app420"),"EG","remark","",0.0000002,"User","kg","Unk","","91075","EG")
siteName <- "GMIA"
siteNo <- '040871475'
siteINFO <-  getNWISSiteInfo(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "EG"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/",siteName,sep="")

