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

temp <- testAbs$GRnumber
temp2 <- substr(testAbs$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(patter="_2",temp))+8)
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  c <- length(b)
  temp3[i] <- b[c]
}

testAbs$date <- temp2
testAbs$ProjectID <- temp3
testAbs$datetime <- strptime(testAbs$date,format="%Y%m%d")

write.csv(testAbs,file="testAbs.csv")


