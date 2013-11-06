################################################################################################
###                                                                                          ###
#                                        Milwaukee Airport                                     #
###                                                                                          ###
################################################################################################

setwd("//igsarmewfsapa/projects/NonPoint Evaluation/gmia/R")

source("//igsarmewfsapa/projects/NonPoint Evaluation/gmia/R/fxn_TSstats.R")
source("//igsarmewfsapa/projects/NonPoint Evaluation/gmia/R/fxn_RMprep.R")

###Create common time values:
times.1 <- c(6)

###Read in files:
MKE <- read.delim("//igsarmewfsapa/projects/NonPoint Evaluation/gmia/R/mke.txt") 
MKEdates <- read.delim("//igsarmewfsapa/projects/NonPoint Evaluation/gmia/R/gmia-start_storm-dates-2006-2012.txt",header=FALSE)
names(MKEdates) <- "date"

###RMprep common files:
MKE <- RMprep(df=MKE,prep.type=1,date.type=3,dates.in=c("Date","Time"),dates.out="date")
MKEdates <- RMprep(df=MKEdates,prep.type=1,date.type=1,dates.in="date",dates.out="psdate")

###TSstats unique files:
MKE.out <- TSstats(df=MKE,date="date",varnames="HourlyPrecip",dates=MKEdates,starttime="psdate",times=times.all,units="hrs",stats.return="sum")
MKE.out <- TSstats(df=MKE,date="date",varnames="DryBulbFarenheit",dates=MKE.out,starttime="psdate",times=times.all,units="hrs",stats.return=c("mean","max","min")
MKE.out <- TSstats(df=MKE,date="date",varnames="DryBulbCelsius",dates=MKE.out,starttime="psdate",times=times.4,units="hrs",stats.return=c("mean","max","min")

###Output to file:
write.table(x=MKE.out,file="//igsarmewfsapa/projects/igsarmewfsapa/projects/gmia/WY2012 QW data/R/MKE.out.txt",row.names=FALSE,sep="\t")

#End of Milwaukee Airport compilation
################################################################################################