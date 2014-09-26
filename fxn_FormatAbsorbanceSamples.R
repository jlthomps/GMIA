#' formatAbsSamples
#' 
#' Retrieves specific (or all) absorbance files for  user defined AquaLog projects conducted at the USGS Wisconsin
#' Water Sciences Center (WI WSC). The user specifies the date range and project of interest, and the function formats the 
#' absorbance files such that each individual sample within the specified date range and project 
#' occupies a column; and so that one column contains the wavelengths. Converts the raw Aqualog absorbance dataframes 
#' to the format required for functions in USGSHydroOpt. Note that this function formats specific or ALL sample types
#' (blanks, 1% tea standards, replicates, and environmental samples).
#' 
#' @param dateLower the earliest date for which the user wishes to format and retrieve data. Should be specified as 
#' data type character and in the format 'YYYYMMDD.'
#' @param dateUpper the latest or most recent date for which the user wishes to format and retrieve data. Should be specified
#' as data type character and in the format 'YYYYMMDD'
#' @param Type the type of sample for the specified AquaLog project the user wishes to format.
#' Must be specified as data type character. There are four options: (1)-'Environmental Samples' 
#' (2)- 'Blank', (3)- 'Tea Standard', (4)- 'Replicate', or (5)- 'All.' The user must specify only
#' one of these options for this function input.
#' @param Project the name of the project for which optical data was analyzed. Should be formatted as data type
#' character and match the specification in the 'Project Name' field in the AquaLog sample file. If the user
#' specifies '', then all projects listed in the AquaLog sample file are processed, but still constrained by Type.
#' @return Absorbance dataframe where one column contains the wavelengths and all other columns contain the
#' absorbance value at a given wavelength.
#' @export
#' @examples
#' data <- formatAbsSamples(dateLower='20140716',dateUpper='20140725',Type='Environmental Samples',Project='OshkoshBeaches')
#' data1 <- formatAbsSamples(dateLower='20140716',dateUpper='20140725', Type='Blank',Project='OshkoshBeaches')
#' data2 <- formatAbsSamples(dateLower='20140716',dateUpper='20140725', Type='Tea Standard',Project='OshkoshBeaches')
#' data3 <- formatAbsSamples(dateLower='20140716',dateUpper='20140725', Type='Replicate',Project='OshkoshBeaches')
#' data4 <- formatAbsSamples(dateLower='20140716',dateUpper='20140725',Type='All',Project='')
formatAbsSamples <- function(dateLower,dateUpper,Type,Project){
  
  dateRangeFiles <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data')
  XLfile <- grep('xlsx',dateRangeFiles)
  dateRangeFiles <- dateRangeFiles[-c(XLfile)]
  dateRangeFiles <- dateRangeFiles[which(dateRangeFiles <= dateUpper)]
  dateRangeFiles <- dateRangeFiles[which(dateRangeFiles >= dateLower)]
  
  AbsList <- list()
  
  for(i in 1:length(dateRangeFiles)){
    
    fileName <- paste('//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data',dateRangeFiles[i],sep='/')
    
    setwd(fileName)
    
    allFiles <- list.files(path='.')
    
    DescriptionFile <- allFiles[grep('.csv',allFiles)]
    DescriptionFile <- read.csv(DescriptionFile,header=TRUE,stringsAsFactors=FALSE)
    DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] !=''),]
    
    if (Project == ''){
      DescriptionFile <- DescriptionFile
    }else{
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,9]==Project),]}
      if (length(which(DescriptionFile[,9]==Project))>0){
    
    if(Type =='Environmental Samples'){
      blank <- grep('bla',DescriptionFile[,1])
      
      if(length(blank)==0){
        DescriptionFile <- DescriptionFile}else{
          DescriptionFile <- DescriptionFile[-c(blank),]
        }
      
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] != '1% Tea Standard'),]
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] != 'Baseline'),]
      r <- grep('-R',DescriptionFile[,1])
      
      if(length(r)==0){
        DescriptionFile <- DescriptionFile}else{
        DescriptionFile <- DescriptionFile[-c(r),]
        }
    }
    
    if(Type == 'Blank'){
      n = grep('bla',DescriptionFile[,1])
      DescriptionFile <- DescriptionFile[c(n),]} 
    
    if(Type == 'Tea Standard'){
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] == '1% Tea Standard'),]}
    
    if(Type == 'Replicate'){
      r <- grep('-R',DescriptionFile[,1])
      DescriptionFile <- DescriptionFile[r,]}
    
    if(Type == 'All'){
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] != 'Baseline'),]
    } 
    
    filesNeeded <- DescriptionFile[,3]
    filesNeeded <- paste(filesNeeded,'ABS.dat',sep='')
    
    AbsfileNames <- filesNeeded
    AbsStructure <- read.delim(paste(fileName,AbsfileNames[1],sep="/"), header=FALSE, stringsAsFactors=FALSE,row.names=NULL)
    AbsWavs <- AbsStructure[,1]
    
    
    AbsDf <- data.frame(matrix(numeric(), length(AbsWavs), length(AbsfileNames)), stringsAsFactors=FALSE)
    colnames(AbsDf) <- gsub('ABS.dat','',AbsfileNames)
    colnames(AbsDf) <- paste(colnames(AbsDf),dateRangeFiles[i],sep='_')
    colnames(AbsDf) <- paste(colnames(AbsDf),DescriptionFile[,1],sep='_')
    
    for (j in 1:length(AbsfileNames)){
      file <- paste(fileName,AbsfileNames[j],sep="/")
      name <- gsub('ABS.dat','',AbsfileNames[j])
      tempDf <- read.delim(file, header=FALSE, stringsAsFactors=FALSE,col.names=c('Wavelengths',name),row.names=NULL)
      tempDf <- tempDf[,-c(1)]
      AbsDf[,j] <- tempDf
    }
    AbsList[[i]] <- AbsDf
  }}
  
  AbsList[sapply(AbsList,is.null)] <- NULL
  FinalAbsDf <- do.call("cbind", AbsList)
  FinalAbsDf$Wavelength <- AbsWavs
  
  return(FinalAbsDf)
}
