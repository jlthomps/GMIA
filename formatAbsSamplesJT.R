formatAbsSamplesJT <- function(dateLower,dateUpper,Type,Project){
  
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
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,9]==Project),]
      if(length(which(DescriptionFile[,9]==Project))<1) next}
    
    if(Type =='Environmental Samples'){
      exclude <- grep('lank',DescriptionFile[,1],ignore.case = TRUE)
      exclude <- c(exclude,grep('tandard',DescriptionFile[,1],ignore.case = TRUE))
      exclude <- c(exclude,grep('aseline',DescriptionFile[,1],ignore.case = TRUE))
      exclude <- c(exclude,grep('-R',DescriptionFile[,1],ignore.case = TRUE))
      exclude <- c(exclude,grep('-D',DescriptionFile[,1],ignore.case = TRUE))
      exclude <- c(exclude,grep('1',DescriptionFile[,2],ignore.case = TRUE))
      exclude <- c(exclude,grep('Tea',DescriptionFile[,1],ignore.case = TRUE))
      
      if(length(exclude)==0){
        DescriptionFile <- DescriptionFile}else{
          DescriptionFile <- DescriptionFile[-c(exclude),]
        }
    }
    
    if(Type == 'Blank'){
      n = grep('bla',DescriptionFile[,1],ignore.case = TRUE)
      DescriptionFile <- DescriptionFile[c(n),]} 
    
    if(Type == 'Tea Standard'){
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] == '1% Tea Standard'),]}
    
    if(Type == 'Replicate'){
      r <- grep('-R',DescriptionFile[,1],ignore.case = TRUE)
      r <- c(r,grep('-D',DescriptionFile[,1],ignore.case = TRUE))
      DescriptionFile <- DescriptionFile[r,]}
    
    if(Type == 'All'){
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] != 'Baseline'),]
    } 
    
    filesNeeded <- DescriptionFile[,c(1,3)]
    if (sum(nchar(filesNeeded[,2]))>0) {
    filesNeed <- paste(filesNeeded[,2],'ABS.dat',sep='')
    dilution <- DescriptionFile[,4]
    names(dilution) <- filesNeed
    
    AbsfileNames <- filesNeed[which(filesNeed!='ABS.dat')]
    AbsStructure <- read.delim(paste(fileName,AbsfileNames[1],sep="/"), header=FALSE, stringsAsFactors=FALSE,row.names=NULL)
    AbsWavs <- AbsStructure[,1]
    
    
    AbsDf <- data.frame(matrix(numeric(), length(AbsWavs), length(AbsfileNames)), stringsAsFactors=FALSE)
    filesNeeded <- filesNeeded[nchar(filesNeeded[,2])>1,]
    colnames(AbsDf) <- paste(filesNeeded[,1],filesNeeded[,2],sep="_")
    colnames(AbsDf) <- paste(colnames(AbsDf),dateRangeFiles[i],sep='_')
  
    
    for (j in 1:length(AbsfileNames)){
      file <- paste(fileName,AbsfileNames[j],sep="/")
      name <- gsub('ABS.dat','',AbsfileNames[j])
      tempDf <- read.delim(file, header=FALSE, stringsAsFactors=FALSE,col.names=c('Wavelengths',name),row.names=NULL)
      tempDf <- tempDf[,-c(1)]
      if (!is.na(dilution[j])) {
      AbsDf[,j] <- tempDf*dilution[filesNeed[j]]
      } else {
        AbsDf[,j] <- tempDf
      }
    }
    AbsList[[i]] <- AbsDf
  }}
  
  AbsList[sapply(AbsList,is.null)] <- NULL
  FinalAbsDf <- do.call("cbind", AbsList)
  FinalAbsDf$Wavelength <- AbsWavs
  
  return(FinalAbsDf)
}