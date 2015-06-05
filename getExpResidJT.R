#changed dfabs to dataAbs in line 18
#
getExpResidJT <- function (wavelength, rangeReg, rangeGap, dataAbs, waveCol, colSubsetString, 
          dataSummary, grnum) 
{
  df <- dataAbs[, grep(colSubsetString, names(dataAbs))]
  grnums <- as.character(dataSummary[, grnum])
  df <- df[, grnums]
  aRef <- which(dataAbs[, waveCol] == rangeReg[2])
  aStart <- which(dataAbs[, waveCol] == rangeReg[2])
  L <- dataAbs[, waveCol]
  wvRows <- which((L >= rangeReg[1] & L <= rangeGap[1]) | (L >= 
                                                             rangeGap[2] & L <= rangeReg[2]))
  wvRowsAll <- which((L >= rangeReg[1] & L <= rangeReg[2]))
  wvRowsGap <- wvRowsAll[which(!wvRowsAll %in% wvRows)]
  AResids <- numeric()
  aWavelngth <- which(dataAbs[wvRowsAll, waveCol] == wavelength)
  for (i in 1:dim(df)[2]) {
    aCoef <- df[wvRows, i]
    names(aCoef) <- dataAbs[wvRows, waveCol]
    if (sum(aCoef > 0)) {
      if (min(aCoef) <= 0) {
        minA <- min(aCoef[aCoef > 0])
        aCoef[aCoef <= 0] <- minA/2
      }
      y <- log(aCoef/aCoef[as.character(rangeReg[2])])
      x <- L[wvRows] - L[which(L == rangeReg[2])]
      m <- lm(y ~ x)
      Sag <- -coef(m)[2]
      aLAll <- df[aRef, i] * exp(-Sag * (dataAbs[wvRowsAll, 
                                                 waveCol] - rangeReg[2]))
      AResidsAll <- df[wvRowsAll, i] - aLAll
      AResid <- AResidsAll[aWavelngth]
      AResids <- c(AResids, AResid)
      residRow <- which(dataAbs[wvRowsAll, waveCol] == 
                          wavelength)
      plot(df[wvRowsAll, i] ~ dataAbs[wvRowsAll, waveCol], 
           main = names(df)[i])
      points(df[wvRowsGap, i] ~ dataAbs[wvRowsGap, waveCol], 
             col = "blue")
      lines(aLAll ~ dataAbs[wvRowsAll, waveCol], col = "red")
    }
    else {
      AResid <- NA
    }
  }
  dfResids <- data.frame(grnums, AResids)
  names(dfResids) <- c(grnum, "Aresids")
  dataSummaryFinal <- merge(dataSummary, dfResids, by = grnum, 
                            all = TRUE)
  return(dataSummaryFinal)
}