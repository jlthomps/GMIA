getSagJT <- function (dataAbs, waveCol, sag, colSubsetString, dataSummary, 
          grnum) {
  df <- dataAbs[, grep(colSubsetString, names(dataAbs))]
  df <- df[, dataSummary[, grnum]]
  L <- dataAbs[, waveCol]
  for (j in 1:dim(sag)[1]) {
    wvRows <- which(L >= sag[j, 1] & L <= sag[j, 2])
    Sag <- numeric()
    for (i in 1:dim(df)[2]) {
      aCorr <- df[wvRows, i]
      names(aCorr) <- dataAbs[wvRows, waveCol]
      if (min(aCorr) <= 0) {
        minA <- min(aCorr[aCorr > 0])
        aCorr[aCorr <= 0] <- minA/2
      }
      y <- log(aCorr/aCorr[as.character(sag[j, 2])])
      x <- L[wvRows] - L[which(L == sag[j, 2])]
      Sag <- c(Sag, -coef(lm(y ~ x))[2])
    }
    SagName <- paste("Sag", sag[j, 1], "_", sag[j, 2], sep = "")
    dfSag <- data.frame(Sag, names(df))
    names(dfSag) <- c(SagName, grnum)
    dataSummary <- merge(dataSummary, dfSag, by = grnum, 
                         all = TRUE)
  }
  return(dataSummary)
}