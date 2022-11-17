sd0 <- function(vct) {
  if (!is.numeric(vct)) {
    return(NA)
  }
  
  return(sd(vct, na.rm = T))
}


std0 <- function(vct, scl) {
  if (!is.numeric(vct)) {
    return(vct)
  }
  
  return(vct/scl)
}

scale <- function(data) {
  unlist(lapply(data, sd0))
}

standardise <- function(data) {
  scaler <- scale(data)
  
  numCols <- which(unlist(lapply(data, is.numeric)))
  num <- as.data.frame(mapply(std0, vct = data[,numCols], scl = scaler[numCols], SIMPLIFY = T))
  fct <- data[,-numCols]
  return(cbind(fct, num))
}
