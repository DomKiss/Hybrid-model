PrepareData <- function(T_colnumber, P_colnumber, DB_length){
  ddates <- as.matrix(dcoil[T_colnumber])
  prices <- as.matrix(dcoil[P_colnumber])
  log_returns <- diff(log(prices), p=1)
}

