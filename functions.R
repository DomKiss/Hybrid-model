PrepareData <- function(T_colnumber, P_colnumber, DB_length){
  dates <- adata$Date[2:DB_length]
  prices <- adata$`Adj Close`[2:DB_length]
  log_returns <- diff(log(adata$`Adj Close`), p=1)
  data.frame(dates, prices, log_returns)
}




n_vector <- c(5,3,4)


Indicators <- function(Ind_name, n){
  ind <- SMA(prep_adata$prices, n=n)
  return(assign(paste(Ind_name, n_vector[n], sep=""), n_vector[n]))

}




#data=dcoil$prices, n_vector=milyen késleltetések, Ind_name=ma/mom
TI_gen <- function(data, date, n_vector, Ind_name) {
  result=data.frame()
  result <- as.data.frame.Date(date)
  for (n in n_vector) {
    ind <- SMA(data, n=n)
     #assign(paste(Ind_name, n_vector[n], sep=""), ind)
    result <- cbind(result, ind)
    colnames(result)[n] <- paste(Ind_name, n, sep="")
  }
  for (i in 2:lenth(data)){
    
    colnames(result)[i] <-paste(Ind_name, n_vector[i], sep="")
  }
  
  colnames(result)[1] <- c("Date")
  return(result)
}


TIs <- TI_gen(prep_adata$prices, prep_adata$dates, n_vector, Ind_name="MA")
View(TIs)



MA300 <- SMA(prep_adata$prices, n=300)
MA <- SMA(prices2, n = 200)
MA100 <- SMA(prices2, n=100)
MA50 <- SMA(prices2, n=50)
MA10 <- SMA(prices2, n=10)
moment <- momentum(prices2)
moment10 <- momentum(prices2, n=10)
moment50 <- momentum(prices2, n=50)
moment100 <- momentum(prices2, n=100)
moment200 <- momentum(prices2, n=200)
moment300 <- momentum(prices2, n=300)
a=na.omit(cbind(ddates2, moment, moment10, moment50, moment100, moment200, moment300, MA, MA10, MA50, MA100, MA300, log_returns))

