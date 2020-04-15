

importdata <- function(ticker, start="2016-01-01", end="2020-02-28") {
  
  adatax <- getSymbols.yahoo(paste(ticker), env=globalenv(), from= "2016-01-01", to="2020-02-28", auto.assign = FALSE, return.class = 'data.frame')
  adata <- cbind(as.data.frame.Date(rownames(adatax)), adatax)
  colnames(adata)[1] <- c("Date")
  prep_adata <- PrepareData(1, 7, nrow(adata), adata=adata)
  return(prep_adata)
}




#T_colnumber=number of the Date column; P_colnumber=number of the price column, DB_length=length of the importent database
PrepareData <- function(T_colnumber, P_colnumber, DB_length, adata){
  dates <- adata[2:DB_length, T_colnumber]
  #dates <- adata$Date[2:DB_length]
  prices <- adata[2:DB_length, P_colnumber]
  #prices <- adata$`Adj Close`[2:DB_length]
  log_returns <- diff(log(adata[,P_colnumber]), p=1)
  #log_returns <- diff(log(adata$`Adj Close`), p=1)
  return(data.frame(dates, prices, log_returns))
}


#data=dcoil$prices, n_vector=milyen késleltetések, Ind_name=ma/mom
TI_gen <- function(data, date, n_vector, Ind_name) {
  result = data.frame()
  result <- as.data.frame.Date(date)
  
  if (Ind_name == "MA") {
    for (n in n_vector) {
      ind <- SMA(data, n = n)
      result <- cbind(result, ind)
    }
  } else{
    for (n in n_vector) {
      ind <- momentum(data, n = n)
      result <- cbind(result, ind)
    }
  }
  
  for (i in 1:length(n_vector)) {
    colnames(result)[i + 1] <- paste(Ind_name, n_vector[i], sep = "")
  }
  
  colnames(result)[1] <- c("Date")
  return(result)
}


#select data for a given window.
sel_data <- function(start_obs, end_obs, date, data_df, Ind_name) {
  result=data.frame()
  result <- as.data.frame.Date(date[start_obs:end_obs])
   for(i in 2:length(data_df)) {
    
    sel <- data_df[start_obs:end_obs,i]
    result <- cbind(result, sel)
    colnames(result)[i] <- colnames(data_df)[i]
   }
  
  
  colnames(result)[1] <- c("Date")
  
  return(result)
}

#save coeffitients for the linear regression
save_coef <- function(dfx, lm) {
  intercept <- coef(lm)[1]
  result <- intercept
  for (i in 2:(length(dfx)+1)) {
  
    koef <- coef(lm) [i]
    result <- cbind(result, koef)
    colnames(result)[i] <- paste("coef_",colnames(dfx)[i-1], sep="")
    
  }
  colnames(result)[1] <- c("Intercept")
  return(result)
}

#making log returns lagged
laglog <- function(df,column_to_be_lagged, l) {
  lag1 <- as.data.frame(embed(column_to_be_lagged,l+1)[,1])
  colnames(lag1) <- c("Lagged_logreturn")
  df <- cbind(df[1:(nrow(df)-1),], lag1)
  result <- df[ ,!(colnames(df) == "Log_returns")]
  
}
