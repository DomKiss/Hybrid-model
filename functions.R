PrepareData <- function(T_colnumber, P_colnumber, DB_length){
  dates <- adata$Date[2:DB_length]
  prices <- adata$`Adj Close`[2:DB_length]
  log_returns <- diff(log(adata$`Adj Close`), p=1)
  data.frame(dates, prices, log_returns)
}


#data=dcoil$prices, n_vector=milyen késleltetések, Ind_name=ma/mom
TI_gen <- function(data, date, n_vector, Ind_name) {
  result=data.frame()
  result <- as.data.frame.Date(date)
  for (n in n_vector) {
    ind <- SMA(data, n=n)
    result <- cbind(result, ind)
  }
  for (i in 1:length(n_vector)){
    
    colnames(result)[i+1] <-paste(Ind_name, n_vector[i], sep="")
  }
  
  colnames(result)[1] <- c("Date")
  return(result)
}


#select data in each loop
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

#save coeffitients
save_coef <- function(dfx, lm) {
  intercept <- coef(lm)[1]
  result <- intercept
  for (i in 2:(length(df_x)+1)) {
  
    koef <- coef(lm) [i]
    result <- cbind(result, koef)
    colnames(result)[i] <- colnames(dfx)[i-1]
    
  }
  colnames(result)[1] <- c("Intercept")
  return(result)
}

save_coef(df_x, llmm)





