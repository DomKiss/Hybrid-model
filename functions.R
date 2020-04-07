
#T_colnumber=number of the Date column; P_colnumber=number of the price column, DB_length=length of the importent database
PrepareData <- function(T_colnumber, P_colnumber, DB_length){
  dates <- adata[2:DB_length, T_colnumber]
  #dates <- adata$Date[2:DB_length]
  prices <- adata[2:DB_length, P_colnumber]
  #prices <- adata$`Adj Close`[2:DB_length]
  log_returns <- diff(log(adata[,P_colnumber]), p=1)
  #log_returns <- diff(log(adata$`Adj Close`), p=1)
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
  for (i in 2:(length(df_x)+1)) {
  
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


#the hybrid model function
hybridmodel <- function(ticker) {
  
  #importing data
  adatax <- getSymbols.yahoo(paste(ticker), env=globalenv(), from= "2010-01-01", to="2020-02-28", auto.assign = FALSE, return.class = 'data.frame')
  adata <- cbind(as.data.frame.Date(rownames(adatax)), adatax)
  colnames(adata)[1] <- c("Date")
  #adata2<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/BTC-USD (1).xlsx", sep="")) #read excel
  
  
  #select date (ddates) and price (prices) columns from the dataset, and calculate the log returns (log_returns). 
  #arguments: colnumber of the date column, column number of the price column, lenth of the time-series
  prep_adata <- PrepareData(1, 7, nrow(adata))
  
  #testing stationarity
  adf.test(prep_adata$log_returns)
  
  #creating technical indicators
  n_vector <- c(60,70,80,90,100,120)
  TIs <- TI_gen(prep_adata$prices, prep_adata$dates, n_vector, Ind_name="MA")
  
  #join TIs + log_returns matrix, omit NAs
  Input_data_df_nonlagged <- na.omit(cbind(TIs, prep_adata$log_returns))
  colnames(Input_data_df_nonlagged)[length(Input_data_df_nonlagged)] <- c("Log_returns")
  
  #creating df with lagged log_returns
  Input_data_df <- laglog(Input_data_df_nonlagged, Input_data_df_nonlagged$Log_returns,1)
  
  
  RMSE_lin <- matrix(0,50,1)
  RMSE_hibr <- matrix(0,50,1)
  RMSE_NN<- matrix(0,50,1)
  
  
  d=0
  nt=3
  RMSEs_lin<- matrix(0,nt,1)
  RMSEs_nn<- matrix(0,nt,1)
  RMSEs_hibr<- matrix(0,nt,1)
  TrainingSiz <- matrix(0,nt,1)
  for(d in 1:nt) 
  {
    
    #rolling window lm model 
    k=0
    windowsSize <- nrow(Input_data_df)-1400+d*100 # training data size
    testsize    <- 1    # 1 step ahead forecast
    Nexp <- nrow(Input_data_df)-windowsSize-1 #maximum number of experiments
    Nexp
    for(k in 0:Nexp)  # run experiments
      
    {
      A         <- k*testsize + 1 
      B         <- A + windowsSize - 1 
      start_obs <- A 
      end_obs   <- B
      
      #select the window in each loop
      df_sel <-  sel_data(start_obs,end_obs, Input_data_df$Date,Input_data_df, "MA")
      df_x <- df_sel[,2:(length(df_sel)-1)] #selected x variables
      df_y <- as.data.frame(df_sel[,length(df_sel)])  #selected y variables
      colnames(df_y) <- ("Lagged_logreturns")
      
      #fitting linear regression
      llmm <- lm(Lagged_logreturn ~ . - Date, data = df_sel)
      
      #saving coeffitiens
      df_lin_coefs <- save_coef(df_x, llmm)
      residual_tr <- residuals(llmm)
      
      #fitting neural net on the linreg residuals
      nn=neuralnet(residual_tr~., data=df_x, hidden=1, act.fct = "logistic", threshold=0.1)
      
      
      #fitting only neural net on the log_returns
      nn_only=neuralnet(Lagged_logreturn ~ . - Date, data = df_sel)
      i =1
      A <- B+1
      df_sel_test <- sel_data(A,A, Input_data_df$Date,Input_data_df, "MA")
      df_x <- df_sel_test[,2:(length(df_sel)-1)]
      df_y <- as.data.frame(df_sel_test[,length(df_sel_test)])
      colnames(df_y) <- ("Log_returns")
      predict_y <- matrix(0, testsize, 1)
      nnonly_results <- matrix(0, testsize, 1)
      residual_fc <- matrix(0, testsize, 1)
      nnresults<- matrix(0, testsize, 1)
      final_results<- matrix(0, testsize, 1)
      SSE_lin <- 0
      SSE_hibr <- 0
      SSE_NN <- 0
      for(i in 1:testsize)
      {
        #lin reg test
        predict_y[i] <- df_lin_coefs[1] + sum(df_lin_coefs[,-1]*df_x[i,])
        SSE_lin <- SSE_lin+(predict_y[i]-df_y[i,])^2
        residual_fc[i] <- predict_y[i]-df_y[i,]
        #nn test
        predict_y_nnonly <- predict(nn_only, df_x[i,])
        nnonly_results[i] <- predict_y_nnonly
        SSE_NN <- SSE_NN + (nnonly_results[i]-df_y[i,])^2
        
        predictnn_y <- compute(nn,df_x[i,])
        nnresults[i] <- predictnn_y$net.result
        final_results[i] <- nnresults[i] + unlist(predict_y[i])
        SSE_hibr <- SSE_hibr+(final_results[i]-df_y[i,])^2
      }
      RMSE_lin[k+1] <- sqrt(SSE_lin/testsize)
      RMSE_NN[k+1] <- sqrt(SSE_NN/testsize)
      RMSE_hibr[k+1] <- sqrt(SSE_hibr/testsize)
      
      
    }
    
    print(RMSE_lin[k+1])
    print(RMSE_NN[k+1])
    print(RMSE_hibr[k+1]) 
    print(SSE_lin)
    print(SSE_NN)
    print(SSE_hibr)
    
    
    RMSEs_lin[d]=RMSE_lin[k+1]
    RMSEs_nn[d]=RMSE_NN[k+1]
    RMSEs_hibr[d]=RMSE_hibr[k+1]
    TrainingSiz[d]=(A + windowsSize - 1 )-(k*testsize + 1 )
    
  }
  final_results=cbind(TrainingSiz,RMSEs_lin,RMSEs_nn,RMSEs_hibr)
  colnames(final_results) <- c("Training set size","Lin", "NN", "Hibr")
  thebest <-  as.data.frame(colnames(final_results)[apply(final_results,1,which.min)])
  final_table <-  as.data.frame(cbind(final_results, thebest))
  colnames(final_table) <- c("Training set size","Lin", "NN", "Hibr", "Best model")
  View(final_table)
  
  
  
}
  
  
