library(tseries)
library(forecast)
library(neuralnet)
library(dynlm)
library(LPTime)
library(TTR)
library(rstudioapi)
library(quantmod)
library(writexl)
library(plm)
library(dplyr)


tickervector <-  c("BTC-USD", "XRP-USD", "ETH-USD")
merged_results = data.frame()
resulttable = data.frame()
RMSEs_t = data.frame()
RMSEs = data.frame()
RMSEs_tick = data.frame()
f_results_table = data.frame(stringsAsFactors = F)
dibmar =data.frame()
dibmar_total =data.frame()
testnumber=tibble()
yvalos2 <- tibble()

yvalos <- matrix(0, 0, 1)
for (ticker in tickervector) {
  
  #importing data
  adatax <-
    getSymbols.yahoo(
      paste(ticker),
      env = globalenv(),
      from = "2010-01-01",
      to = "2020-03-31",
      periodicity = "weekly",
      auto.assign = FALSE,
      return.class = 'data.frame'
    )
  adata <- cbind(as.data.frame.Date(rownames(adatax)), adatax)
  colnames(adata)[1] <- c("Date")
  #adata2<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/BTC-USD (1).xlsx", sep="")) #read excel
  
  
  #select date (ddates) and price (prices) columns from the dataset, and calculate the log returns (log_returns).
  #arguments: colnumber of the date column, column number of the price column, lenth of the time-series
  prep_adata <- PrepareData(1, 7, nrow(adata), adata)
  
  #testing stationarity
  adf.test(prep_adata$log_returns)
  
  #creating technical indicators
  n_vector <- c(seq(28, 52, by=2))
  TIs <-
    cbind(TI_gen(prep_adata$prices, prep_adata$dates, n_vector, Ind_name = "MA"),TI_gen(prep_adata$prices, prep_adata$dates, n_vector, Ind_name = "TSMOM")[-1])
  
  #join TIs + log_returns matrix, omit NAs
  Input_data_df_nonlagged <-
    na.omit(cbind(TIs, prep_adata$log_returns))
  colnames(Input_data_df_nonlagged)[length(Input_data_df_nonlagged)] <-
    c("Log_returns")
  
  #creating df with lagged log_returns
  Input_data_df <-
    laglog(Input_data_df_nonlagged,
           Input_data_df_nonlagged$Log_returns,
           1)
  
  
  RMSE_lin <- matrix(0, 50, 1)
  RMSE_hibr <- matrix(0, 50, 1)
  RMSE_NN <- matrix(0, 50, 1)
  
  
  d = 0
  nt = 2
  
  
  #rolling window lm model
  k = 0
  windowsSize <-
    round (0.50 * nrow(Input_data_df)) # training data size
  testsize    <- 1    # 1 step ahead forecast
  Nexp <-
    nrow(Input_data_df) - windowsSize - 1 #maximum number of experiments
  Nexp
  RMSEs_lin <- matrix(0, 50, 1)
  RMSEs_nn <- matrix(0, 50, 1)
  RMSEs_hibr <- matrix(0, 50, 1)
  test_date <- matrix(0, 50, 1)
  ynn <- matrix(0, 0, 1)
  yllmm <- matrix(0, 0, 1)
  yhibr <- matrix(0, 0, 1)
  
  TrainingSiz <- Nexp/nrow((Input_data_df))
  for (k in 0:Nexp)
    # run experiments
    
  {
    A         <- k * testsize + 1
    B         <- A + windowsSize - 1
    start_obs <- A
    end_obs   <- B
    
    #select the window in each loop
    df_sel <-
      sel_data(start_obs,
               end_obs,
               Input_data_df$Date,
               Input_data_df,
               "MA")
    df_x <- as.data.frame(df_sel[, 2:(length(df_sel) - 1)]) #selected x variables
    df_y <-
      as.data.frame(df_sel[, length(df_sel)])  #selected y variables
    colnames(df_y) <- ("Lagged_logreturns")
    
    #fitting linear regression
    llmm <- lm(Lagged_logreturn ~ . - Date, data = df_sel)
    
    #saving coeffitiens
    df_lin_coefs <- save_coef(df_x, llmm)
    residual_tr <- residuals(llmm)
    maxmindfx <- as.data.frame(lapply(df_x, normalize))
    ddfx <- cbind(residual_tr, maxmindfx)
    #fitting neural net on the linreg residuals
    nn = neuralnet(
      residual_tr ~ .,
      data = ddfx,
      hidden = 1,
      rep =2,
      act.fct = "logistic"
    )
    
    
    #fitting only neural net on the log_returns
    nn_only = neuralnet(Lagged_logreturn ~ . - Date, data = df_sel,
                        hidden = 1, rep=2,
                        act.fct = "logistic"
                        )
    
    
    #Test
    i = 1
    A <- B + 1
   # testnumber[k+1,1] <- A
    df_sel_test_h <-
      sel_data(start_obs, A, Input_data_df$Date, Input_data_df, "MA")
    df_xh <- as.data.frame(df_sel_test_h[, 2:(length(df_sel) - 1)])
    df_xh<- as.data.frame(lapply(df_xh, normalize))
    df_xh <- as.data.frame(df_xh[nrow(df_xh),])
    
    
    df_sel_test <-
      sel_data(A, A, Input_data_df$Date, Input_data_df, "MA")
    df_x <- as.data.frame(df_sel_test[, 2:(length(df_sel) - 1)])
    df_y <- as.data.frame(df_sel_test[, length(df_sel_test)])
    colnames(df_y) <- ("Log_returns")
    predict_y <- matrix(0, testsize, 1)
    nnonly_results <- matrix(0, testsize, 1)
    residual_fc <- matrix(0, testsize, 1)
    nnresults <- matrix(0, testsize, 1)
    final_results <- matrix(0, testsize, 1)
    SSE_lin <- 0
    SSE_hibr <- 0
    SSE_NN <- 0
    for (i in 1:testsize)
    {
      #lin reg test
      predict_y[i] <-
        df_lin_coefs[1] + sum(df_lin_coefs[, -1] * df_x)
      SSE_lin <- SSE_lin + (predict_y[i] - df_y[i, ])^2
      residual_fc[i] <- predict_y[i] - df_y[i, ]
      
      #nn test
      predict_y_nnonly <- predict(nn_only, df_x)
      nnonly_results[i] <- predict_y_nnonly
      SSE_NN <- SSE_NN + (nnonly_results[i] - df_y[i, ])^2
      
      #hybrid test
      predictnn_y <- predict(nn, df_xh)
      nnresults[i] <- predictnn_y
      final_results[i] <- nnresults[i] + unlist(predict_y[i])
      SSE_hibr <- SSE_hibr + (final_results[i] - df_y[i, ])^2
    } # tesztmeret miatti for vége
    test_date[k + 1] <- as.Date(df_sel_test$Date)
    yllmm <-  rbind(yllmm, predict_y[i])
    ynn<-  rbind(ynn, nnonly_results)
    yhibr<-  rbind(yhibr, final_results)
    y_new <- tibble(ticker, df_y)
    yvalos2 <- rbind(yvalos2, y_new)
    yvalos <-  rbind(yvalos, df_y)
    
    RMSE_lin[k + 1] <- sqrt(SSE_lin / testsize)
    RMSE_NN[k + 1] <- sqrt(SSE_NN / testsize)
    RMSE_hibr[k + 1] <- sqrt(SSE_hibr / testsize)
    
    cum_lin <- cumsum(RMSE_lin)
    cum_NN <- cumsum(RMSE_NN)
    cum_hibr <- cumsum(RMSE_hibr)
    
    
    RMSEs <-
      as.data.frame(cbind(as.data.frame.Date(test_date), TrainingSiz, RMSE_lin, RMSE_NN, RMSE_hibr, cum_lin, cum_NN, cum_hibr))
    
    
  } #egy tickeren teljesen vege
  dibmar <- cbind(ticker, dm.test(RMSE_lin, RMSE_NN)$p.value,dm.test(RMSE_lin, RMSE_hibr)$p.value, dm.test(RMSE_NN, RMSE_hibr)$p.value)
  dibmar_total <- rbind(dibmar_total, dibmar)
  
  
  RMSEs_tick <- as.data.frame(cbind(ticker, RMSEs))
  f_results_table <- rbind(f_results_table, RMSEs_tick)
  
  
}

f_results_table$test_date <- as.Date(f_results_table$test_date)
View(f_results_table)


final_results <-
  cbind(
    tapply(f_results_table$RMSE_lin, f_results_table$ticker, mean),
    tapply(f_results_table$RMSE_NN, f_results_table$ticker, mean),
    tapply(f_results_table$RMSE_hibr, f_results_table$ticker, mean)
  )

colnames(final_results) <- c("Lin", "NN", "Hibr")
thebest <-  as.data.frame(colnames(final_results)[apply(final_results,1,which.min)])
final_results <-  as.data.frame(cbind(final_results, thebest))
colnames(final_results) <-  c("Lin", "NN", "Hibr", "Best model")
View(final_results)
cbind(yvalos, yllmm, ynn, yhibr)
colnames(dibmar_total) <-  c("ticker", "LinNN", "LinHibr", "NNHibr")

