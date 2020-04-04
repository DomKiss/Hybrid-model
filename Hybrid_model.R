library(tseries)
library(forecast)
library(neuralnet)
library(dynlm)
library(LPTime)
library(TTR)
library(rstudioapi)
#library(parallel)
#library(foreach)
#library(doParallel)
#registerDoParallel(cores=1)

#importing data, defining variables
adata<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/BTC-USD (1).xlsx", sep="")) #read excel

#select date (ddates) and price (prices) columns from the dataset, and calculate the log returns (log_returns). 
#arguments: colnumber of the date column, column number of the price column, lenth of the time-series
prep_adata <- PrepareData(1, 6, nrow(adata))

#testing stationarity
adf.test(prep_adata$log_returns)


#creating technical indicators
n_vector <- c(5,3,4,44,55)
TIs <- TI_gen(prep_adata$prices, prep_adata$dates, n_vector, Ind_name="MA")

#TIs + log_returns matrix
Input_data_df <- na.omit(cbind(TIs, prep_adata$log_returns))
colnames(Input_data_df)[length(Input_data_df)] <- c("Log_returns")


RMSE_lin <- matrix(0,50,1)
RMSE_hibr <- matrix(0,50,1)
RMSE_NN<- matrix(0,50,1)
RMSEs_lin<- matrix(0,4,1)
RMSEs_nn<- matrix(0,4,1)
RMSEs_hibr<- matrix(0,4,1)

d=0
for(d in 1:4) 
{
#rolling window lm model 
k=0
windowsSize <- 1200+100*d # training data size
testsize    <- 1    # 1 step ahead forecast
Nexp <- length(x.ma)-windowsSize-testsize #maximum number of experiments
Nexp
for(k in 0:Nexp)  # run experiments

{
  k <- 275
  A         <- k*testsize + 1 
  B         <- A + windowsSize - 1 
  start_obs <- A 
  end_obs   <- B
  
 
  df_sel <-  NN_input(start_obs,end_obs, Input_data_df$Date,Input_data_df, "MA")
  df_x <- df_sel[,2:(length(df_sel)-1)]
  df_y <- as.data.frame(df_sel[,length(df_sel)])
  colnames(df_y) <- ("Log_returns")
  
  llmm <- lm(y_AB~x.ma_AB + x.ma10_AB + x.ma50_AB + x.ma100_AB + x.ma300_AB + x.moment_AB+x.moment10_AB+x.moment50_AB+x.moment100_AB+ x.moment200_AB+ x.moment300_AB) #define linear model
  intercept  <- coef(llmm)[1]
  co_x.ma_AB   <- coef(llmm)[2]
  co_x.ma10_AB <- coef(llmm)[3]
  co_x.ma50_AB <- coef(llmm)[4]
  co_x.ma100_AB <- coef(llmm)[5]
  co_x.ma300_AB <- coef(llmm)[6]
  co_x.moment_AB <- coef(llmm)[7]
  co_x.moment10_AB <- coef(llmm)[8]
  co_x.moment50_AB <- coef(llmm)[9]
  co_x.moment100_AB <- coef(llmm)[10]
  co_x.moment200_AB <- coef(llmm)[11]
  co_x.moment300_AB <- coef(llmm)[12]
  residual_tr <- residuals(llmm)
  df_nn=data.frame(residual_tr, x.ma_AB, x.ma10_AB,x.ma50_AB, x.ma100_AB,x.ma300_AB,x.moment_AB,x.moment10_AB,x.moment50_AB, x.moment100_AB, x.moment200_AB, x.moment300_AB) #data frame, for hybrid NN
  nn=neuralnet(residual_tr~x.ma_AB + x.ma10_AB + x.ma50_AB + x.ma100_AB + x.ma300_AB + x.moment_AB+x.moment10_AB+x.moment50_AB+x.moment100_AB+ x.moment200_AB+ x.moment300_AB, data=df_nn, hidden=3,act.fct = "logistic", threshold=0.1) #neural network fitting on the residuals of the linear model
  nn_only=neuralnet(y_AB~x.ma_AB + x.ma10_AB + x.ma50_AB + x.ma100_AB + x.ma300_AB + x.moment_AB+x.moment10_AB+x.moment50_AB+x.moment100_AB+ x.moment200_AB+ x.moment300_AB, data=df_nnonly, hidden=3, act.fct="logistic", threshold=0.1) #single neural network, fitting on the original dataset
  
  A <- B+1
  B <- B + testsize
  y_AB <- as.numeric(y[A:B])
  x.ma_AB <- as.numeric(x.ma[A:B])
  x.ma10_AB <- as.numeric(x.ma10[A:B])
  x.ma50_AB <- as.numeric(x.ma50[A:B])
  x.ma100_AB <- as.numeric(x.ma100[A:B])
  x.ma300_AB <- as.numeric(x.ma300[A:B])
  x.moment_AB <- as.numeric(x.moment[A:B])
  x.moment10_AB <- as.numeric(x.moment10[A:B])
  x.moment50_AB <- as.numeric(x.moment50[A:B])
  x.moment100_AB <- as.numeric(x.moment100[A:B])
  x.moment200_AB <- as.numeric(x.moment200[A:B])
  x.moment300_AB <- as.numeric(x.moment300[A:B])
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
    predict_y[i] <- intercept+x.ma_AB[i]*co_x.ma_AB + x.ma10_AB[i]*co_x.ma10_AB + x.ma50_AB[i]*co_x.ma50_AB + x.ma100_AB[i]*co_x.ma100_AB + x.ma300_AB[i]*co_x.ma300_AB + x.moment_AB[i]*co_x.moment_AB+x.moment10_AB[i]*co_x.moment10_AB+x.moment50_AB[i]*co_x.moment50_AB+x.moment100_AB[i]*co_x.moment100_AB+ x.moment200_AB[i]*co_x.moment200_AB+ x.moment300_AB[i]*co_x.moment300_AB
    SSE_lin <- SSE_lin+(predict_y[i]-y_AB[i])^2
    residual_fc[i] <- predict_y[i]-y_AB[i]
    nn_test_df=data.frame(x.ma_AB[i], x.ma10_AB[i],x.ma50_AB[i], x.ma100_AB[i],x.ma300_AB[i],x.moment_AB[i],x.moment10_AB[i],x.moment50_AB[i], x.moment100_AB[i], x.moment200_AB[i], x.moment300_AB[i])
    predict_y_nnonly <- compute(nn_only, nn_test_df)
    nnonly_results[i] <- predict_y_nnonly$net.result
    SSE_NN <- SSE_NN + (nnonly_results[i]-y_AB[i])^2
    
    predictnn_y <- compute(nn,nn_test_df)
    nnresults[i] <- predictnn_y$net.result
    final_results[i] <- nnresults[i] + predict_y[i]
    SSE_hibr <- SSE_hibr+(final_results[i]-y_AB[i])^2
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
}
final_results=cbind(RMSEs_lin,RMSEs_nn,RMSEs_hibr)
colnames(final_results) <- c("Lin", "nn", "hibr")
View(final_results)

