library(tseries)
library(forecast)
library(neuralnet)
library(dynlm)
library(LPTime)
library(TTR)
library(rstudioapi)
library(quantmod)


tickerlist = c("BTC-USD",'XRP-USD')

for(ticker in 1:length(tickerlist)) {
   
   hybridmodel(tickerlist[ticker])
  
}