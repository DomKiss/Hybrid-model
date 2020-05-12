par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,28),ylim=c(0,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
i <- 1
for(j in 1:10)
{
  test <- (6+j):26
  train <- 1:(5+j)
  arrows(0,1-j/20,27,1-j/20,0.05)
  points(train,rep(1-j/20,length(train)),pch=19,col="black")
  if(length(test) >= i)
    points(test[i], 1-j/20, pch=19, col="red")
  if(length(test) >= i)
    points(test[-i], rep(1-j/20,length(test)-1), pch=19, col="gray")
  else
    points(test, rep(1-j/20,length(test)), pch=19, col="gray")
}
