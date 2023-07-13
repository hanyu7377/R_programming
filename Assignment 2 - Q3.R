###########Q3.1, Q3,2, Q3,3
powerSeq <- function(base,maxExp){
  base <- as.integer(base)
  maxExp <- as.integer(maxExp)
  x <- vector("numeric",maxExp + 1)
  y <- vector("numeric",maxExp + 1)
  for (i in 0:maxExp ){
    y[i+1] <- base**i
    x[i+1] <- i 
  }
  names(y) <- x
  return (y)
}
base_3 <- powerSeq(3,10)
base_4 <- powerSeq(4,10)
base_5 <- powerSeq(5,10)
print(base_3)
print(base_4)
print(base_5)
x_axis <- seq(from = 0,to = 10,by = 1)
plot(x_axis,base_3, type = "l",col = "green",xlab = "Power", ylab = "value", main = "Power plot")
lines(x_axis,base_4, type = "l",col = "red")
lines(x_axis,base_5, type = "l", col= "blue")
#legend("topleft",legend=c("base 3", "base 4","base 5"),lty=c(1,1,1),col=c("green","red","blue"),bg="white",lwd=2)

powerChart <- cbind(base_3,base_4,base_5)
colnames(powerChart) <- c("3","4","5")
print(powerChart)
############Q3.4   create a powerChartDes matrix
powerChartDes <- setNames(data.frame(matrix(nrow = 11,ncol = 3)),colnames(powerChart))
rownames(powerChartDes) <- paste0(0:10)
for (i in 1:length(rownames(powerChart))){
  power <- as.character(rownames(powerChart)[i])
  for (j in 1:length(colnames(powerChart))){
    base <- as.character(colnames(powerChart)[j])
    value <- as.character(powerChart[i,j])
    result = paste(base,"to the power of", power, "=",value)
    powerChartDes[i,j] <- result
  }
}
print(powerChartDes)
###########Q3.5   create a negpowerChart matrix
negpowerChart <- setNames(data.frame(matrix(nrow = 11,ncol = 3)),colnames(powerChart))
rownames(negpowerChart) <- paste0(0:10)
for (i in 1:length(rownames(powerChart))){
  for (j in 1:length(colnames(powerChart))){
    negpowerChart[i,j] <- 1/powerChart[i,j]
  }
}
print(negpowerChart)
############Q3.6 create C vector 
for (i in 1:length(rownames(powerChart))){
  result <- 0
  for (j in 1:length(colnames(powerChart))){
    if (j == 2){
      result <- result - powerChart[i,j]
    }
    else {
      result <- result + powerChart[i,j]
    }
  }
  C[i] <- result
}
print(C)
lines(x_axis,C, type = "l", col= "black")
legend("topleft",legend=c("base 3", "base 4","base 5","C"),lty=c(1,1,1,1),col=c("green","red","blue","black"),bg="white",lwd=2)