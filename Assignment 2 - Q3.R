###########Q3.1, Q3,2, Q3,3
powerSeq <- function(base,maxExp){
  base <- as.integer(base)
  maxExp <- as.integer(maxExp)
  x <- vector("numeric",maxExp)
  y <- vector("numeric",maxExp)
  x[1] <- 0
  y[1] <- 1
  for (i in 1:maxExp ){
    y[i+1] <- base ** i
    x[i+1] <- i
  }
  return (list(x,y))
}
base_3 <- powerSeq(3,10)
base_4 <- powerSeq(4,10)
base_5 <- powerSeq(5,10)

plot(unlist(base_3[1]),unlist(base_3[2]), type = "l",col = "green",xlab = "Power", ylab = "value", main = "Power plot")
lines(unlist(base_4[1]),unlist(base_4[2]), type = "l",col = "red")
lines(unlist(base_5[1]),unlist(base_5[2]), type = "l", col= "blue")
legend("topleft",legend=c("base 3", "base 4","base 5"),lty=c(1,1,1),col=c("green","red","blue"),bg="white",lwd=2)

powerChart <- cbind(unlist(base_3[2]),unlist(base_4[2]),unlist(base_5[2]))
colnames(powerChart) <- c("3","4","5")
rownames(powerChart) <- paste0( 0:10)
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
C <- vector('numeric', 11)
for (i in 1:length(rownames(powerChart))){
  result <- 0
  for (j in 1:length(colnames(powerChart))){
    result <- result + powerChart[i,j]
    if (j == 2){
      result <- result - powerChart[i,j]
    }
  }
  C[i] <- result
}

print(C)