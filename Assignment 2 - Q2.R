#############################################Q2.1
check_is_vector <- function(input){
  if (is.vector(input) && length(input) != 1){
    input <- input[1]
  }
  return (input)
}

convert_to_integer <- function(input){
  if (is.numeric(input)){
    input <- as.integer(input)
  }
  return (input)
}
#print(check_is_vector(c(1)))
runFSequence <- function(x,a,f1){
  ###########check the x, a, f1 are mulit-component vector and then take the first element
  x <- check_is_vector(x)
  a <- check_is_vector(a)
  f1 <- check_is_vector(f1)
  ####if one of x,a,f1 is character, return NA
  if (typeof(x) == "character" | typeof(a) == "character" | typeof(f1) == "character")  {
      print(" Invalid Argument. A single integer is expected.")
      return (NA)
  }
  #####if one of x,a,f1 is lower than 0 ,return NA
  else if (x <= 0 | a <= 0 | f1 <= 0){
      print("The value should be a positive number")
      return (NA)
  }
  #####convert x, a, f1 to integer
  x <- convert_to_integer(x)
  a <- convert_to_integer(a)
  f1 <- convert_to_integer(f1)    
  #####################start main core operation##########
  F_n_vec <- vector("numeric", x)
  F_n_vec[1] <- f1  #####it will be y_axis in plot
  for (i in 2:x){
      F_n_vec[i] <- (F_n_vec[i-1]/i) * a 
  }
  return (F_n_vec)
}
result <- runFSequence(6777,29,2345)
#print(result[5:10])
#result <- runFSequence(50,7,9)
x_axis <- seq(from = 1,to = length(result),by = 1)
#result <- runFSequence(c(7,2,3),c(1,3,4),c(1,5,6))
print(result)

plot(x_axis,result, type = "o",xlab = "x_axis", ylab = "F_n_vec", main = "F Sequence")
#################################Q2.2
Vector1 <- runFSequence(20,5,30000)[5:10]
names(Vector1) <- paste0(5:10,"th")
print(Vector1)

Vector2 <- runFSequence(20,5,95000)[10:15]
names(Vector2) <- paste0(10:15,"th")
print(Vector2)
####################Q2.3
checkFDirectionChange <- function(input){
  #####use max and max.which function to get the max value and its index
  index_of_maxV <- which.max(input)
  maxV <- max(input)
  ##### IF there are two max in input,pick the one with higher index
  if (input[index_of_maxV] == input[index_of_maxV + 1]){ 
      index_of_maxV <- which.max(input) + 1
      maxV <- input[index_of_maxV]
  }
  return (print(paste0("a = ","<",maxV,">.", " Direction changed after<",index_of_maxV,">")))
}
#####result is assigned with runFSequence return
checkFDirectionChange(result)
############################



