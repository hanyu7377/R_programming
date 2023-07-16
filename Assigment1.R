#####Q1
vec1 <- c(1,2,3)
vec2 <- c(4,5,6,7,8)
sum <- vec1 + vec2
product <- vec1 * vec2
print(product)

#######Q2
norm_dis <- rnorm(15,10,2)

print(mean(norm_dis))
print(sd(norm_dis))
print(norm_dis)
?hist
hist(norm_dis)



set.seed(453)
 runif(20) 
 
 
#####Q3
input <- "I am a student"
####With sub function, it could replace the string 
output_1 <- sub("student","teacher",input)
print(output_1)
####With strsplit function, it could split the string into substring 
####Delimiter is space in this case
output_2 <- strsplit(input, split = " ")
print(output_2)


#######Q4
####sum function will calculate the summation of a given vector
####it will return a length equal to one vector
nums_vec <- c(1,2,3,4,5)
print(sum(nums_vec))
####cumsum function will calculate the cumulative sum of elements of a given vector
####it will return a vector, which has same length with given vector
###element of index1 = 1, element of index2 = 1 + 2, element of index3 = 1 + 2 + 3...and so on
print(cumsum((nums_vec)))

#####Q5
nums_vec <- c(4,5,7,1,7,8,3,9,1);
diff_lag3 <- diff(nums_vec,3)
print(diff_lag3)
diff_lag1 <- diff(nums_vec,1)
print(diff_lag1)
########Q6"
cumsum_vec <- vector("numeric",10)
nums_vec <- seq(from = 1, to = 10, by = 1)
sum <- 0
for (i in 1:length(cumsum_vec)){
  sum <- sum + nums_vec[i]
  cumsum_vec[i] <- sum
}
print(cumsum_vec)
##############Q7
#####1
names <- c(paste0("panel",1:10))

Star1 <- c(30.533, 28.180, 35.027, 26.340, 37.630, 28.294, 37.361, 37.191, 40.104, 34.394)
Star2 <- c(26.564, 226.673, 26.770, 16.996, 29.518, 16.169, 24.545, 39.584, 25.173, 33.518)
names(Star1) <- c(paste0("panel",1:10))
names(Star2) <- c(paste0("panel",1:10))
print("For Star1")
print(Star1)
print("For Star2")
print(Star2)
####2
print(paste0("Average radiation from Star1 is ", mean(Star1)))
print(paste0("Average radiation from Star2 is ", mean(Star2)))
####3 










########Q8
###create an inte_vec, which has 500 values lie in the range between 20 to 100

inte_vec <- sample(20:100,500,replace=TRUE)
print(inte_vec)
hist(inte_vec)
calculate_for_Q8 <- function(input_vec){
  if (length(input_vec) == 0){#####if input_vec is empty, return NA
    return(NA)
  }
  ###create output_vec, which has same length with input_vec, to store the values after calculation#####
  output_vec <- vector("numeric",length(input_vec))
  for (i in 1:length((input_vec))){
    if (i %% 2 == 1){#####if index is odd, do this operation
      answer <- input_vec[i] ** 0.5
      output_vec[i] <- round(answer,digit = 2)
    }else {####if index is even, do this operation
      answer <- input_vec[i] ** 2
      output_vec[i] <- round(answer,digit = 2)
    }
  }
  return (output_vec)
}

print(calculate_for_Q8(inte_vec))

####Given a empty_vec, it will return NA
empty_vec <- c()
print(calculate_for_Q8(empty_vec))