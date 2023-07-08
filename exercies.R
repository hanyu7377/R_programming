#######exercise 4.1
vec1 <- seq(from = 18, by =6, length.out = 12)
vec1;
vec2 <- seq(from = 14, by = 7 , length.out = 7)
vec2;
########exercise 4.2
result1 <- vec1 + max(vec2)
result1;
result2 <- vec1 * mean(vec2)
result2;
result3 <- vec1 * vec2
result3
########exercise 5
score_input <- function(num){
  score_title <- c("Assign_1:", "Assign_2:","Assign_3:","Assign_4:")
  weight_vec <- c(Assign_1 = 0.2,Assign_2 =0.25, Assign_3 = 0.3,Assign_4 = 0.25)
  score_vec <- vector("numeric",num)
  final_grade <- 0
  for (i in 1:num){
    score <- as.integer(readline(score_title[i]))
    score_vec[i] <- score
    tmp = score_vec[i] * weight_vec[names(weight_vec)][i]
    final_grade = final_grade + tmp
  }
  return (final_grade)
}
for (i in 1:7){
  student_name <- c("John's", "Mary's","Tom's", "Wendy's","Jack's","An's","Haung's")
  print(paste("Please input",studen_name[i], "score"))
  print(paste(student_name[i], "final score is: ", score_input(4)))
}
######exercise 6
Information <- c(EMP_ID = 1234, Age = 18,Height = 1.76, Weight = 70)
calculateBMI <- function(Height,Weight){
  Height <- as.numeric(readline("Please input your height in meter: "))
  Weight <- as.numeric(readline("Please input your weight in kilogram: "))
  BMI = Weight/(Height** 2)
  return (BMI)
}
print(paste("Your BMI is: " , calculateBMI(height,weight)))
#################
