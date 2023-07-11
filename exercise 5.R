class_avr <- function() {
  columns <- c("Name", "Assign_1", "Assign_2", "Assign_3", "Assign_4", "Final")
  rows <- c("John", "Mary", "Tom", "Wendy", "Jack", "An", "Haung")
  Data_Frame <- setNames(data.frame(matrix(nrow = length(rows),ncol = length(columns))), columns)
  weight_vec <- c(Assign_1 = 0.2, Assign_2 = 0.25, Assign_3 = 0.3, Assign_4 = 0.25)
  for (i in 1:length(rows)) {
    print(paste("Please input", rows[i], "score and the score should be given out of 100 for each!"))
    Data_Frame[i, columns[1]] <- rows[i]
    final_grade <- 0
    
    for (j in 2:5) {
      score <- as.integer(readline(paste(columns[j], ": ")))
      Data_Frame[i, columns[j]] <- score
      tmp <- score * weight_vec[columns[j]]
      final_grade <- final_grade + tmp
     
    }
    
    Data_Frame[i, columns[6]] <- final_grade
    print(Data_Frame[,])  
  }
  class_average <- mean(Data_Frame[, columns[6]])
 
  return(class_average)
}
print(paste("The class average is: ", class_avr()))


print(11> 10)
