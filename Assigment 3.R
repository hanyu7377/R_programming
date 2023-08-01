#### Q1
#### A recursive list is a list that contains other lists
#### It could be used to store more complex data structures, 
#### such as tree structure or hierarchical data
#### Data could be Heterogeneous

#### create a list to store Teacher's name, age and the objects, which categorized into language, science, 
#### Humanities and art
Course <- list(Teacher = c("John","Brown","Mandy","Jane"),
               Age = c("John" = 34,"Brown" = 29,"Mandy" = 54,"Jane" = 26),
               John = list("Chinese","English"), 
               Brown =list("Physics","Chemistry","Math"),
               Mandy = list("History","geography"),
               Jane = list("Art","Music","Physical"))

Course;
#### get all teacher's name and age
Course[1];
Course[2];
Course$Teacher;
Course$Age;
### access objects taught by John
Course[3];
Course$John;
### access objects taught by Brown
Course[4];
Course$Brown;


### To access the first element of first sub-list in the Course list
### we could use bracket with index 
### we could also use namee list to access data 
### use bracket with vector is also available
#### access all element from John sub-list of Course,respectively
Course[[3]][[1]];
Course$John[[2]];
Course[[c(3,1)]];
Course[[c(3,2)]];

#### access all element from Jane sub-list of Course, respectively
Course[[6]][[1]];
Course[[6]][[2]];
Course$Jane[[3]];
Course[[c(6,1)]];
Course[[c(6,2)]];
Course[[c(6,3)]];

### Q2
## create df1 data frame
df1 <- data.frame(name = c("judy","Tom","jerry","andy","Roy","wendy"),
                  age = c(17,20,21,13,27,24),
                  marks = c(1,2,3,4,5,6),
                  gender = c("F","M","M","M","F","F"))
df1;
##### mixed-style
##### get the row data corresponding to the 
##### value of the "marks" column in the df1 
##### that is greater than 2
df1[df1$marks > 2, ];


##### list-style
##### use the $ to access "age" column,
##### which return vector containing all values of "age" column
df1$age;

#### subset style
#### extract row data from df1 base on condition age > 19
#### first argu is original df1 and second argu is the conditional expression
#### It return subset of df1
subset(df1,df1$age>19);

#### matrix-style
#### Specify row and column index
#### return value at first row and fourth column
df1[1,4];



####Q3 
#####

df1 <- data.frame(city = c("Tokyo", "New York"), continent =
                     c("Asia","North America"), language = c("Japanese","English"))

df2 <- data.frame(city = c("New York", "Tokyo"), 
                  population = c(33000000,125000000))

df3 <- data.frame(city = c("Berlin", "Moscow"),
                  continent = c("Europe", "Asia"), language = c("German","Russian"))

##### use cbind() or rbind function to combine existing df
##### combine two df horizontally
#### but there are two duplicated column "city"
#### moreover, the column "city" didn't match 
cbin_df <- cbind(df1,df2)
cbin_df;

#### merge() function could perform merge based on the same key
#### in df1 and df2
#### when the two df contains different attributes, it will be properiate
#### use merge() function to merge.
merge_df <- merge(df1,df2)
merge_df;


####cbind() and rbind() are suitable for hori or verti concatenation
#### when df have same number of rows or columns
rbin_df <- rbind(df1,df3)
rbin_df;




#####Q4
####

#####create data frame
trees <- data.frame(
  name = c("Oak","Maple","Banana","Apple"),
  age = c(49,34,65,22),
  height = c(12.4,13.5,6.6,7.9)
)
trees;
####save df as "trees.csv" file
write.csv(trees,"trees.csv",row.names = FALSE)
#### load tree.csv file
load_data <- read.csv("trees.csv")
load_data;


##### Q5
##### categorical variable could represented data in
##### categories or group.

##### We use categorical variable to represent qualitative data,
##### where each value represents a distinct category or attribute.
shape_data <- c("square","triangle","triangle",
                "star","star","circle",
                "square","circle","cardioid")
### use factor() function to obtain categorical variable.
### At here, shape data is nominal.
### It means we can't order it.
categorical_shape_data <- factor(shape_data)
categorical_shape_data;

### for ordinal data, we can order it.
### take grade data for example.
### we could group the grade data based on defined grade range 
### into different rank and the rank could be ordered.
grade_data <- c(45,78,90,70,100,23,12,80,65,77,89,90,91)
rank <- cut(grade_data,
            breaks = c(0,60,70,80,90,100),
            labels = c("F","C","B","A","A+"))
rank;




#####Q6
### represent the Data with categorical variable
Data <- c(1,3,2,2,2,1,3,1)
catVar <- factor(Data,levels = 1:3,
                 labels = c("Pass", "Fail", "Dropped"))
levels(catVar);
catVar;
#### Add the new level "Merit"
levels(catVar) <- c(levels(catVar),"Merit")
levels(catVar);
catVar;

#### Add two additional data items
#### categorize 4 to Merit category
catVar <- factor(c(Data, 4,3), labels = levels(catVar));
catVar;

catVar;