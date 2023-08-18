### load the data
BurgundySip_df <- read.csv(file.choose(),na.strings = c("","NA"," NA ","N.V."))
BurgundySip_df;
View(BurgundySip_df)
str(BurgundySip_df)




##### treat SN as our candidate key
top_bottom <- duplicated(BurgundySip_df[c("SN")]);



no_duplicated_BurgundySip_df_1 <- BurgundySip_df[!top_bottom,];
na_matrix <- is.na(no_duplicated_BurgundySip_df_1)

col_na_counts <- colSums(na_matrix);
col_na_counts;
sum(col_na_counts);
####### We choose bottom to top to delete the duplicated entries
####### because it will keep less NA value within entire data frame
bottom_top <- duplicated(BurgundySip_df[c("SN")],fromLast =TRUE);
no_duplicated_BurgundySip_df_2 <- BurgundySip_df[!bottom_top,];
na_matrix <- is.na(no_duplicated_BurgundySip_df_2)

col_na_counts <- colSums(na_matrix);
col_na_counts;
sum(col_na_counts);


####delete duplicated data 
BurgundySip_df <- BurgundySip_df[!bottom_top,]
View(BurgundySip_df)




### create train data frame, which doesn't include any NA value in DN and AL
### After we obtain the equation, we go back to original data frame 
### to predict the NA value
train_data <- BurgundySip_df[!is.na(BurgundySip_df$DN) & 
                            !is.na(BurgundySip_df$AL) 
                            ,]
 

#### Both pearson and spearman test tell us there is highly negative relationship
#### between AL and DN.
#### Through the background of science, the alcoholic rate will effect the density of liquid
shapiro.test(BurgundySip_df$AL)
shapiro.test(BurgundySip_df$DN)
cor.test(train_data$AL,train_data$DN,method = "pearson")
cor.test(train_data$AL,train_data$DN,method = "spearman")
library(ggplot2)
ggplot(data = train_data, aes(x = AL, y = DN)) +
  geom_point()
##### Through scatter plots and correlation tests, 
##### we have decided to use simple variable linear regression as our model. 
##### However, to be cautious, we should still check for outliers.
#### Through box plot, we could observe a little bit outlier
shapiro.test(BurgundySip_df$AL)
shapiro.test(BurgundySip_df$DN)
boxplot(train_data$AL, main = "Box Plot of AL column", xlab = "AL", ylab = "Value")
boxplot(train_data$DN, main = "Box Plot of DN column", xlab = "DN", ylab = "Value")
### count outlier
count_outliers <- function(data, threshold = 1.5) {
  target <- data
  q1 <- quantile(target, 0.25)
  q3 <- quantile(target, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  
  outliers <- target[target < lower_bound | target > upper_bound]
  num_outliers <- length(outliers)
  
  return(num_outliers)
}
count_outliers(train_data$AL) ### 56 outlier in AL
count_outliers(train_data$DN) ### 9 outlier in DN
#### With over 3000 data points in the original dataset, 
#### the proportion of outliers is relatively low within the entire population. 
#### By observing scatter plots, it's evident that the outliers are not significantly distant from the main population. 
#### Therefore, linear regression remains applicable.



### Fit AL and DN with simple variable linear regression and 
### fill the missing value in both DN and AL columns

View(train_data)

fit <- lm(DN ~AL,data = train_data)
summary(fit)
equation <- coef(fit);
equation;
slope <- equation[2]
intercept <- equation[1]
library(ggplot2)
ggplot(data = train_data, aes(x = AL, y = DN)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "AL", y = "DN", title = "Scatter Plot for DN and AL variables")+
  geom_text(aes(label = paste("y =", round(slope, 3), "x +", round(intercept, 3))),
          x = 12.5, y = 0.998, color = "black")+
theme(plot.title = element_text(size = 12,hjust = 0.5))

#### create a function to fill the NA based on the linear model
fill_na <- function(x, y, equation) {
  if (is.na(x) && !is.na(y)) {
    x <- ((y - equation[1]) / equation[2])
    return(x)
  } else if (is.na(y) && !is.na(x)) {
    y <- equation[2] * x + equation[1]
    return(y)
  }
}

for (i in 1:nrow(BurgundySip_df)) {
  ## x is independent variable
  ## y is dependent variable 
  x <- BurgundySip_df$AL[i]
  y <- BurgundySip_df$DN[i]
  if (is.na(x) && is.na(y)) {
    ### if both x and y are NA , we assume DN is equal to mean of DN column
    ### and we could get AL value through model
    BurgundySip_df$DN[i] <- mean(BurgundySip_df$DN, na.rm = TRUE) 
    BurgundySip_df$AL[i] <- fill_na(x, BurgundySip_df$DN[i], equation)
  } else if (is.na(x)) {
    BurgundySip_df$AL[i] <- fill_na(x, y, equation)
  } else if (is.na(y)) {
    BurgundySip_df$DN[i] <- fill_na(x, y, equation)
  } else {
    print("No NA value in AL or DN")
    next
  }
}
View(BurgundySip_df)

# REG, TP are independent variables and can be a factor value
# Perform the factorization
BurgundySip_df[c("REG","TP")] <- 
  lapply(BurgundySip_df[c("REG","TP")],FUN = factor);

str(BurgundySip_df[c("REG","TP")]);
summary(BurgundySip_df[c("REG","TP")]);


# REG have 76 level of factors and TP have 21 ones
# There are some NA value in REG (2) and TP(154 < 5% total number of observations 3666)

# => Limit number of factor in REG and TP to 10 and change the rest to "Other", 
# the NA values will also be changed to "Other"
# Create function to limit number of factor in REG & TP.
convert_Other <- function(factor_var){
  top10 <- names(sort(table(factor_var), decreasing = T)[1:10]);
  factor_var <- as.character(factor_var)
  factor_var[!(factor_var %in% top10)] <- "Other"
  factor_var <- factor(factor_var)
  return(factor_var)
}

# Keep the top 10 factor with top frequency and convert the rest to "Other"
BurgundySip_df$TP <- convert_Other(BurgundySip_df$TP)
BurgundySip_df$REG <- convert_Other(BurgundySip_df$REG);
summary(BurgundySip_df$REG); 
summary(BurgundySip_df$TP);

str(BurgundySip_df)
summary(BurgundySip_df)

# Perform tests 
library(dplyr);
library(PerformanceAnalytics);

BurgundySip_omit <- na.omit(BurgundySip_df)
summary(BurgundySip_omit);
str(BurgundySip_omit)

# Shapiro
BurgundySip_omit %>% 
  select_if(is.numeric) %>%
  lapply(FUN = shapiro.test);

# => YR, RT, NUMR, PR, BD, ACD are NOT normal distribution

# Correlation Chart
BurgundySip_omit %>% 
  select_if(is.numeric) %>%
  chart.Correlation();

# => BD and ACD are Strong Independent variables
# => BD and ACD have value from 1-5 => could be factors

BurgundySip_df[c("BD","ACD")] <- 
  lapply(BurgundySip_df[c("BD","ACD")],FUN = factor);

str(BurgundySip_df);
summary(BurgundySip_df[c("BD","ACD")]);

### Handle NAs in BD and ACD:

#Chi-Square
BurgundySip_df %>% 
  select_if(is.factor) %>%
  table() %>%
  summary();
# There are no relatioship between the factors

# => Solution: Use the mode (most frequent category)
# Function to get the mode 
get_mode <- function(df){
  df <- table(df)
  df_mode <- names(df[which.max(df)])
  return(df_mode)
  
}

# Impute the most frequent category to the NAs in BD and ACD
BurgundySip_df$BD[is.na(BurgundySip_df$BD)] <- get_mode(BurgundySip_df$BD)
BurgundySip_df$ACD[is.na(BurgundySip_df$ACD)] <- get_mode(BurgundySip_df$ACD)

summary(BurgundySip_df)





##### prediction of RSG
##### use multiple variable regression 
##### Take BD,ACD,AL,DN,YR as our candidate key to test.
##### convert BD and ACD back into numeric data type
BurgundySip_df$BD <- as.numeric(BurgundySip_df$BD)
BurgundySip_df$ACD <- as.numeric(BurgundySip_df$ACD)
fit <- lm(RSG ~ BD + RT + AL + DN + YR ,data = BurgundySip_df)
summary(fit)### We could observe BD and RT effect more on the RSG
fit <- lm(RSG ~ BD + RT  ,data = BurgundySip_df)
summary(fit)
equation <- coef(fit);
equation;
intercept <- equation[1]
BD <- equation[2]
RT <- equation[3]
print(BurgundySip_df$RSG[1])
#####Fill the na value in RSG column
for (i in 1:nrow(BurgundySip_df)) {
  print(is.na(BurgundySip_df$BD[i]))
  if (is.na(BurgundySip_df$RSG[i])){
    BurgundySip_df$RSG[i] <- intercept + BD*BurgundySip_df$BD[i] + RT*BurgundySip_df$RT[i]

    
  }
}

View(BurgundySip_df)


# Goal : To detect missing values and outliers in Price and replace them using non-linear regression model

library(dplyr)

# 1. Observing correlation between RT and PR, which turned out to be 1.00
BurgundySip_df %>% 
  select(RT,PR) %>%
  chart.Correlation(method = "spearman");

# 1-1. Missing values in PR
# Checking if there is any missing value in PR. There are 58 missing values.
sum(is.na(BurgundySip_df$PR))

# Building non-linear model for imputing missing values in PR
RT_PR_NA_Train <- (BurgundySip_df %>% 
                     select(RT,PR))[!(is.na(BurgundySip_df$RT)|is.na(BurgundySip_df$PR)),]
RT_PR_NA_Train;
PR_NA_forPredict <- (BurgundySip_df %>% select(RT,PR))[is.na(BurgundySip_df$PR),]
PR_NA_forPredict;

RT_PR_NA_loess <- loess(PR ~ RT , data = RT_PR_NA_Train)
RT_PR_NA_loess

PR_NA_predict <- predict(RT_PR_NA_loess, newdata = data.frame("RT"=PR_NA_forPredict$RT))

PR_NA_predict;
# Replacing missing PR with the predict calculated above.
BurgundySip_df[is.na(BurgundySip_df$PR),"PR"] <- PR_NA_predict
View(BurgundySip_df)

# Testing
BurgundySip_df %>% 
  select(RT,PR) %>%
  chart.Correlation(method = "spearman");



# 1-2. Outliers in PR


# According to the result of Shapiro-Wilk normality test, p-value < 2.2e-16
# The price doesn't follow normal distribution.
shapiro.test(BurgundySip_df$PR)

# Thus, IQR method will be used for detecting outliers.
PR_Q1 <- quantile(BurgundySip_df$PR,0.25, na.rm=T)
PR_Q3 <- quantile(BurgundySip_df$PR,0.75, na.rm=T)

# Observing rows with outlier price
PROutlier <- BurgundySip_df[BurgundySip_df$PR < PR_Q1-1.5*IQR(BurgundySip_df$PR,na.rm=T) | BurgundySip_df$PR > PR_Q3+1.5*IQR(BurgundySip_df$PR,na.rm=T),]
PROutlier;

# Building non-linear model for replacing outliers in PR 
RT_PR_Outlier_Train <- BurgundySip_df[BurgundySip_df$PR > PR_Q1-1.5*IQR(BurgundySip_df$PR,na.rm=T) & BurgundySip_df$PR < PR_Q3+1.5*IQR(BurgundySip_df$PR,na.rm=T),] %>% select(RT,PR)
RT_PR_Outlier_Train
chart.Correlation(RT_PR_Outlier_Train)

PR_Outlier_forPredict <- BurgundySip_df[BurgundySip_df$PR < PR_Q1-1.5*IQR(BurgundySip_df$PR,na.rm=T) | BurgundySip_df$PR > PR_Q3+1.5*IQR(BurgundySip_df$PR,na.rm=T),] %>% select(RT,PR)


RT_PR_Outlier_loess <- loess(PR ~ RT , data = RT_PR_Outlier_Train, control=loess.control(surface="direct"))
RT_PR_Outlier_loess

PR_Outlier_predict <- predict(RT_PR_Outlier_loess, newdata = data.frame("RT"=PR_Outlier_forPredict$RT))
PR_Outlier_predict

# Replacing PR outliers with the predict calculated above.
BurgundySip_df[BurgundySip_df$PR < PR_Q1-1.5*IQR(BurgundySip_df$PR,na.rm=T) | BurgundySip_df$PR > PR_Q3+1.5*IQR(BurgundySip_df$PR,na.rm=T),"PR"] <- PR_Outlier_predict

# Testing
BurgundySip_df %>% 
  select(RT,PR) %>%
  chart.Correlation(method = "spearman");

View(BurgundySip_df)


table(BurgundySip_df$TP)
sum(is.na(BurgundySip_df$TP))

###### The statistics of the type of wine
table(BurgundySip_df$TP)


### plot top 10 type of wine this retailer mainly promote
#### This retailer mainly boost their red wine
library(ggplot2)
wine_table <- sort(table(BurgundySip_df$TP),decreasing = TRUE)
wine_table;
Top_10 <- head(wine_table,10)
Top_10;
Top_10_df <- data.frame(WineType = names(Top_10), Frequency = as.numeric(Top_10))
Top_10_df;
ggplot(Top_10_df, aes(x = WineType, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Count the type of the wine",
       x = "Type",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))


###### calculate average RT base on the year, and we could investigate the variation
###### of average RT from year 1910 to 2021


BurgundySip_df <- transform(BurgundySip_df,
                    TotalScore = BurgundySip_df$RT * BurgundySip_df$NUMR)
View(BurgundySip_df)
aver_RT_by_year <- aggregate(cbind(NUMR,TotalScore)~ YR, data  = BurgundySip_df,FUN = sum)
aver_RT_by_year;
aver_RT_by_year <- transform(aver_RT_by_year,
                             aver_RT = aver_RT_by_year$TotalScore / aver_RT_by_year$NUMR)
aver_RT_by_year;

####### plot average RT base on the year
####### histogram plot indicate as the years
####### progress,there is a downward trend in 
####### the wine ratings, especially after the year 2000
####### This suggests that wines become more favored as they age
library(ggplot2)
ggplot(aver_RT_by_year, aes(x = YR, y = aver_RT)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Histogram of aver_RT by YR",
       x = "YR",
       y = "aver_RT") +
  theme(plot.title = element_text(hjust = 0.5)) 

############ plot average RT base on the REG and year
############ We could investigate the variety of the RT of specific REG over year
aver_RT_by_REG_YR <- aggregate(cbind(NUMR,TotalScore)~ REG + YR, data  = BurgundySip_df,FUN = sum)
aver_RT_by_REG_YR;
aver_RT_by_REG_YR <- transform(aver_RT_by_REG_YR,
                               aver_RT = aver_RT_by_REG_YR$TotalScore / aver_RT_by_REG_YR$NUMR)
aver_RT_by_REG_YR;

table(aver_RT_by_REG_YR$REG)
sorted_table <- sort(table(aver_RT_by_REG_YR$REG), decreasing = TRUE)
print(sorted_table[1:5])

plot_base_on_reg <- function(reg){
  reg <- aver_RT_by_REG_YR[aver_RT_by_REG_YR$REG == as.character(reg),]
  ggplot(reg, aes(x = YR, y = aver_RT)) +
    geom_line(color = "green") + 
    labs(title = paste0("Variation of RT in ", unique(reg$REG), " region"),
         x = "YR",
         y = "aver_RT") +
    theme(plot.title = element_text(size = 12,hjust = 0.5))+
    ylim(3.9, 5) 
}
plot1 <- plot_base_on_reg("Ribera del Duero")
plot2 <- plot_base_on_reg("Toro")
plot3 <- plot_base_on_reg("Rioja")
plot4 <- plot_base_on_reg("Priorato")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4,ncol = 2)
############ plot average RT base on the Winery and year
############ We could investigate the variety of the RT of specific Winery over year
aver_RT_by_wine_YR <- aggregate(cbind(NUMR,TotalScore)~ WINE + YR, data  = BurgundySip_df,FUN = sum)
aver_RT_by_wine_YR;
aver_RT_by_wine_YR <- transform(aver_RT_by_wine_YR,
                                aver_RT = aver_RT_by_wine_YR$TotalScore / aver_RT_by_wine_YR$NUMR)
aver_RT_by_wine_YR;

sorted_table <- sort(table(aver_RT_by_wine_YR$WINE), decreasing = TRUE)
print(sorted_table[1:5])

plot_base_on_wine <- function(wine){
  wine <- aver_RT_by_wine_YR[aver_RT_by_wine_YR$WINE == as.character(wine),]
  ggplot(wine, aes(x = YR, y = aver_RT)) +
    geom_line(color = "green") + 
    labs(title = paste0("Variation of RT in ", unique(wine$WINE), " Winery"),
         x = "YR",
         y = "aver_RT") +
    theme(plot.title = element_text(size = 8,hjust = 0.5)) +
    ylim(4.2, 5) 
}

plot1 <- plot_base_on_wine("Unico")
plot2 <- plot_base_on_wine("Valbuena 5o")
plot3 <- plot_base_on_wine("Unico Reserva Especial Edicion")
plot4 <- plot_base_on_wine("Priorat")
plot5 <- plot_base_on_wine("L'Ermita Velles Vinyes Priorat")

library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4,plot5, ncol = 3)



#####################cluster test to investigate the potential effect on RT

fit <- kmeans(BurgundySip_df$RT, 4) 
fit;

BurgundySip_df$RT_Cluster <- fit$cluster;
BurgundySip_df;
View(BurgundySip_df)
###### check the mean based on each cluster before label
###### the average for each cluster must be calculated first to determine 
###### which group these four levels are classified into. 
###### After running these two lines, assign the order for the labels below based on the results.

check <- aggregate(RT ~ RT_Cluster,data = BurgundySip_df, FUN = mean)
check;

BurgundySip_df$RT_Cluster <- factor(BurgundySip_df$RT_Cluster, 
                               levels = 1:4,
                               labels = c("medium","Best","not popular","good"))

View(BurgundySip_df)
library(plotly)
RT_cluster_plot <- plot_ly(x = BurgundySip_df$RSG, y = BurgundySip_df$AL, 
             z = BurgundySip_df$DN, type = "scatter3d", mode = "markers", 
             color = BurgundySip_df$RT_Cluster,
             marker = list(size = 6, opacity = 0.7))

RT_cluster_plot <- RT_cluster_plot %>% layout(scene = list(xaxis = list(title = "Residual sugar level",
                                            titlefont = list(size = 12)),
                               yaxis = list(title = "Alcoholic percentage",
                                            titlefont = list(size = 12)),
                               zaxis = list(title = "Typical density",
                                            titlefont = list(size = 12)))
)
print(RT_cluster_plot)
table(BurgundySip_df$RT_Cluster)

######

fit <- kmeans(BurgundySip_df$PR, 3) 
fit;
BurgundySip_df$PR_Cluster <- fit$cluster
View(BurgundySip_df)
###### check the mean based on each cluster before label
check <- aggregate(PR ~ PR_Cluster,data = BurgundySip_df, FUN = mean)
check;

BurgundySip_df$PR_Cluster <- factor(BurgundySip_df$PR_Cluster, 
                                    levels = 1:3,
                                    labels = c("medium","low","high"))

View(BurgundySip_df)

PR_cluster_plot <- plot_ly(x = BurgundySip_df$RT, y = BurgundySip_df$PR, 
                          type = "scatter", mode = "markers", 
                           color = BurgundySip_df$PR_Cluster,
                           marker = list(size = 6, opacity = 0.7))

PR_cluster_plot <- PR_cluster_plot %>% layout(xaxis = list(title = "RT",
                                                      titlefont = list(size = 12)),
                                              yaxis = list(title = "PR",
                                                      titlefont = list(size = 12)))

print(PR_cluster_plot)

table(BurgundySip_df$PR_Cluster)
View(BurgundySip_df)






