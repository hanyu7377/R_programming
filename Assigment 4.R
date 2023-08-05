#####################Q1
#### CustomerID CustomerName Location Phone ProductName Price SalesDate Qty

# Download the following CSV files onto your local computer. 
#1.Create 3 data frames to load data from the 3 CSV files into R.

customers <- read.csv("D:\\RData\\Assignment 4\\Customer-1.csv", stringsAsFactors = T);
products <- read.csv("D:\\RData\\Assignment 4\\Product-1.csv", stringsAsFactors = T);
sales <- read.csv("D:\\RData\\Assignment 4\\Sales-1.csv", stringsAsFactors = T);

str(customers);
str(products);
str(sales);
sales;
customers;
products;

#2. Generate a new data frame (customerOrders) with the following variables:
customerOrders <- merge(customers,sales,by.x = "customer_id",by.y = "CustomerID");
customerOrders;
customerOrders <- merge(customerOrders,products,by.x = "ProductID",by.y = "ProductID");
customerOrders;
str(customerOrders);
#### delete unnecessary columns in customerOrders dataframe
customerOrders$rating <- NULL
customerOrders$ProductType<- NULL
customerOrders$Warranty<-NULL
customerOrders$ProductID<-NULL



str(customerOrders);

customerOrders <- subset(customerOrders, select = c("customer_id","customer_name","location", "phone", 
                                                    "ProductName", "Price", "SalesDate", "Qty"));

str(customerOrders);
customerOrders;

#3. Add the calculated variable TotalSale to customerOrders (TotalSale = Price * Qty)
# customerOrders[["TotalSale"]] <- customerOrders$Price * customerOrders$Qty;
customerOrders <- transform(customerOrders, TotalSale= Price * Qty);
customerOrders; str(customerOrders);


#4. Generate a default list to profile the sales of a customer. 
# This list is called defaultCusProfile, and it must follow the following structure and NULL/NA values.
#a. customerID (a single integer value)
#b. customerName (a single character value)
#c. location (a single character value)
#d. phone (a single character value)
#e. productsBaught (a vector listing the products the customer has bought)
#f. totalSalesValue (a single numeric value with two decimal places)

defaultCusProfile <- list(customerID = as.integer(),
                          customerName = as.character(),
                          location = as.character(),
                          phone = as.character(),
                          productsBaught = c(),
                          totalSalesValue = round(as.numeric(),2));

defaultCusProfile;





profileCustomers<- function(cusIDs){
  if (!all(cusIDs >= 0)){
    return (print("each value in cusIDs must be positive"))
  }
  empty_defaultCusProfile <- defaultCusProfile 
  all_profile <- list()
  for (i in 1:length(cusIDs)){
    tmp_df <- customerOrders[customerOrders$customer_id == cusIDs[i],]
    defaultCusProfile$customerID = unique(tmp_df$customer_id);
    defaultCusProfile$customerName <- unique(tmp_df$customer_name);
    defaultCusProfile$location <- unique(tmp_df$location);
    defaultCusProfile$phone <- unique(tmp_df$phone);
    defaultCusProfile$productsBaught <- unique(tmp_df$ProductName)
    defaultCusProfile$totalSalesValue <- sum(tmp_df$TotalSale)
    all_profile[[i]] <- defaultCusProfile
    defaultCusProfile <- empty_defaultCusProfile 
  }
  return (all_profile)
}
###demonstrate 

profileCustomers(c(1,8));


#####################Q2

### load the data
weatherDS <- read.csv(file.choose())
weatherDS;

#### Based on the command below, we choose year and month
#### as the candidate key to filter the duplicated data
#### combines these two column could identify uniquely each row
#### in the table

#### 1.duplicated comes from data entry error
#### 2.duplicated data maybe comes from different sources
#### 3.The database can't allow the user to change the duplicated data 
#### because the company want to keep history

### Can't just consider the year or month alone.
### because there are 12 month in same year, the year must repeat.
### similarly, the same month data will also belong to different years.

top_bottom <- duplicated(weatherDS[c("year","month")]);
bottom_top <- duplicated(weatherDS[c("year","month")],fromLast = TRUE);
weatherDS[bottom_top|top_bottom, ];





#### Remove the duplicate entries in weatherDS
weatherDS <- weatherDS[!top_bottom, ]
weatherDS;


#####factorize 
factorize <-factor(weatherDS$month, levels = month.abb)
factorize;


###### add season column 
month_to_number <- as.numeric(factorize)
month_to_number;
seasons <- cut(month_to_number,
               breaks = c(0,2,5,8,11,12),
               labels = c("winter", "spring","summer","fall","winter") )

seasons;

weatherDS <- transform(weatherDS,
                 temperatureF = weatherDS$temperature*1.8 + 32,
                 precipitationCm = weatherDS$precipitation/10,
                 season = seasons)
weatherDS;


#####summarize

#i. The average temperature and precipitation of this city.
#ii. The standard deviation of temperature and precipitation of this city.
#iii. The maximum temperature and precipitation of this city.
#iv. The minimum temperature and precipitation of this city.
summary_weatherDS <- data.frame(
  aver_tmp = mean(weatherDS$temperature),
  aver_preci = mean(weatherDS$precipitation),
  std_tmp = sd(weatherDS$temperature),
  std_preci = sd(weatherDS$precipitation),
  max_tmp = max(weatherDS$temperature),
  max_preci = max(weatherDS$precipitation),
  min_tmp = min(weatherDS$temperature),
  min_preci = min(weatherDS$precipitation)
)
summary_weatherDS;

####### Exhibit the monthly average temperature and precipitation
####### for the period of consideration.
weatherDS$month <- factor(weatherDS$month, levels = month.abb)
weatherDS_m <- aggregate(weatherDS[c("temperature","precipitation")],by=list(Month =weatherDS$month), FUN=mean);
weatherDS_m;
names(weatherDS_m)[2:3] <- c("avr_temp","avr_precipi"); ###renew the name of attribute
weatherDS_m;


##### exhibit the seasonal average, maximum, and minimum 
##### of temperature and precipitation for each year.

df1 <- aggregate(cbind(temperature,precipitation) ~ season , data = weatherDS,FUN = mean);
names(df1)[2:3] <- c("avr_temp","avr_precipi");
df1;

df2 <- aggregate(cbind(temperature,precipitation) ~ season , data = weatherDS,FUN = base::max);
names(df2)[2:3] <- c("max_temp","max_precipi");
df2;

df3 <- aggregate(cbind(temperature,precipitation) ~ season , data = weatherDS,FUN = min);
names(df3)[2:3] <- c("min_temp","min_precipi");
df3;
####### merge three df 
tmp_df <- merge(df2,df1,by.x = "season",by.y = "season");
tmp_df;
weatherDS_s <- merge(tmp_df,df3,by.x = "season",by.y = "season");
weatherDS_s;


####reorder the sequence of the observation in weatherDS_s
order <- c("spring", "summer", "fall", "winter")
weatherDS_s <- weatherDS_s[order(match(weatherDS_s$season, order)), ]
rownames(weatherDS_s) <- c(1,2,3,4);
weatherDS_s;

#### round some columns to make it more clear
weatherDS_s$avr_precipi <- round(weatherDS_s$avr_precipi,1)
weatherDS_s$avr_temp <- round(weatherDS_s$avr_temp,1)
weatherDS_s;
