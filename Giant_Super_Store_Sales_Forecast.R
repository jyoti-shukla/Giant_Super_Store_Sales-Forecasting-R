#************Giant_Super_Store_Sales  Forecasting************# ----

#************ Business understanding ************# ----
# Understanding data in term of business understanding 
# "Giant Super Store" is an online store super giant having worldwide operations.
# It takes orders and delivers across the globe and 
# deals with all the major product categories - consumer, corporate & home office.
# As a sales/operations manager-want to finalize plan & forecast the sales and the demand for the next 6 months, 
# that would help Giant Super Store manage the revenue and inventory accordingly.

# This case study is to build a efficient model to forecast the sales and the demand for the next 6 months,
# which would help Giant Super Store to manage the future revenue.

# Install and Load the required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tseries)
library(stats)
library(forecast)
require(graphics)

#************ Data understanding ************# ----
# Giant Super Store is a data set which has around 51,000 values. 
# Its a customer-centric data set , which has the data of all the orders that have been placed 
# through different vendors and markets,starting from the year 2011 till 2014.

# Loading the given data file
superstore<-read.csv("Giant_Super_Store.csv",stringsAsFactors = FALSE)
print(superstore)
str(superstore)                 #51290 obs. of  24 variables
#View(superstore)
summary(superstore)

# ************  data dictionary ************ # ------

# Order ID	      - Unique ID of the transaction 
# Order Date	    -	Date on which the order was placed
# Ship Date	      -	Date on which the shipment was made
# Ship Mode	      -	The mode of shipment (category)
# Customer ID	    -	The unique ID of the customer
# Customer Name	  -	Name of the customer
# Segment	        - The market segment to which the product belongs
# City	        	- City of the delivery address
# State		        - State of the delivery address
# Country	        - Country of the delivery address
# Postal Code	    -	Postal code of the delivery address
# Market		      - Market segment to which the customer belongs
# Region		      - Geographical region of the customer
# Product ID   	  -	Unique ID of the product
# Category	      -	Category of the product
# Sub-Category	  -	Sub-category of the product
# Product Name	  -	Name of the product
# Sales		        - Total sales value of the transaction
# Quantity	      -	Quantity of the product ordered
# Discount	      -	Discount percentage offered on the product
# Profit		      - Profit made on the transaction
# Shipping Cost	  -	Shipping cost incurred on the transaction
# Order Priority	-	Priority assigned to the order

#************ Data Cleaning ************#

# Checking for missing column names
names(superstore)
length(names(superstore))
# No missing column names

# Checking duplicates
which(duplicated(superstore))
# No duplicates

#Looking for Missing values in the dataframe
sapply(superstore, function(x) sum(is.na(x)))

# Column Postal.Code has 41296 NA's.Thus,removing from the dataset.
superstore$Postal.Code <- NULL

# Removing the Row.Id column from the dataset as it is not required for analysis
superstore<-superstore[,-1]

# Checking for blanks""
sapply(superstore, function(x) length(which(x == "")))
# No blanks

# Checking for Lower and uppercase issues
sapply(superstore, function(x) summary(factor(x)))
# No upper and lowercase issues

# Checking for columns with 1 unique value
superstore [sapply(superstore, function(x) length(unique(x)) == 1)]
# No columns with 1 unique value

# Changing Order.Date & Ship.Date to default R date format
superstore$Order.Date <- as.Date(superstore $Order.Date, format = "%d-%m-%Y")
superstore$Ship.Date  <- as.Date(superstore $Ship.Date, format = "%d-%m-%Y")

#************ Data Preparation ************# ----

# Combining 2 columns(Market , Segment) to create a new column Market_Segment in the dataset
summary(superstore)
superstore$Market.Segment <- paste(superstore$Market, superstore$Segment, sep = " ")

# segmenting the whole dataset into the 21 subsets based on the market and the customer segment level.
invisible(lapply(split(superstore, superstore$Market.Segment), function(x) {assign(paste0("Market.Segment", x$Market.Segment[1]), x, pos = .GlobalEnv)}))

# Extract the Month-Year from the Order.Date and create a column year for the aggregation of data
superstore$Year.Month <- format(as.Date(superstore$Order.Date,"%d-%m-%Y"), "%Y-%m")
#Aggregating data monthly on each attribute
superstore_agg <- superstore %>%       # Use dplyr for Aggregation
  group_by(Market.Segment, Year.Month) %>%
  summarise(., sum(Sales), # Total Sales Per Segment Per Market
            sum(Profit),   # Total Profit Per segment Per Market
            sum(Quantity), # Total Quantity per segment per Market
            sd(Profit)*100/abs(mean(Profit)) # Profit Co-efficient of Variation
  )
colnames(superstore_agg) = c("Market_Segment","Year_Month","Sales","Profit","Quantity","CV_Profit")


# Extract the Year from the Order.Date and create a column year for the aggregation of data
superstore$Year <- format(as.Date(superstore$Order.Date, "%d-%m-%Y"), "%Y")

superstore_agg_year <- superstore %>% # Use dplyr for Aggregation
  group_by(Market.Segment, Year) %>%
  summarise(., sum(Sales), # Total Sales Per Segment Per Market
            sum(Profit), # Total Profit Per segment Per Market
            sum(Quantity) , # Total Quantity per segment per Market
            sd(Profit)*100/abs(mean(Profit)) # Profit Co-efficient of Variation
  )
colnames(superstore_agg_year) = c("Market_Segment","Year","Sales","Profit","Quantity","CV_Profit")

#Data Plots
sales_profit <- superstore[,c("Profit","Sales","Market","Segment","Quantity")] %>% group_by(Market,Segment) %>% dplyr::summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(sales_profit) = c("Market","Segment","Sales","Profit","CV")
#check the  CV & Profit for 21 segments
sales_profit

ggplot(sales_profit, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(sales_profit, aes(Segment, Profit, fill=Segment)) + geom_bar(position = "dodge",stat = "identity")
ggplot(sales_profit, aes(Segment, CV, fill=Market)) + geom_bar(position = "dodge",stat = "identity")

# Market and Segment generating most profit
ggplot(sales_profit,aes(x=Market,y=Profit,fill=Segment))+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit")

####################$$$$$$$$$$$$$$$$$$$$$$$$$$$######################
top_profitable_MarketSeg_2011 <- superstore_agg_year %>% 
  group_by(Market_Segment, Year) %>%
  arrange(desc(Profit)) %>% 
  filter(Year == "2011")  %>% 
  head(n = 5)
# top_profitable_MarketSeg_2011$CV_Profit <- sd(top_profitable_MarketSeg_2011$Profit)*100/abs(mean(top_profitable_MarketSeg_2011$Profit)) # Profit Co-efficient of Variation
top_profitable_MarketSeg_2011 
# Market_Segment  Year    Sales   Profit Quantity CV_Profit
# <chr> <chr>    <dbl>    <dbl>    <int>     <dbl>
# 1  APAC Consumer  2011 299654.3 36246.08     3706  464.9002
# 2  APAC Corporate  2011 223712.0 31907.00     2395  384.0778
# 3  EU Consumer  2011 256073.6 26089.51     3186  587.2611
# 4  LATAM Consumer  2011 216559.4 24701.63     3366  476.6691
# 5  US Consumer  2011 266096.8 24319.85     4053 1037.9883


top_profitable_MarketSeg_2012 <- superstore_agg_year %>% 
  group_by(Market_Segment, Year) %>%
  arrange(desc(Profit)) %>% 
  filter(Year == "2012")  %>% 
  head(n = 5)

top_profitable_MarketSeg_2012 
# Market_Segment  Year    Sales   Profit Quantity CV_Profit
# <chr> <chr>    <dbl>    <dbl>    <int>     <dbl>
# 1  EU Consumer  2012 377547.8 50729.53     4630  436.3560
# 2  APAC Consumer  2012 404834.3 49075.52     4590  361.6032
# 3  US Consumer  2012 266535.9 28460.17     4272  791.3730
# 4  APAC Corporate  2012 218973.4 27326.85     2516  362.4091
# 5  LATAM Consumer  2012 229904.6 21321.84     4356  641.2060

top_profitable_MarketSeg_2013 <- superstore_agg_year %>% 
  group_by(Market_Segment, Year) %>%
  arrange(desc(Profit)) %>% 
  filter(Year == "2013")  %>% 
  head(n = 5)

top_profitable_MarketSeg_2013
# Market_Segment  Year    Sales   Profit Quantity CV_Profit
# <chr> <chr>    <dbl>    <dbl>    <int>     <dbl>
# 1  APAC Consumer  2013 504008.4 65642.99     5836  385.3726
# 2  EU Consumer  2013 383658.9 50601.07     4962  479.4650
# 3  US Consumer  2013 296295.5 35758.27     4896 1013.0766
# 4  APAC Corporate  2013 288625.7 35060.17     3239  395.3866
# 5  LATAM Consumer  2013 332723.5 34774.11     5901  559.9129

top_profitable_MarketSeg_2014 <- superstore_agg_year %>% 
  group_by(Market_Segment, Year) %>%
  arrange(desc(Profit)) %>% 
  filter(Year == "2014")  %>% 
  head(n = 5)

top_profitable_MarketSeg_2014
# Market_Segment  Year    Sales   Profit Quantity CV_Profit
# <chr> <chr>    <dbl>    <dbl>    <int>     <dbl>
# 1  APAC Consumer  2014 608256.6 71852.97     7282  467.9595
# 2  EU Consumer  2014 512435.9 61267.59     6763  439.8350
# 3  US Consumer  2014 332473.1 45580.92     6300  905.4786
# 4  EU Corporate  2014 337926.8 44445.18     4168  455.5397
# 5  LATAM Consumer  2014 354659.6 39835.35     6230  518.7465

#************ Exploratory DATA Analysis ************# -----
# we can cross verify the above data with EDA for each of the Market and customer segment level

# Check based on profit for each Year
ggplot(superstore_agg_year, aes(Year,Profit, fill=Market_Segment)) + 
  geom_bar(position = "dodge",stat = "identity")

ggplot(data = superstore_agg_year, aes(x = Year, y = Profit,fill=as.factor(Market_Segment))) + geom_bar(stat="identity",position="dodge") + facet_wrap(~Market_Segment,scales = "free")

# Check based on coefficient profit for each Year
ggplot(superstore_agg_year, aes(Year,CV_Profit, fill=Market_Segment)) +
  geom_bar(position = "dodge",stat = "identity")

ggplot(data = superstore_agg_year, aes(x = Year, y = CV_Profit,fill=as.factor(Market_Segment))) + geom_bar(stat="identity",position="dodge") + facet_wrap(~Market_Segment,scales = "free")

# So in all the 4 years of data the Market: 'APAC' and 'EU' in the customer segment level: 'Consumer'  
# are the the 2 most profitable and consistently profitable segments.


#************ MODELLING ************# ----

#************ Market: 'APAC'  And customer segment level: 'Consumer'************# 
APAC_Consumer <- subset(superstore_agg, superstore_agg$Market_Segment =="APAC Consumer" )
# ************APAC_consumer_sales ************# -----
# Conversion of APAC sales data to time series.
APAC_consumer_sales_ts <- ts(APAC_Consumer$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
# plot
plot(APAC_consumer_sales_ts)
# Smoothing the series - Moving Average Smoothing
Smoothed_APAC_consumer_sales <- stats::filter(APAC_consumer_sales_ts, # Here had to use stats::filter due tp plyr issue#
                                              filter=rep(1/3,3,method='convolution',
                                                         sides=2)) 
#As the smoothing removes the first and the last data values, they need to be filled in

#Smoothing left end of the time series
diff_1 <- Smoothed_APAC_consumer_sales[3] - Smoothed_APAC_consumer_sales[2]
Smoothed_APAC_consumer_sales[1] <- Smoothed_APAC_consumer_sales[2]- diff_1

#Smoothing right end of the time series
diff_2 <- Smoothed_APAC_consumer_sales[47] - Smoothed_APAC_consumer_sales[46]
Smoothed_APAC_consumer_sales[48] <- Smoothed_APAC_consumer_sales[47] + diff_2

#plot the smoothed sales curve
lines(Smoothed_APAC_consumer_sales,col='red',lwd=2)

#Building a model on the smoothed time series using classical decomposition
# Converting the Smoothed series into a data frame and adding the Year_Month column from the original sales data.
APAC_consumer_sales_df <- data.frame(cbind(APAC_Consumer$Year_Month,Smoothed_APAC_consumer_sales))

# Renaming the columns
colnames(APAC_consumer_sales_df) <- c("Month","Sales")
APAC_consumer_sales_df$sales <- as.numeric(as.character((APAC_consumer_sales_df$Sales)))

# Conversion of the filled in data frame to time series
APAC_consumer_sales.ts <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                             end=c(2014,12))

APAC_Consumer_sales_hw <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                             end=c(2014,12))

#Predicting the time series using Holtwinters algorithm for next 6 months.

HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw)

HoltWinters(APAC_Consumer_sales_hw)$fitted

APAC_Consumer_sales_hw_hw<-HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw_hw)
lines(predict(APAC_Consumer_sales_hw_hw,n.ahead=46),col=2)
predict(APAC_Consumer_sales_hw_hw,n.ahead=6)

# predicted sales values based on Holtwinters
#       Jan      Feb      Mar      Apr      May      Jun
# 2015 72940.53 67325.78 63380.99 67364.03 73619.40 71042.47

#-------------------------------------------------------------------------------#
# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_sales.ts)-ntest
train.ts <- window(APAC_consumer_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_sales.ts,start=c(2011,nTrain+1),
                   end=c(2011,nTrain+ntest))
#-------------------------------------------------------------------------------# 

# Curve fitting for Linear Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red") 
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
train.lm.forecast
# Point         Forecast     Lo 0     Hi 0
# Jul 2014       50393.73 50393.73 50393.73
# Aug 2014       47828.49 47828.49 47828.49
# Sep 2014       57587.85 57587.85 57587.85
# Oct 2014       60009.38 60009.38 60009.38
# Nov 2014       62519.22 62519.22 62519.22
# Dec 2014       57393.64 57393.64 57393.64

# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) 
APAC_consumer_accuracy

#-----------------------------------#  
# FOR LINEAR MODEL MAPE IS 10.2114
#-----------------------------------#    

#--- AUTOREGRESSION Models ARIMA ---#
# Plotting of ACF and PACF plots for calculation of the p,q,d values
#Plot acf 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=0 or 1 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be decided upon

#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)          # MAPE : 215.2423

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)        # MAPE : 190.0039

# So the models using pdq of 2,0,1 was giving a better MAPE value
# plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)

# Now the graph resembles noise, not significant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_consumer_combined_accuracy
# Accuracy of the model decreased. MAPE is 9.497


# Curve Fitting VIA Auto ARIMA
# Create Auto ARIMA model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
par(mar = rep(2, 4))
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc

# The Linear model is better than the Auto ARIMA model.
#------------------------------------------------------# 

# ************APAC_consumer_Quantity ************# ---- 

# Conversion of APAC quantity data to time series.
APAC_Consumer_Quantity_ts <-  ts(APAC_Consumer$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
# plot
plot(APAC_Consumer_Quantity_ts)
# Smoothing of the time series
Smoothed_APAC_consumer_Quantity <- stats::filter(APAC_Consumer_Quantity_ts, 
                                                 filter=rep(1/3,3,method='convolution',
                                                            sides=2))
#As the smothening removes the first and the last data values, thus adding it
diff_1 <- Smoothed_APAC_consumer_Quantity[3] - Smoothed_APAC_consumer_Quantity[2]
Smoothed_APAC_consumer_Quantity[1] <- Smoothed_APAC_consumer_Quantity[2]- diff_1 

diff_2 <- Smoothed_APAC_consumer_Quantity[47] - Smoothed_APAC_consumer_Quantity[46]
Smoothed_APAC_consumer_Quantity[48] <- Smoothed_APAC_consumer_Quantity[47] + diff_2

# plotting the smoothed Quantity curve
lines(Smoothed_APAC_consumer_Quantity ,col='red',lwd=2)

# Converting the Smoothed series into a data frame and adding the Yr_Month column to original Quantity data.
APAC_consumer_Quantity_df <- data.frame(cbind(APAC_Consumer$ Year_Month,
                                              Smoothed_APAC_consumer_Quantity))

# Renaming the columns
colnames(APAC_consumer_Quantity_df) <- c("month","quantity")
APAC_consumer_Quantity_df$quantity <- as.numeric(as.character((APAC_consumer_Quantity_df$quantity)))

# Reconversion of the filled in data frame to time series
APAC_consumer_Quantity.ts <- ts(APAC_consumer_Quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))

APAC_Consumer_Quantity_hw <- ts(APAC_consumer_Quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))

#Predicting the time series using Holtwinters algorithm
HoltWinters(APAC_Consumer_Quantity_hw)
plot(APAC_Consumer_Quantity_hw)
HoltWinters(APAC_Consumer_Quantity_hw)$fitted
APAC_Consumer_Quantity_hw_hw<-HoltWinters(APAC_Consumer_Quantity_hw)
plot(APAC_Consumer_Quantity_hw_hw)
lines(predict(APAC_Consumer_Quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_Consumer_Quantity_hw_hw,n.ahead=6)
# predicted Quantity values based on Holtwinters Model
#         Jan      Feb      Mar      Apr      May      Jun
#2015 812.9676 717.5249 685.3520 751.3690 858.4393 849.7440

#Splitting the Data into TRAIN and Validation for Regression line fitting
#creating windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_Quantity.ts)-ntest
train.ts <- window(APAC_consumer_Quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_Quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))

# ***********Curve fitting for Linear Regression Model ************* #

#creating a linear regression model and plotting it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_Quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red") 
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

# *************** MAPE STATISTIC CALCULATION *************** #

# Calculating MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) 
APAC_consumer_accuracy
#----------------------------------------#  
#MAPE for Linear Model - 15.67811
#----------------------------------------#  

#*************** AUTOREGRESSION Model - ARIMA *****************#


# Plotting the ACF and PACF plots for caluluation of the p,q,d values 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)
# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p=2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=3 as there are 3 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# Running the model with (p,d,q) as (2,0,3) and then one more model with (p,d,q) as (2,0,1) depending on the statistics 
# Model AR of (p,d,q) as (2,0,3)

train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,3))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")
#summary of the AR model-(p,d,q) as (2,0,3)

summary(train.res.ar1)                       #MAPE = 96.16933
# Model AR of (p,d,q) as (2,0,1)
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
#summary of the AR model-(p,d,q) as (2,0,1)
summary(train.res.ar2)                      #MAPE = 101.3898
# From the  models using (p,d,q) as (2,0,1) & (p,d,q) as (2,0,3) we could see better fitting for value (2,0,3)
#plotting the ACF of AR(2,0,3)

acf(train.res.arima.pred1$residuals,lag.max = 12)
# Now it is clear that graph resembles noise, not significant peaks apart from the 0 lag.
#calculating the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred1$mean),valid.ts)
APAC_consumer_combined_accuracy

#-------------------------------------------------------------------#  
#Accuracy of the model is 15.64.so, Thus-Linear model is better
#-------------------------------------------------------------------#

# ******************Curve Fitting VIA Auto ARIMA**********************#
#Creating Auto ARIMA model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc             # MAPE on the validation set is 16.78
# so manual linear model is better than Auto ARIMA model

#***************************FORECASTING*******************************#
#************ Results for Market:'APAC' And customer segment level: 'Consumer'************#
#Forecasting by time series linear model
train.lm.model <- tslm(APAC_consumer_Quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan

#        Point Forecast    Lo 20    Hi 20    Lo 40    Hi 40    Lo 60    Hi 60    Lo 80    Hi 80
#  Jan 2015       607.5278 590.2096 624.8459 571.6264 643.4291 549.7278 665.3278 518.9151 696.1405
#  Feb 2015       543.2778 525.9596 560.5959 507.3764 579.1791 485.4778 601.0778 454.6651 631.8905
#  Mar 2015       524.0278 506.7096 541.3459 488.1264 559.9291 466.2278 581.8278 435.4151 612.6405
#  Apr 2015       602.6111 585.2930 619.9293 566.7097 638.5125 544.8111 660.4111 513.9984 691.2238
#  May 2015       693.6111 676.2930 710.9293 657.7097 729.5125 635.8111 751.4111 604.9984 782.2238
#  Jun 2015       701.4444 684.1263 718.7626 665.5431 737.3458 643.6444 759.2445 612.8318 790.0571

plot(train.lm.total.forecast_quan,col="red")

#************ Results for Market: 'APAC' And customer segment level: 'Consumer'************# ----
#------------------------------------------------------#  
# The Linear model is better than the Auto ARIMA model.
#------------------------------------------------------#   

#-------- Forecast --------#
# Forecasting for the next six months sales Based on the Linear model  

train.lm.model <- tslm(APAC_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
# Point          Forecast    Lo 20    Hi 20    Lo 40    Hi 40    Lo 60    Hi 60    Lo 80    Hi 80
# Jan 2015       51908.62 50366.14 53451.11 48710.98 55106.27 46760.52 57056.72 44016.11 59801.13
# Feb 2015       47893.31 46350.83 49435.79 44695.67 51090.95 42745.21 53041.41 40000.80 55785.82
# Mar 2015       45687.30 44144.81 47229.78 42489.65 48884.94 40539.20 50835.40 37794.79 53579.80
# Apr 2015       52187.40 50644.92 53729.88 48989.76 55385.04 47039.30 57335.50 44294.89 60079.91
# May 2015       58889.24 57346.76 60431.72 55691.60 62086.88 53741.14 64037.34 50996.73 66781.75
# Jun 2015       59354.30 57811.81 60896.78 56156.65 62551.94 54206.20 64502.40 51461.79 67246.81 

plot(train.lm.total.forecast,col="red")  

#************ Results for Market: 'APAC'  And customer segment level: 'Consumer'************#
#------------------------------------------------------#  
# The Linear model is better than the auto arima model.
#------------------------------------------------------# 
#-------- Forecast --------#
# Forecasting for the next six months quantity Based on the Linear model  

#Forecasting by time series linear model
train.lm.model <- tslm(APAC_consumer_Quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan

#        Point Forecast    Lo 20    Hi 20    Lo 40    Hi 40    Lo 60    Hi 60    Lo 80    Hi 80
#  Jan 2015       607.5278 590.2096 624.8459 571.6264 643.4291 549.7278 665.3278 518.9151 696.1405
#  Feb 2015       543.2778 525.9596 560.5959 507.3764 579.1791 485.4778 601.0778 454.6651 631.8905
#  Mar 2015       524.0278 506.7096 541.3459 488.1264 559.9291 466.2278 581.8278 435.4151 612.6405
#  Apr 2015       602.6111 585.2930 619.9293 566.7097 638.5125 544.8111 660.4111 513.9984 691.2238
#  May 2015       693.6111 676.2930 710.9293 657.7097 729.5125 635.8111 751.4111 604.9984 782.2238
#  Jun 2015       701.4444 684.1263 718.7626 665.5431 737.3458 643.6444 759.2445 612.8318 790.0571

plot(train.lm.total.forecast_quan,col="red")

APAC_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_consumer_final_df) <- c("Sales","Qty")

# Predicted sales and quantity for 6 months of 2015
#    Sales	  Qty
#  51908.62	607.5278
#  47893.31	543.2778
#  45687.3	524.0278
#  52187.4	602.6111
#  58889.24	693.6111
#   59354.3	701.4444

#2.EU Consumer  
#-----------------------------------------------------------------------------#
#************ Market: 'EU'  And customer segment level: 'Consumer'************# 

EU_Consumer <- subset(superstore_agg, superstore_agg$Market_Segment =="EU Consumer" )

# ************EU_consumer_sales ************# ----

# Conversion of APAC sales data to time series.
EU_consumer_sales_ts <- ts(EU_Consumer$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

plot(EU_consumer_sales_ts)

# Smootheing of the time series
Smoothed_EU_consumer_sales <- stats::filter(EU_consumer_sales_ts, # Here had to use stats::filter due tp plyr issue#
                                           filter=rep(1/3,3,method='convolution',
                                                      sides=2))

#Converting the Smoothed series into a data frame and adding the Yr_Month column to from original sales data.

EU_consumer_sales_df <- data.frame(cbind(EU_Consumer$Year_Month,
                                         Smoothed_EU_consumer_sales))
#Renaming the columns
colnames(EU_consumer_sales_df) <- c("month","sales")
EU_consumer_sales_df$sales <- as.numeric(as.character((EU_consumer_sales_df$sales)))

#As the smothening removes the first and the last data values, thus adding it
diff_1 <- EU_consumer_sales_df$sales[3] - EU_consumer_sales_df$sales[2]
EU_consumer_sales_df$sales[1] <- EU_consumer_sales_df$sales[2]- diff_1 

diff_2 <- EU_consumer_sales_df$sales[47] - EU_consumer_sales_df$sales[46]
EU_consumer_sales_df$sales[48] <- EU_consumer_sales_df$sales[47] + diff_2

#plotting the smoothed sales curve
lines(Smoothed_EU_consumer_sales,col='red',lwd=2)  

#Reconversion of the filled in data frame to time series  
EU_consumer_sales.ts <- ts(EU_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))

EU_Consumer_sales_hw <- ts(EU_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))

#Predicting the time series using Holtwinters algorithm
HoltWinters(EU_Consumer_sales_hw)
plot(EU_Consumer_sales_hw)
HoltWinters(EU_Consumer_sales_hw)$fitted
EU_Consumer_sales_hw_hw<-HoltWinters(EU_Consumer_sales_hw)
plot(EU_Consumer_sales_hw)
lines(predict(EU_Consumer_sales_hw_hw,n.ahead=48),col=2)
predict(EU_Consumer_sales_hw_hw,n.ahead=6) 

# predicted sales values based on Holtwinters
#         Jan      Feb      Mar      Apr      May      Jun
# 2015 48978.49 42382.51 42294.61 47182.53 55656.26 56422.91

#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation

ntest <- 6
nTrain <- length(EU_consumer_sales.ts)-ntest
train.ts <- window(EU_consumer_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))

# Curve fitting for Linear Regression Model ----
#create a linear regression model and plotting it

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red") 

lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue") 

#************************* MAPE STATISTIC CALCULATION ************************#
#Calculate MAPE and other performance metrics
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) 
EU_consumer_accuracy  

# FOR LINEAR MODEL MAPE IS 13.02

#*******************  AUTOREGRESSION Model- ARIMA   ***************************#
# Plotting of ACF and PACF plots for calculation of the p,q,d values

#Plotting acf  
acf(train.lm.forecast$residuals,lag.max = 12)

#plotting pacf
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 1 as there is only one significant peak in the PACF plot (AR=1)
# q=1 or 2 as there is only 2 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# First  p,d,q as 1,0,1 and then one more model with p,d,q as 1,0,2  , depending on the statistics 
#Model AR and plot it with pdq = 1,0,1

train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(1,0,1))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#checking the summary of the AR model
summary(train.res.ar1)       # MAPE - 201.87
#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(1,0,2))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")

#summary
summary(train.res.ar2)      # MAPE 167.62

# from the summaries of both the models using pdq of 1,0,2 was giving a better MAPE vale ( MAPE =201 vs MAPE=167)

#plot the ACF of AR(1,0,2)
acf(train.res.arima.pred2$residuals,lag.max = 12)

# Now the graph resembles noise, not significant peaks apart from the 0 lag.
#calculating the accuracy of combined linear and AR model

EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
EU_consumer_combined_accuracy   #MAPE of 14.56

#Accuracy of the model decreased. Mape is 14.56, so the linear model is better

#**********************Curve Fitting by Auto ARIMA*****************#  

#Creating autoarima model and check its accuracy

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc  

# MAPE of validation is - 27.11 
# Thus, Linear model is better than the auto arima model

#************************   Forecasting   **********************#  
#Forecasting for the next six months  
train.lm.model <- tslm(EU_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast

#  Point Forecast    Lo 20    Hi 20    Lo 40    Hi 40    Lo 60    Hi 60    Lo 80    Hi 80
# Jan 2015       41118.57 39726.53 42510.61 38232.80 44004.34 36472.58 45764.56 33995.84 48241.30
#  Feb 2015       36870.96 35478.92 38263.00 33985.19 39756.73 32224.97 41516.95 29748.23 43993.69
#  Mar 2015       35873.12 34481.08 37265.16 32987.35 38758.89 31227.13 40519.11 28750.39 42995.85
#  Apr 2015       39016.07 37624.03 40408.11 36130.30 41901.84 34370.08 43662.06 31893.34 46138.80
#  May 2015       45407.94 44015.90 46799.98 42522.17 48293.71 40761.95 50053.93 38285.21 52530.67
#  Jun 2015       46506.10 45114.06 47898.14 43620.33 49391.87 41860.11 51152.09 39383.37 53628.83

plot(train.lm.total.forecast,col="red") 

# ************EU_consumer_Quantity ************# ----

# Conversion of EU quantity data to time series.
EU_consumer_qunatity_ts <-  ts(EU_Consumer$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_consumer_qunatity_ts)

#Smoothening of the time series
Smoothed_EU_consumer_quantity <- stats::filter(EU_consumer_qunatity_ts, # Here had to use stats::filter due tp plyr issue#
                                               filter=rep(1/3,3,method='convolution',
                                                          sides=2)) 

#Converting the Smoothed series into a data frame and adding the Yr_Month column to from original sales data.
EU_consumer_quantity_df <- data.frame(cbind(EU_Consumer$Year_Month,
                                            Smoothed_EU_consumer_quantity))

#Renaming the columns
colnames(EU_consumer_quantity_df) <- c("month","quantity")
EU_consumer_quantity_df$quantity <- as.numeric(as.character((EU_consumer_quantity_df$quantity)))

#As the smothening removes the first and the last data values, thus adding it
diff_1 <- EU_consumer_quantity_df$quantity[3] - EU_consumer_quantity_df$quantity[2]
EU_consumer_quantity_df$quantity[1] <- EU_consumer_quantity_df$quantity[2]- diff_1 


diff_2 <- EU_consumer_quantity_df$quantity[47] - EU_consumer_quantity_df$quantity[46]
EU_consumer_quantity_df$quantity[48] <- EU_consumer_quantity_df$quantity[47] + diff_2  

#plotting the smoothed quantity curve
lines(Smoothed_EU_consumer_quantity,col='red',lwd=2)  

#Reconversion of the filled in data frame to time series  
EU_consumer_quantity.ts <- ts(EU_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                              end=c(2014,12))
EU_Consumer_quantity_hw <- ts(EU_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                              end=c(2014,12))
EU_consumer_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(EU_Consumer_quantity_hw)
plot(EU_Consumer_quantity_hw)
HoltWinters(EU_Consumer_quantity_hw)$fitted
EU_Consumer_quantity_hw_hw<-HoltWinters(EU_Consumer_quantity_hw)
plot(EU_Consumer_quantity_hw)
lines(predict(EU_Consumer_quantity_hw_hw,n.ahead=48),col=2)
predict(EU_Consumer_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
#2015 719.6529 687.8594 707.0050 743.2716 793.8799 786.4806

# SPlitting the Data into TRAIN and Validation for Regression line fitting
#create windows for train and validation
ntest <- 6
nTrain <- length(EU_consumer_quantity.ts)-ntest
train.ts <- window(EU_consumer_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))

#***************** Curve fitting for Linear Regression Model  ********************#
#creating a linear regression model and plotting it

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red") 
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")

#************************* MAPE STATISTIC CALCULATION  ***************************#
#Calculating MAPE and other performance metrics  
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts) 

EU_consumer_accuracy    #MAPE is:16.51

#**************************AUTOREGRESSION Model- ARIMA ****************************#
# Plotting of ACF and PACF plots for calculation of the p,q,d values 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are two singnificant peaks in the PACF (AR=2)
# q=1 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# Model with p,d,q as 2,0,0 and then one more model with p,d,q as 2,0,1 depending on the statistics

#Modelling AR with p,d,q = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")
#checking the summary of the AR model
summary(train.res.ar1)   #MAPE-138.84

#Modelling AR and plotting it with p,d,q = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
#summary of model
summary(train.res.ar2)         #MAPE -141.43

#From the summaries of both the models though the MAPE value is less of p,d,q of 2,0,0& 2,0,1 gives a better graph
#plotting the ACF of AR(2,0,1)  
acf(train.res.arima.pred2$residuals,lag.max = 12)

# Now the graph resembles noise, not significant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
EU_consumer_combined_accuracy
#MAPE of 16.8 so ,Linear model is a  better one

#******************************Curve Fitting VIA Auto ARIMA**********************************#
#Creating autoarima model and checking its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
# MAPE for validation is = 19.28 so ,manual linear model is better than auto arima model
#*******************************FORECASTING******************************************#  
#Forecasting for the next six months 
train.lm.model <- tslm(EU_consumer_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")
EU_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(EU_consumer_final_df) <- c("Sales","Qty") 
#************ Results for Market: 'EU'  And customer segment level: 'Consumer'************# ----
#************************   Forecasting   **********************#  
#Forecasting for the next six monthsof EU-Consumer sales 
train.lm.model <- tslm(EU_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
#  Point Forecast    Lo 20    Hi 20    Lo 40    Hi 40    Lo 60    Hi 60    Lo 80    Hi 80
# Jan 2015       41118.57 39726.53 42510.61 38232.80 44004.34 36472.58 45764.56 33995.84 48241.30
#  Feb 2015       36870.96 35478.92 38263.00 33985.19 39756.73 32224.97 41516.95 29748.23 43993.69
#  Mar 2015       35873.12 34481.08 37265.16 32987.35 38758.89 31227.13 40519.11 28750.39 42995.85
#  Apr 2015       39016.07 37624.03 40408.11 36130.30 41901.84 34370.08 43662.06 31893.34 46138.80
#  May 2015       45407.94 44015.90 46799.98 42522.17 48293.71 40761.95 50053.93 38285.21 52530.67
#  Jun 2015       46506.10 45114.06 47898.14 43620.33 49391.87 41860.11 51152.09 39383.37 53628.83

plot(train.lm.total.forecast,col="red") 

#************ Results for Market: 'EU'  And customer segment level: 'Consumer'************#
#Forecasting for the next six months of EU-Consumer Quantity 
train.lm.model <- tslm(EU_consumer_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")
#Combined results of sales and quantity for EU-Consumer 
EU_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(EU_consumer_final_df) <- c("Sales","Qty") 

#  Month           Sales   	Qty
#*******************************  
# Jan 2015   41118.57	  552.2847
#  Feb 2015  36870.96	  514.0347
#  Mar 2015  35873.12	  505.5347
#  Apr 2015  39016.07	  524.3681
#  May 2015  45407.94	  587.8681
#  Jun 2015  46506.1	  587.9514

