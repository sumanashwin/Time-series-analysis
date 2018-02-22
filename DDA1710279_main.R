#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1. Business objective: Forecast the sales and demand for the Global mart for the next 6 months

#2. Data understanding : There are 24 attributes related to each such transaction. The "Market" attribute has 7-factor levels representing the geographical market sector that the customer belongs to. 
#   The "Segment" attribute tells which of the 3 segments that customer belongs to

#3. Data preparation:Segment the whole dataset into the 21 subsets based on the market and the customer segment level
#   convert the transaction-level data into a time series. Thus, you would need to aggregate the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive at
#   monthly values for these attributes

#4. Model building:Forecast the sales and quantity for the next 6 months using classical decomposition and Auto ARIMA method

#5. Model evaluation:To test the accuracy of your forecast, you must initially separate out the last 6 months values from your dataset and Use MAPE as evaluation criteria


#Please note ,there are any error messages regarding the graph margins , please maximise the Viewer window to see the plots
#Try dev.off() if there is any graphics related warnings.
#These messages/warnings occur intermittantly and are not related to the code.


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Loading the libraries required for Time series analysis


library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
require(graphics)
library(lubridate)
library(stringr)
library(rowr)
library(formattable)
library(grid)

#---------------------------------------------------------------------------------------------------------------------------------------------

#Importing the dataset Global Superstore.csv

global_Superstore <- read.csv("Global Superstore.csv",stringsAsFactors = F)
# Has 51290 observations of 24 variables

summary(global_Superstore)


#Eliminating the columns which are not required for our analysis after the data understanding and in the process simplify the analysis
#We will retain only these :"Order.Date" ,"Segment","Market","Sales","Quantity","Profit"
#Why these 6 attributes only :  Order.Date for extracting the time series. Segment and Market for segmentation
# Sales,Quantity,profit : for aggregation and applying the Coefficient of variation to come up with 2 profitable segments
# Use Sales and Quantity for the time series forecasting

superstore_Data <- global_Superstore[,c("Order.Date" ,"Segment","Market","Sales","Quantity","Profit")]
#We have now narrowed down to 6 attributes

#-------------------------------------------------------------------------------------------------------------------
#                         DATA PREPARATION(DATA CLEANING)
#-----------------------------------------------------------------------------------------------------------------------
#Lets perform some data quality check on the superstore_Data

#Dealing with missing values(Imputing the NA's)
sum(is.na(superstore_Data))  #0. There are no NA's

#Dealing with blanks
storesdata_Blank <- sapply(superstore_Data, function(x) length(which(x == "")))
storesdata_Blank 
# Order.Date    Segment     Market      Sales   Quantity     Profit 
#   0                0          0          0          0          0 

#Looking at the structure of the dataframe
str(superstore_Data)


# $ Order.Date: chr  "31-07-2012" "05-02-2013" "17-10-2013" "28-01-2013" ...
# $ Segment   : chr  "Consumer" "Corporate" "Consumer" "Home Office" ...
# $ Market    : chr  "US" "APAC" "APAC" "EU" ...
# $ Sales     : num  2310 3709 5175 2893 2833 ...
# $ Quantity  : int  7 9 9 5 8 5 4 6 5 13 ...
# $ Profit    : num  762.2 -288.8 920 -96.5 311.5 ..

#lets convert the Order.Date column to date object

superstore_Data$Order.Date <- dmy(superstore_Data$Order.Date)
typeof(superstore_Data$Order.Date) #double, the conversion is successful

#lets convert the Segment and Market to factor types

superstore_Data$Segment <- as.factor(superstore_Data$Segment)
superstore_Data$Market <- as.factor(superstore_Data$Market)

summary(superstore_Data)

#Looking at Outliers/extreme values

#Range for Sales is from 0.44 to 22638 
#Range for Quantity is from 1 to 14
#Range for profit is from -6599 to 8399
#Since all these values are numeric in nature suited to type of attribute(Sales,Quantity,Profit)
#we can conculde there are not outliers in this dataset

numeric_Dataframe <- superstore_Data[,c("Profit","Sales","Quantity")]
outlier_df <- data.frame(sapply(numeric_Dataframe,function(x) quantile(x,seq(0,1,0.01))))
outlier_df

#We can also confirm that there has been a steady response of the numeric attributes with respect to time series

#--------------------------------------------------------------------------------------------------------------------------------
#                                   DATA PREPARATION and EDA (AGGREGATION AND FINDING 2 MOST PROFITABLE SEGMENTS)           
#--------------------------------------------------------------------------------------------------------------------------------


# Lets aggregate the different performance units of the stores, i.e Sales,Quantity and Profit by Month and Year
#Its easier to first create the aggregation and then subset into 21 buckets for Unique combination of Market and segment
#This is also a way of coming with derived metrics

superstore_aggregate <- superstore_Data %>% group_by(month=floor_date(Order.Date, "month"),Market,Segment) %>% summarise(Tot.Profit=sum(Profit),Tot.Sales=sum(Sales),Tot.Qty=sum(Quantity))


superstore_aggregate$Tot.Profit <- round(superstore_aggregate$Tot.Profit,2) #rounding off to 2 digits
superstore_aggregate$Tot.Sales <- round(superstore_aggregate$Tot.Sales,2) #rounding off to 2 digits
superstore_aggregate$Segment <- as.character(superstore_aggregate$Segment)
superstore_aggregate$Segment <- gsub('\\s+', '',superstore_aggregate$Segment) #removing the space in Home Office segment, this logic helps in dynamically naming the dataframes
superstore_aggregate$Segment <- as.factor(superstore_aggregate$Segment) #converting back to factor type

str(superstore_aggregate$Segment)

#Now lets segment the whole dataset into subsets based on Unique number of Market and segments

summary(superstore_aggregate)

unique(superstore_aggregate$Market)
#[1] Africa APAC   Canada EMEA   EU     LATAM  US    

unique(superstore_aggregate$Segment)
#[1] Consumer    Corporate   HomeOffice

#There are 7 unique Markets and 3 unique segments thus giving 21 unique combinations of market and segment datasets
buckets <- unique(superstore_aggregate[,c("Market","Segment")])
View(buckets)


#Actually creating 21 different subsets
y <- data.frame(matrix(, nrow =48, ncol=21)) #Initializing empty data frame

#logic for aggregating profits for all the 21 segments
for (i in 1:nrow(buckets)) {
  
 y<- rowr::cbind.fill(y, data.frame(subset(superstore_aggregate,superstore_aggregate$Market == buckets$Market[i] & superstore_aggregate$Segment == buckets$Segment[i]))$Tot.Profit,fill=NA)
 colnames(y)[i] <- paste(buckets$Market[i],buckets$Segment[i],sep  = "_")
}  

colnames(y)[22:42] <- colnames(y)[1:21]
profit_Dataframe <- y[,-c(1:21)] #Contains the profit vectors to each of the 21 buckets
View(profit_Dataframe)

#Now lets find out the 2 most profitable segment by using the CV
#Computing the coefficient of variation for each of 21 buckets

cv_Profits <- round(sapply(profit_Dataframe, function(x) sd(x,na.rm = T)/abs(mean(x,na.rm = T))),2)
#getting the 2 most profitable segments in order of CV (Consistency)
cv_min <- order(cv_Profits, decreasing = F)[1:2]
cv_Profits[cv_min]
# EU_Consumer APAC_Consumer 
# 0.62          0.63 

cv_Profits <- data.frame(cv_Profits)
cv_Profits$segment <- rownames(cv_Profits)

#Viewing 2 most profitable segments in lowest order of CV
ggplot(cv_Profits,aes(x =reorder(segment,cv_Profits),y=cv_Profits))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



#getting the 2 most profitable segments in order of total profit(Profitability)

total_profits <- round(sapply(profit_Dataframe, function(x) sum(x,na.rm = T)),2)

total_profits_max  <- order(total_profits, decreasing = T)[1:2]
total_profits[total_profits_max]
# APAC_Consumer   EU_Consumer 
# 222817.5      188687.7 

total_profits <- data.frame(total_profits)
total_profits$segment <- rownames(total_profits)

#Viewing 2 most profitable segments in highest order of profits
ggplot(total_profits,aes(x =reorder(segment,-total_profits),y=total_profits))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



#Hence , both from the consistency and profitability standpoints, APAC_Consumer and the EU_Consumer are the 2 most segments for further analysis

#Lets Create 2 dataframes for APAC_Consumer and EU_Consumer(2 most profitable segments)
APAC_Consumer <- data.frame(subset(superstore_aggregate,superstore_aggregate$Market == "APAC" & superstore_aggregate$Segment == "Consumer"))
EU_Consumer <-   data.frame(subset(superstore_aggregate,superstore_aggregate$Market == "EU" & superstore_aggregate$Segment == "Consumer"))


#Visualising various trends for sales,Quantity and profits for APAC_Consumer

plot1 <- ggplot(APAC_Consumer,aes(x=month,y=Tot.Profit))+ geom_line(col="red")+labs(title="Trend for APAC Consumer Profit",x="Year",y="Profits")
plot2 <- ggplot(APAC_Consumer,aes(x=month,y=Tot.Sales))+ geom_line(col="blue")+labs(title="Trend for APAC Consumer Sales",x="Year",y="Sales")
plot3 <- ggplot(APAC_Consumer,aes(x=month,y=Tot.Qty))+ geom_line(col="green")+labs(title="Trend for APAC Consumer Quantity",x="Year",y="Quantity")

grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), ggplotGrob(plot3),size = "last"))

#Visualising various trends for sales,Quantity and profits for EU_Consumer


plot4 <- ggplot(EU_Consumer,aes(x=month,y=Tot.Profit))+ geom_line(col="red")+labs(title="Trend for EU Consumer Profit",x="Year",y="Profits")
plot5 <- ggplot(EU_Consumer,aes(x=month,y=Tot.Sales))+ geom_line(col="blue")+labs(title="Trend for EU Consumer Sales",x="Year",y="Sales")
plot6 <- ggplot(EU_Consumer,aes(x=month,y=Tot.Qty))+ geom_line(col="green")+labs(title="Trend for EU Consumer Quantity",x="Year",y="Quantity")

grid.draw(rbind(ggplotGrob(plot4), ggplotGrob(plot5), ggplotGrob(plot6),size = "last"))



# Subsetting the 2 most profitable segments by sales and Quantity
#a) for EU Consumer
EU_Consumer_Sales <- EU_Consumer[,c(1,5)]
EU_Consumer_Qty <-   EU_Consumer[,c(1,6)]
#b) for APAC consumer
APAC_Consumer_Sales <- APAC_Consumer[,c(1,5)]
APAC_Consumer_Qty <-   APAC_Consumer[,c(1,6)]

#--------------------------------------------------------------------------------------------------------------
#                           TIME SERIES MODELLING BEGINS HERE
#--------------------------------------------------------------------------------------------------------------

#1. Lets set aside the last 6 months of data for Test and retain the remaining for train

EU_Consumer_Sales_train <- EU_Consumer_Sales[1:42,];EU_Consumer_Sales_test <- EU_Consumer_Sales[43:48,]
EU_Consumer_Qty_train <- EU_Consumer_Qty[1:42,];EU_Consumer_Qty_test <- EU_Consumer_Qty[43:48,]

APAC_Consumer_Sales_train <- APAC_Consumer_Sales[1:42,];APAC_Consumer_Sales_test <- APAC_Consumer_Sales[43:48,]
APAC_Consumer_Qty_train <- APAC_Consumer_Qty[1:42,];APAC_Consumer_Qty_test <- APAC_Consumer_Qty[43:48,]

#------------------------------------------------------------------------------------------------
#Coming up with global trend
# Function for plotting the global trend component of the time series
timeSeries_Plot <- function(segment,title,sales=1){
  
  ifelse((sales==1),timeseries <- ts(segment$Tot.Sales),timeseries <- ts(segment$Tot.Qty))
  plot(timeseries,col="red",main=title)
}

par(mfrow=c(2,1))
timeSeries_Plot (EU_Consumer_Qty_train,title = "Monthly Demand in EU Consumer Segment(training set)",sales =0)
timeSeries_Plot (EU_Consumer_Sales_train,title = "Monthly Sales in EU Consumer Segment(Training set)",sales =1)
timeSeries_Plot (APAC_Consumer_Qty_train,title = "Monthly Demand in APAC Consumer Segment(training set)",sales =0)
timeSeries_Plot (APAC_Consumer_Sales_train,title = "Monthly Sales in APAC Consumer Segment(Training set)",sales =1)

#------------------------------------------------------------------------------------------------------------------

# Lets perform the smoothing process using the exponential smoothing
# Function for generating smoothened time series using different values of Alpha and creating a 
#smoothened dataframe for  1 to 42 months

smooth_Series <- function(series,sales=1,title){   #---Function begins here
  
ifelse(sales==1,timeser <- ts(series$Tot.Sales),timeser <- ts(series$Tot.Qty))


#Starting with smoothing  with exponential smoothing for different values of alpha
  plot(timeser, main=title)
  
  cols <- c("red", "blue", "green", "black")
  alphas <- c(0.02, 0.4, 0.8)
  labels <- c(paste("alpha =", alphas), "Original")
  for (i in seq(1,length(alphas))) {
    smoothedseries <- HoltWinters(timeser, alpha=alphas[i],
                                  beta=FALSE, gamma=FALSE)
    
    lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
  }
  
  legend("bottomright", labels, col=cols, lwd=2)
  
  # As we can see from the plots , the alpha with 0.4 gives the desired amount of smoothing.
  
  timevals_in <- seq(1:42) #Storing 42 months in seq_month, to fit a regression line
  smoothedseries <-HoltWinters(timeser,alpha = 0.4,beta = F,gamma = F) #generating smoothened series using Alpha as 0.4
  extract_smoothfit <- fitted(smoothedseries)[,1] #extracting the smoothened series
  smootheddf <- as.data.frame(cbind(timevals_in, round(as.vector(extract_smoothfit),2)))
  ifelse((sales==1),entity <- "Tot.Sales",entity <- "Tot.Qty")
  colnames(smootheddf) <- c('Month', entity)
  return(smootheddf)

}

dev.off()
#Lets call the smooth function and store the smoothened series in separate variables
#Plots for each function call will be on the plot viewer

smooth_APAC_Qty <- smooth_Series(APAC_Consumer_Qty_train,sales=0,title="APAC Consumer Demand(Qty)")  #Storing smoothened series for APAC_Consumer segment, for Quantity
smooth_APAC_Sales <- smooth_Series(APAC_Consumer_Sales_train,sales=1,title="APAC Consumer Sales") #Storing smoothened series for APAC_Consumer segment, for Sales

smooth_EU_Qty <- smooth_Series(EU_Consumer_Qty_train,sales=0,title="EU Consumer demand(Qty)")  #Storing smoothened series for EU_Consumer segment, for Quantity
smooth_EU_Sales <- smooth_Series(EU_Consumer_Sales_train,sales=1,title="EU Consumer sales") #Storing smoothened series for EU_Consumer segment, for Sales


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                               BEGINNING OF THE CLASSICAL DECOMPOSITION METHOD
#------------------------------------------------------------------------------------------------------------------------------------------------------------

#Now that we have the smoothened time series lets begin the with classical decomposition method of doing time series analysis

#Some of the consideration for performing the forecasting using classical decomposition method

#1. Both the markets EU and APAC have comparable CV (coefficient of variations) for profits
#2. All the 4 time series plots i.e, EU Sales, EU Qty, APAC sales, APAC Qty shows similar time series patterns
#3. Sales and Quantity are correlated , as sales increases , there is a rise in demand (Quantity) and vice versa
#4. We tried the trial and error method to arrive at a regression function which best fits the time series
#5. Out of all the functions we tried ( straight line fit, cos function with poly,sine function with poly,one specific function gave a reasonably lower MAPE for all 4 buckets)
#6. With the above observations , here is a common function to perform the classical decomposition time series analysis

timevals_in <- seq(1:42) #sequence for first 42 months

#smoothedddf1 --->smoothened series
#series --->training set
#testdata --->test set
#totaldata --->entire dataframe without split


classical_decomposition <- function(smootheddf1,sales=1,series,testdata,totaldata){

  
ifelse(sales==1, x<-"Tot.Sales",x<- "Tot.Qty") #For dynamic selection of entity

#a. Modelling the global component with cos and sin functions
  
if(sales==1){
lmfit <- lm(Tot.Sales ~ cos(0.5*Month)+sin(0.5*Month)+Month, data=smootheddf1)  #regression fit
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
}else{
  
  lmfit <- lm(Tot.Qty ~cos(0.5*Month)+sin(0.5*Month)+Month, data=smootheddf1)
  global_pred <- predict(lmfit, Month=timevals_in)
  summary(global_pred)
}
  

  plot(ts(series))
  lines(smootheddf1$x, col="blue", lwd=2)
  lines(timevals_in, global_pred, col='red', lwd=2) #return plot_timeseries2
  
#b Plotting the time series for Training set
timeser <-ts(series)

#c. Now, let's look at the locally predictable series, We will model it as an ARMA series

local_pred <- timeser-global_pred
armafit <- auto.arima(local_pred)


#d. We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf_test_Result <- adf.test(resi,alternative = "stationary" ) #return adf_test_Result
kpass_test_Result <- kpss.test(resi)  #return kpass_test_Result


#e.Now, let's evaluate the model using MAPE and let's make a prediction for the last 6 months

outdata <- testdata
timevals_out <- seq(43,48,1)

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#f. Predicting from months 49 to 54 for unseen data
timevals_out_unseen <- seq(49,54,1)
forecast_next6months <- predict(lmfit,data.frame(Month =timevals_out_unseen))


#g. Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata)[5] #return MAPE

#h. forecasted dataframe for months 43 to 48

month_seq1 <- format(seq(as.Date("2014/7/1"), by = "month", length.out = 6),"%b %Y")
forecast_knowndata <- data.frame(cbind(timevals_out,outdata,fcast))
colnames(forecast_knowndata) <- c("Months","Actual_Value","Forecasted_Value")
percent_Deviation <- formattable::percent((forecast_knowndata$Forecasted_Value-forecast_knowndata$Actual_Value)/forecast_knowndata$Actual_Value) #Calculating deviance from Actual value
forecast_knowndata <- cbind(forecast_knowndata,percent_Deviation)
forecast_knowndata[1] <- month_seq1

#i forecasted dataframe for months 49 to 54 
month_seq <- format(seq(as.Date("2015/1/1"), by = "month", length.out = 6),"%b %Y")
forecast_unknowndata <- data.frame(cbind(month_seq,round(forecast_next6months,2)))
colnames(forecast_unknowndata) <- c("Months","Forecasted_Value")


#j. Below 2 variables are calculated to get the visual fit of the data

total_timeser <- ts(totaldata)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))


output <- list(global_pred,local_pred ,adf_test_Result,kpass_test_Result,MAPE_class_dec,forecast_knowndata,forecast_unknowndata,total_timeser,class_dec_pred)
return(output)

}

#--------------------------------------------------------------------------------------------
#Interpretation for the APAC sales
#---------------------------------------------------------------------------------------------

result_APAC_Sales <- classical_decomposition(smooth_APAC_Sales,sales=1,APAC_Consumer_Sales_train$Tot.Sales,APAC_Consumer_Sales_test$Tot.Sales,APAC_Consumer_Sales$Tot.Sales)


#1. Plotting the timeseries

plot(ts(APAC_Consumer_Sales_train$Tot.Sales),xlab="Months",ylab="APAC Consumer Sales")
lines(smooth_APAC_Sales$Tot.Sales, col="blue", lwd=2)
lines(timevals_in , result_APAC_Sales[[1]], col='red', lwd=2)

#2.  Plotting local predictable series

plot(result_APAC_Sales[[2]], col='red', type = "l",xlab="Months",ylab="APAC Consumer Sales")

#3. Viewing ACF plots and ARIMA fit

acf(result_APAC_Sales[[2]]) #Shows a ACF of 1 for lag 0
acf(result_APAC_Sales[[2]], type="partial") #return plot_acf_partial
armafit <- auto.arima(result_APAC_Sales[[2]]) 

tsdiag(armafit) #Plots for different diagnostics for armafit
armafit  

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 123696859:  log likelihood=-450.9
# AIC=903.79   AICc=903.89   BIC=905.53

#4. We'll check if the residual series is white noise

#Augmented Dickey-Fuller Test
result_APAC_Sales[[3]]   
#data:  resi, implying 
# Dickey-Fuller = -4.3109, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

#KPSS Test for Level Stationarity
result_APAC_Sales[[4]]  
# data:  resi
# KPSS Level = 0.069008, Truncation lag parameter = 1, p-value = 0.1

#This confirms residual series is white noise

#5. Evaluating prediction using MAPE

result_APAC_Sales[[5]]  
#MAPE= [1] 20.88024


#6.Storing the forecasted sales data in Dollars for last 6 months[July 2014-Dec 2014 for known data]

APAC_Sales_forecast1 <- result_APAC_Sales[[6]] 
View(APAC_Sales_forecast1)

#7. Storing the forecasted sales data for months from Jan 2015 to June 2015

APAC_Sales_forecast2 <- result_APAC_Sales[[7]] 
View(APAC_Sales_forecast2)

#8.Visual fit of the timeseries can be seen on the plot Viewer, where black line indicates original series and redline indicates predicted series

plot(result_APAC_Sales[[8]] , col = "black",xlab="Months",ylab="APAC_Sales")
lines(result_APAC_Sales[[9]] , col = "red") 



#--------------------------------------------------------------------------------------------
#Interpretation for the APAC Quantity
#---------------------------------------------------------------------------------------------

result_APAC_Qty <- classical_decomposition(smooth_APAC_Qty,sales=0,APAC_Consumer_Qty_train$Tot.Qty,APAC_Consumer_Qty_test$Tot.Qty,APAC_Consumer_Qty$Tot.Qty)


#1. Plotting the timeseries

plot(ts(APAC_Consumer_Qty_train$Tot.Qty),xlab="Months",ylab="APAC Consumer Qty")
lines(smooth_APAC_Qty$Tot.Qty, col="blue", lwd=2)
lines(timevals_in , result_APAC_Qty[[1]], col='red', lwd=2)

#2.  Plotting local predictable series

plot(result_APAC_Qty[[2]], col='red', type = "l",xlab="Months",ylab="APAC Consumer Qty")

#3. Viewing ACF plots and ARIMA fit

acf(result_APAC_Qty[[2]]) #Shows a ACF of 1 for lag 0
acf(result_APAC_Qty[[2]], type="partial") #return plot_acf_partial
armafit <- auto.arima(result_APAC_Qty[[2]]) 

tsdiag(armafit) #Plots for different diagnostics for armafit
armafit  

# Series: result_APAC_Qty[[2]] 
# ARIMA(0,0,0) with zero mean     
# 
# sigma^2 estimated as 15983:  log likelihood=-262.86
# AIC=527.72   AICc=527.82   BIC=529.46

#4. We'll check if the residual series is white noise

#Augmented Dickey-Fuller Test
result_APAC_Qty[[3]]   
# data:  resi
# Dickey-Fuller = -4.6714, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

#KPSS Test for Level Stationarity
result_APAC_Qty[[4]]  
# data:  resi
# KPSS Level = 0.082695, Truncation lag parameter = 1, p-value = 0.1

#This confirms residual series is white noise

#5. Evaluating prediction using MAPE

result_APAC_Qty[[5]]  
#MAPE= [1]  26.13344


#6.Storing the forecasted Demand(Qty) data  for last 6 months[July 2014-Dec 2014 for known data]

APAC_Qty_forecast1 <- result_APAC_Qty[[6]] 
View(APAC_Qty_forecast1)

#7. Storing the forecasted Demand(Qty) data for months from Jan 2015 to June 2015

APAC_Qty_forecast2 <- result_APAC_Qty[[7]] 
View(APAC_Qty_forecast2)

#8.Visual fit of the timeseries can be seen on the plot Viewer, where black line indicates original series and redline indicates predicted series

plot(result_APAC_Qty[[8]] , col = "black",xlab="Months",ylab="APAC_Qty")
lines(result_APAC_Qty[[9]] , col = "red") 



#--------------------------------------------------------------------------------------------
#Interpretation for the EU sales
#---------------------------------------------------------------------------------------------

result_EU_Sales <- classical_decomposition(smooth_EU_Sales,sales=1,EU_Consumer_Sales_train$Tot.Sales,EU_Consumer_Sales_test$Tot.Sales,EU_Consumer_Sales$Tot.Sales)


#1. Plotting the timeseries

plot(ts(EU_Consumer_Sales_train$Tot.Sales),xlab="Months",ylab="EU Consumer Sales")
lines(smooth_EU_Sales$Tot.Sales, col="blue", lwd=2)
lines(timevals_in , result_EU_Sales[[1]], col='red', lwd=2)

#2.  Plotting local predictable series

plot(result_EU_Sales[[2]], col='red', type = "l",xlab="Months",ylab="EU Consumer Sales")

#3. Viewing ACF plots and ARIMA fit

acf(result_EU_Sales[[2]]) #Shows a ACF of 1 for lag 0
acf(result_EU_Sales[[2]], type="partial") #return plot_acf_partial
armafit <- auto.arima(result_EU_Sales[[2]]) 

tsdiag(armafit) #Plots for different diagnostics for armafit
armafit  

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 121169809:  log likelihood=-450.46
# AIC=902.92   AICc=903.02   BIC=904.66

#4. We'll check if the residual series is white noise

#Augmented Dickey-Fuller Test
result_EU_Sales[[3]]   
#data:  resi, implying 
# Dickey-Fuller = -3.8219, Lag order = 3, p-value = 0.02753
# alternative hypothesis: stationary

#KPSS Test for Level Stationarity
result_EU_Sales[[4]]  
# data:  resi
#KPSS Level = 0.079523, Truncation lag parameter = 1, p-value = 0.1

#This confirms residual series is white noise

#5. Evaluating prediction using MAPE

result_EU_Sales[[5]]  
#MAPE= [1] 24.61031


#6.Storing the forecasted sales data in Dollars for last 6 months[July 2014-Dec 2014 for known data]

EU_Sales_forecast1 <- result_EU_Sales[[6]] 
View(EU_Sales_forecast1)

#7. Storing the forecasted sales data for months from Jan 2015 to June 2015

EU_Sales_forecast2 <- result_EU_Sales[[7]] 
View(EU_Sales_forecast2)

#8.Visual fit of the timeseries can be seen on the plot Viewer, where black line indicates original series and redline indicates predicted series

plot(result_EU_Sales[[8]] , col = "black",xlab="Months",ylab="EU_Sales")
lines(result_EU_Sales[[9]] , col = "red") 


#--------------------------------------------------------------------------------------------
#Interpretation for the EU Qty
#---------------------------------------------------------------------------------------------

result_EU_Qty <- classical_decomposition(smooth_EU_Qty,sales=0,EU_Consumer_Qty_train$Tot.Qty,EU_Consumer_Qty_test$Tot.Qty,EU_Consumer_Qty$Tot.Qty)


#1. Plotting the timeseries

plot(ts(EU_Consumer_Qty_train$Tot.Qty),xlab="Months",ylab="EU Consumer Qty")
lines(smooth_EU_Qty$Tot.Qty, col="blue", lwd=2)
lines(timevals_in , result_EU_Qty[[1]], col='red', lwd=2)

#2.  Plotting local predictable series

plot(result_EU_Qty[[2]], col='red', type = "l",xlab="Months",ylab="EU Consumer Qty")

#3. Viewing ACF plots and ARIMA fit

acf(result_EU_Qty[[2]]) #Shows a ACF of 1 for lag 0
acf(result_EU_Qty[[2]], type="partial") #return plot_acf_partial
armafit <- auto.arima(result_EU_Qty[[2]]) 

tsdiag(armafit) #Plots for different diagnostics for armafit
armafit  

# ARIMA(1,0,2) with zero mean     
# 
# Coefficients:
#   ar1     ma1      ma2
# -0.5319  0.4320  -0.3772
# s.e.   0.2078  0.1933   0.1291
# 
# sigma^2 estimated as 14380:  log likelihood=-259.32
# AIC=526.65   AICc=527.73   BIC=533.6

#4. We'll check if the residual series is white noise

#Augmented Dickey-Fuller Test
result_EU_Qty[[3]]   
#data:  resi, implying 
# Dickey-Fuller = -3.3228, Lag order = 3, p-value = 0.08188
# alternative hypothesis: stationary

#KPSS Test for Level Stationarity
result_EU_Qty[[4]]  
# data:  resi
#KPSS Level = 0.063004, Truncation lag parameter = 1, p-value = 0.1

#This confirms residual series is white noise

#5. Evaluating prediction using MAPE

result_EU_Qty[[5]]  
#MAPE= [1] 27.43684


#6.Storing the forecasted Demand for last 6 months[July 2014-Dec 2014 for known data]

EU_Qty_forecast1 <- result_EU_Qty[[6]] 
View(EU_Qty_forecast1)

#7. Storing the forecasted Demand data for months from Jan 2015 to June 2015

EU_Qty_forecast2 <- result_EU_Qty[[7]] 
View(EU_Qty_forecast2)

#8.Visual fit of the timeseries can be seen on the plot Viewer, where black line indicates original series and redline indicates predicted series

plot(result_EU_Qty[[8]] , col = "black",xlab="Months",ylab="EU_Demand")
lines(result_EU_Qty[[9]] , col = "red") 


####################################################################################################################
#So, that was classical decomposition, now let's do an ARIMA fit

####################################################################################################################


#Lets write a function that returns a forecasted dataframe for prediction using Auto arima
#series --> training set
#total series ---->entire dataframe without split
#outdata ----> test set

forecast_autoarima <- function(series,totalseries,outdata){
  
#1. Computing autoarima

timeser_arima <- ts(series)
total_timeser <- ts(totalseries)
autoarima <- auto.arima(timeser_arima) #return autoarima

#2. Checking the residual series

resi_auto_arima <- timeser_arima - fitted(autoarima)
adf_arima <- adf.test(resi_auto_arima,alternative = "stationary") #return adf_arima
kpss_arima <- kpss.test(resi_auto_arima) #return kpss_arima

#3.#Also, let's evaluate the model using MAPE
#a) lets forecast for 12 months, out of which the first 6 months will be used for evaluation
#b) the next 6 months will be forecast for Unseen data

fcast_auto_arima  <- predict(autoarima, n.ahead = 12) # return fcast_auto_arima$pred[1:6]
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred[1:6],outdata)[5] #return MAPE_auto_arima
#For plotting auto arima prediction
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred[1:6]))


#4. Storing the forecasted values in dataframes
timevals_out <- seq(43,48)

month_seq1 <- format(seq(as.Date("2014/7/1"), by = "month", length.out = 6),"%b %Y")
forecast_knowndata <- data.frame(cbind(timevals_out,outdata,fcast_auto_arima$pred[1:6]))
colnames(forecast_knowndata) <- c("Months","Actual_Value","Forecasted_Value")
percent_Deviation <- formattable::percent((forecast_knowndata$Forecasted_Value-forecast_knowndata$Actual_Value)/forecast_knowndata$Actual_Value) #Calculating deviance from Actual value
forecast_knowndata <- cbind(forecast_knowndata,percent_Deviation)
forecast_knowndata[1] <- month_seq1

# forecasted dataframe for months 49 to 54 
month_seq <- format(seq(as.Date("2015/1/1"), by = "month", length.out = 6),"%b %Y")
forecast_unknowndata <- data.frame(cbind(month_seq,round(fcast_auto_arima$pred[7:12],2)))
colnames(forecast_unknowndata) <- c("Months","Forecasted_Value")

output <- list(autoarima,adf_arima,kpss_arima,MAPE_auto_arima,auto_arima_pred,fcast_auto_arima$pred[1:6],forecast_knowndata,forecast_unknowndata)

}

#--------------------------------------------------------------------------------------------------
#     AUTO ARIMA MODELLING FOR APAC CONSUMER SALES
#--------------------------------------------------------------------------------------------------

result_APAC_Sales_arima <- forecast_autoarima(APAC_Consumer_Sales_train$Tot.Sales,APAC_Consumer_Sales$Tot.Sales,APAC_Consumer_Sales_test$Tot.Sales)

#a) lets look into what autoarima function is returning

result_APAC_Sales_arima[[1]]
# ARIMA(0,1,1)                    
# sigma^2 estimated as 174361546:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

tsdiag(result_APAC_Sales_arima[[1]])
plot(result_APAC_Sales_arima[[1]]$x, col="black",xlab="Months",ylab="APAC Sales")
lines(fitted(result_APAC_Sales_arima[[1]]), col="red")

# b) Again, let's check if the residual series is white noise using Dickey fuller and KPSS tests

result_APAC_Sales_arima[[2]]

# Augmented Dickey-Fuller Test
 
# data:  resi_auto_arima
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

result_APAC_Sales_arima[[3]]

# KPSS Test for Level Stationarity
# data:  resi_auto_arima
# KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

#c. Calculating accuracy using MAPE

result_APAC_Sales_arima[[4]] #[1] 27.68952

#d. Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

plot(APAC_Consumer_Sales$Tot.Sales, col = "black",xlab="Months",ylab="APAC Sales")
lines(result_APAC_Sales_arima[[5]], col = "red")


#e.Storing the forecasted Demand for last 6 months[July 2014-Dec 2014 for known data]
APAC_Sales_arimaforecast1 <- result_APAC_Sales_arima[[7]]
View(APAC_Sales_arimaforecast1)

#f. Storing the forecasted Demand data for months from Jan 2015 to June 2015

APAC_Sales_arimaforecast2<- result_APAC_Sales_arima[[8]]
View(APAC_Sales_arimaforecast2)


#--------------------------------------------------------------------------------------------------
#     AUTO ARIMA MODELLING FOR APAC CONSUMER QUANTITY
#--------------------------------------------------------------------------------------------------

result_APAC_Qty_arima <- forecast_autoarima(APAC_Consumer_Qty_train$Tot.Qty,APAC_Consumer_Qty$Tot.Qty ,APAC_Consumer_Qty_test$Tot.Qty)

#a) lets look into what autoarima function is returning

result_APAC_Qty_arima[[1]]
# ARIMA(0,1,0)                    
 # sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(result_APAC_Qty_arima[[1]]) #Diagnostic plots
plot(result_APAC_Qty_arima[[1]]$x, col="black",xlab="Months",ylab="APAC Qty")
lines(fitted(result_APAC_Qty_arima[[1]]), col="red")


# b) Again, let's check if the residual series is white noise using Dickey fuller and KPSS tests

result_APAC_Qty_arima[[2]]

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

result_APAC_Qty_arima[[3]]

# KPSS Test for Level Stationarity
# data:  resi_auto_arima
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

#c. Calculating accuracy using MAPE

result_APAC_Qty_arima[[4]] #[1] 26.24458

#d. Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

plot(APAC_Consumer_Qty$Tot.Qty, col = "black",xlab="Months",ylab="APAC Qty")
lines(result_APAC_Qty_arima[[5]], col = "red")


#e.Storing the forecasted Demand for last 6 months[July 2014-Dec 2014 for known data]
APAC_Qty_arimaforecast1 <- result_APAC_Qty_arima[[7]]
View(APAC_Qty_arimaforecast1)

#f. Storing the forecasted Demand data for months from Jan 2015 to June 2015

APAC_Qty_arimaforecast2<- result_APAC_Qty_arima[[8]]
View(APAC_Qty_arimaforecast2)


#--------------------------------------------------------------------------------------------------
#     AUTO ARIMA MODELLING FOR EU CONSUMER SALES
#--------------------------------------------------------------------------------------------------

result_EU_Sales_arima <- forecast_autoarima(EU_Consumer_Sales_train$Tot.Sales,EU_Consumer_Sales$Tot.Sales,EU_Consumer_Sales_test$Tot.Sales)

#a) lets look into what autoarima function is returning

result_EU_Sales_arima[[1]]
# ARIMA(2,1,0)                    
# 
# Coefficients:
#   ar1      ar2
# -0.5796  -0.4906
# s.e.   0.1346   0.1310
# 
# sigma^2 estimated as 168564657:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(result_EU_Sales_arima[[1]])
plot(result_EU_Sales_arima[[1]]$x, col="black",xlab="Months",ylab="EU Sales")
lines(fitted(result_EU_Sales_arima[[1]]), col="red")

# b) Again, let's check if the residual series is white noise using Dickey fuller and KPSS tests

result_EU_Sales_arima[[2]]

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima
# Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary


result_EU_Sales_arima[[3]]

# KPSS Test for Level Stationarity
# data:  resi_auto_arima
# KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

#c. Calculating accuracy using MAPE

result_EU_Sales_arima[[4]] #[1] 28.9226

#d. Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

plot(EU_Consumer_Sales$Tot.Sales, col = "black",xlab="Months",ylab="EU Sales")
lines(result_EU_Sales_arima[[5]], col = "red")


#e.Storing the forecasted Demand for last 6 months[July 2014-Dec 2014 for known data]
EU_Sales_arimaforecast1 <- result_EU_Sales_arima[[7]]
View(EU_Sales_arimaforecast1)

#f. Storing the forecasted Demand data for months from Jan 2015 to June 2015

EU_Sales_arimaforecast2<- result_EU_Sales_arima[[8]]
View(EU_Sales_arimaforecast2)

#--------------------------------------------------------------------------------------------------
#     AUTO ARIMA MODELLING FOR EU CONSUMER QUANTITY
#--------------------------------------------------------------------------------------------------

result_EU_Qty_arima <- forecast_autoarima(EU_Consumer_Qty_train$Tot.Qty,EU_Consumer_Qty$Tot.Qty ,EU_Consumer_Qty_test$Tot.Qty)

#a) lets look into what autoarima function is returning

result_EU_Qty_arima[[1]]
# ARIMA(2,1,0)                    
# 
# Coefficients:
#   ar1      ar2
# -0.7359  -0.5879
# s.e.   0.1224   0.1185
# 
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(result_EU_Qty_arima[[1]]) #Diagnostic plots
plot(result_EU_Qty_arima[[1]]$x, col="black",xlab="Months",ylab="EU  Qty")
lines(fitted(result_EU_Qty_arima[[1]]), col="red")


# b) Again, let's check if the residual series is white noise using Dickey fuller and KPSS tests

result_EU_Qty_arima[[2]]

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima
# Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
# alternative hypothesis: stationary

result_EU_Qty_arima[[3]]

# KPSS Test for Level Stationarity
# data:  resi_auto_arima
# KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1


#c. Calculating accuracy using MAPE

result_EU_Qty_arima[[4]] #[1] 30.13319

#d. Lastly, let's plot the predictions along with original values, to get a visual feel of the fit

plot(EU_Consumer_Qty$Tot.Qty, col = "black",xlab="Months",ylab="EU Qty")
lines(result_EU_Qty_arima[[5]], col = "red")


#e.Storing the forecasted Demand for last 6 months[July 2014-Dec 2014 for known data]
EU_Qty_arimaforecast1 <- result_EU_Qty_arima[[7]]
View(EU_Qty_arimaforecast1)

#f. Storing the forecasted Demand data for months from Jan 2015 to June 2015

EU_Qty_arimaforecast2<- result_EU_Qty_arima[[8]]
View(EU_Qty_arimaforecast2)

############################ARIMA MODELLING ENDS HERE ######################################################

#-----------------------------------------------------------------------------------------------------------
#                                       CONCLUSION
#----------------------------------------------------------------------------------------------------------


#Choosing the Best models based on the MAPE
#Here is the summary of MAPE for different Models derived from Classical decomposition and Auto ARIMA techniques

# Parameter	MAPE (Classic Decomposition)	MAPE (Auto Arima)
# APAC Sales	    20.88024			          27.68952
# APAC Quantity	  26.13344			          26.24458
# EU Sales	      24.61031			          28.9226
# EU Quantity	    27.43684			          30.13319


#From the above summary, The classical decomposition models gives better MAPE than Auto ARIMA function
#Hence our forecasting will be based on the Classical decompostion model
#The final forecast are stored in the below dataframes

#APAC CONSUMER SALES
APAC_Sales_forecast2
View(APAC_Sales_forecast2)

#APAC CONSUMER QUANTITY
APAC_Qty_forecast2
View(APAC_Qty_forecast2)

#EU CONSUMER SALES
EU_Sales_forecast2
View(EU_Sales_forecast2)

#EU_CONSUMER QUANTITY
EU_Qty_forecast2
View(EU_Qty_forecast2)

#-----------------------------------------------------------------------------------------------------------------
#                     ANALYSIS ENDS HERE
#----------------------------------------------------------------------------------------------------------------
