```{r}
#We must install all the necessary packages to run our code. By some foresight,
#the following packages will be required to run our code
#ggplot2 is a plotting system for R, based on the grammar of graphics
install.packages("ggplot2")
#Below:Data for an Introduction to Statistical Learning with Applications in R
install.packages("ISLR")
install.packages("logspline")
#To use date and time functions, we must install the lubridate package
install.packages("lubridate")
#to use the function fread, we will have to install the pakage data.table
install.packages("data.table")
install.packages('curl')
install.packages("rmarkdown")
install.packages("leaps")
# to use the vif() function to check  correlattion between two variables, we will have to load the car package
install.packages("car")
install.packages("gridExtra")

library (car)
library(gridExtra)
library (leaps)
library(lubridate)
library(ISLR)
library(fitdistrplus)
library(logspline)
library(ggplot2)
library(data.table)
library (randomForest)
library(plyr)
```

```{r}
 #downloaded and loaded into R the trip data for September 2015 from the function fread
data <- fread("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")

#To find the number of rows and columns extracted
dim(data)
data
```

```{r}
##Data Cleaning. Removing NAs and 0s where not appropriate
data<-data[-which(data$Total_amount<=0)]
data<-data[-which(data$Trip_distance==0)]
data$VendorID[data$VendorID==99]<-2

##Detecting NAs in column
ind<-rep(0,22)
ind<-count(is.na(data))
data<-subset(data, select = -c(Ehail_fee) )
```

```{r}
#To plot the histogram for trip distance(i.e its frequency distribution)
#I also tried using the hist() function but the graph wasn't visually appealing 
qplot(data$Trip_distance, geom="histogram",binwidth = 0.25, main = "Trip Distance Frequency", xlab = "Trip Distance", ylab="Count", fill=I("white"), col=I("red"), xlim = c(0,25))
```

```{r}
#We now plot the cumilative density of our Trip istance and also fit lognormal and standard normal distribution
ggplot(data=data, aes(data$Trip_distance))  + 
  geom_density(col=I("black"), fill="grey") + 
  labs(title="Distribution of Trip Distance") +
  labs(x= "Trip Distance", y="Count") +
  geom_area(stat="function", fun=dlnorm,color="blue", fill="darkseagreen1", alpha=0.5)+
  geom_area(stat="function", fun=dnorm,color="black", fill="coral1", alpha=0.5)+
  scale_colour_manual(breaks=c("lognormal","normal","Continuous density") ,values=c("darkseagreen1","coral1","grey"))+
  xlim (c(0,20))
```

```{r}
##########Feature Extraction##############

#using the hour function we extract the hour component from both pickup and drop off date-times
data$pickup_hour <- hour(data$lpep_pickup_datetime)
data$drop_hour <- hour(data$Lpep_dropoff_datetime)
#data$pickup_week <- week(data$lpep_pickup_datetime)

#The code below extracts the week number from date time which is then converted into numeric format for processing it further 
data$pickup_week <-strftime(data$lpep_pickup_datetime, format = "%V")
data$pickup_week<-as.numeric(data$pickup_week)

#The code below is used to extract the day of week from the date time variable
data$day <- weekdays(as.Date(data$lpep_pickup_datetime))
```

```{r}
#we will need to factor our variable Store_and_fed_flag if we have to use it in a model
data$Store_and_fwd_flag<-as.factor(data$Store_and_fwd_flag)
```

```{r}
pickt<-as.POSIXlt((strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S")))
dropt<-as.POSIXlt((strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S")))

#diff is our variable for storing our time duration in hours
data$diff<-as.numeric(dropt-pickt)/3600
#Remving trips with time duration 0
data<-data[-which(data$diff== 0)]
```

```{r}
#We extract a varaible to see in the trip had a pickup or dropoff at upper Manhattan
#Upper Manhattan is a wealthy area and is very likely to influence the tip the driver gets
range2<-function(pickup_lat,pickup_long,drop_lat,drop_long)
   {
       #Checking for upper Manhattan
         Manhattan_max<-c(40.877175, -73.924087)
         Manhattan_min<-c(40.783445, -73.944354)
          pickup<-c(pickup_lat,pickup_long)
         
           #Checking if the extracted point falls within our polygon around the terminals
           #for pickup
           if(all(Manhattan_min<pickup)&& all(Manhattan_max>pickup))
             {
                 return(TRUE)
                 
                 }
         dropoff<-c(drop_lat,drop_long)
         
           #for dropoff
           if(all(Manhattan_min<dropoff)&&all(Manhattan_max>dropoff))
             {
                 return(TRUE)
               }
         return(FALSE)
       }
index2<-mapply(range2, data$Pickup_latitude, data$Pickup_longitude,data$Dropoff_latitude,data$Dropoff_longitude)index2
data$UMan<-index2
```

```{r}
#######Finding the mean and median of the data by pickup hour
mean.hour<-rep(0,24)
median.hour<-rep(0,24)

#I tried using the for loop but it wasnt working and started assigning value to only the first vector
#The following code did'nt operate correctly
#for(j in 0:23)
#+ {
#  +   if(data$pickup_hour==j)
#    +     {
#      +     mean.hour[j+1]<- mean(data$Trip_distance[data$pickup_hour==j])
#      +     median.hour[j+1]<- median(data$Trip_distance[data$pick_hour==j])
#      +   }
#  + }

data.mean<-aggregate(data$Trip_distance~data$pickup_hour,FUN=mean)
data.mean[,2]<-round(data.mean[,2],2)
data.f1<-data.frame('Pickup.Hour'= data.mean[,1],'Mean.Trip.Distance'=data.mean[,2])

print("The table of Mean Trip Distance by the Hour of the day is displayed below:")
data.f1

ggplot(data.f1,aes(Pickup.Hour,Mean.Trip.Distance)) + 
  geom_bar(stat="identity",fill="dodgerblue2")+ 
  labs(title="Mean Trip Distance by Hour of Day") +
  labs(x= "Hour of Day", y="Mean trip Distance") 
```

```{r}
data.median<-aggregate(data$Trip_distance~data$pickup_hour,FUN=median)
data.f2<-data.frame('Pickup.hour'= data.median[,1],'Mean.Trip.Distance'=data.median[,2])

print("The table of Median Trip Distance by the Hour of the day is displayed below:")
data.f2

ggplot(data.f1,aes(Pickup.Hour,Mean.Trip.Distance)) + 
  geom_bar(stat="identity",fill="dodgerblue2")+ 
  labs(title="Median Trip Distance by Hour of Day") +
  labs(x= "Hour of Day", y="Median trip Distance") 

```{r}
######Finding wether the pickup or drop location is from the airport

#Airport1= JFK airport
#Airport2= LaGaurdia airport
#We define a function which takes the value from the pickup latitude, pickup longitude,
#dropoff latitude and drop longitude

range<-function(pickup_lat,pickup_long,drop_lat,drop_long)
{
# Using google maps I made a POLYGON around the TERMINALS of the airports
# I chose the bottom most left edge and top most right edge of the polygon when the map points NORTH
# For JFK Airport

Airport1_min<-c(40.640,-73.792)
Airport1_max<-c(40.650,-73.776)
pickup<-c(pickup_lat,pickup_long)

#Checking if the extracted point falls within our polygon around the terminals
#for pickup
if(all(Airport1_min<pickup)&& all(Airport1_max>pickup))
{
  return(TRUE)
  
}
dropoff<-c(drop_lat,drop_long)

#for dropoff
if(all(Airport1_min<dropoff)&&all(Airport1_max>dropoff))
{
  return(TRUE)
}

#for LaGaurdia Airport
Airport2_min<-c(40.767,-73.882)
Airport2_max<-c(40.775,-73.864)
if(all(Airport2_min<pickup)&& all(Airport2_max>pickup))
{
  return(TRUE)
  
}

if(all(Airport2_min<dropoff)&&all(Airport2_max>dropoff))
{
  return(TRUE)
}
return(FALSE)
}
```

```{r}
#Extracting the returned value from the range function usimgvariable index
index<-mapply(range, data$Pickup_latitude, data$Pickup_longitude,data$Dropoff_latitude,data$Dropoff_longitude)
data$Airport<-index
air<- c(length(which(index==TRUE)), length(which(index==FALSE)),round(mean(data[index,]$Total_amount),2), round(mean(data[!index,]$Total_amount),2), round(mean(data[index]$Trip_distance),2), round(mean(data[!index,]$Trip_distance),2),round(mean(data[index]$Tip_amount),2),round(mean(data[!index,]$Tip_amount),2))
air1<- as.table(matrix(air, ncol=2, byrow=TRUE))
rownames(air1)<-c("Trip Frequency","Average Total Amount($)","Average Trip Distance(miles)","Average Tip Amount($)")
colnames(air1)<-c("Airport trips","Other trips")
d<-head(air1[,1:2])

#Plotting a table
grid.table(d)

#Plotting a pie chart
slices<-(air1[1,])
lbls <- c("Airport trips", "Other Trips")
pie(slices,labels=lbls,main="Trip Frequency")
ggplot(air1,aes(Pickup.Hour,Mean.Trip.Distance)) + 
  geom_bar(stat="identity",fill="deeppink")+ 
  labs(title="Median Trip Distance by Hour of Day") +
  labs(x= "Hour of Day", y="Median trip Distance")
```

```{r}
####Percentage tip variable
data$percent_tip<-(data$Tip_amount/data$Total_amount)*100

###Tip amount vs Total amount
ggplot(data, aes(y=data$Tip_amount, x=data$Total_amount)) +geom_point(color="indianred4")+ geom_abline()
  labs(title="Tip Amount vs Total Amount") +
  labs(x= "Total Amount", y="Tip Amount")
```
  
```{r}
####Developing a model to predict percent_tip
set.seed(1)
train<-sample((1:dim(data)[1]),(dim(data)[1])*0.7)
data.train<-data[train,]
data.test<-data[-train,]
head(data)
set.seed(1)

#I tried using subset selection but didnt get interestig results
#regfit.bwd=regsubsets( percent_tip??? Tolls_amount+VendorID+Extra+Trip_type+Fare_amount+Passenger_count+Payment_type+Trip_distance+drop_hour+Airport+MTA_tax+improvement_surcharge+pickup_week ,data=data.train, nvmax =9)
#summary(regfit.bwd,9)

#Simple linear regression
#This model was obtained after checking for correlation using the vif() function...
#.., the p value associated with individual predictors and the adjusted R square value
lm.fit<-lm(percent_tip ~ Tolls_amount+Extra+day+VendorID+diff+Trip_type+Passenger_count+Payment_type+pickup_week+drop_hour+Airport+UMan,data=data.train)
summary(lm.fit)
```

```{r}
#checking for co-reation between variables
vif(lm.fit)

#Finding the MSE, mean square  error of our fitted model
mean((data.test$percent_tip -predict (lm.fit ,data.test))^2)
```

```{r}
###Derving Trip_speed as a function of Trip_distance and diff, the time duration of the trip
data$Trip_speed<-data$Trip_distance/data$diff

#We remove all values where Trip_speed is less than 1 or more than 200
data<-data[-which(data$Trip_speed<1)]
data<-data[-which(data$Trip_speed>200)]
```

```{r}
#Finding the aggregate Trip distance 
d.f<- aggregate(data$Trip_speed~data$pickup_week,FUN = mean)
d.f[,1]<-d.f[,1]-35
d.f2<-data.frame('Week.of.Month'= d.f[,1],'Trip.Speed'=d.f[,2])

#plotting Trip Speed vs Week of Month using ggplot
ggplot(d.f2,aes(Week.of.Month,Trip.Speed)) + 
  geom_bar(stat="identity",fill="lightpink")+ 
  labs(title="Average speed by week") +
  labs(x= "Week of month", y="Average Speed (miles/hour)")
```

```{r}
#Performing ANOVA Hypothesis test
anv<-aov(Trip_speed~pickup_week,data=data)
summary(anv)
#As p-value comes out to be very ow, the null hypothesis fails

#Performing linear regression
lm.f<-lm(Trip_speed~pickup_week,data=data)
summary(lm.f)
#As p-value comes out to be very ow, the null hypothesis fails

#Other possible Tests which can be performed are-
#lvtest<-leveneTest(Trip_speed ~ pickup_week, data =data)
#summary(lvtest)

#kruskal.test(Trip_speed ~ pickup_week, data = data)
```

```{r}
#One of the possible hypothesis that the average trip speeds are different...
#..is that the passenger count decreases with Pickup week

#This could mean more traffic and consequent decrease in the trip speed
lm2<-lm(Passenger_count~pickup_week,data=data)
summary(lm2)
#As expected the coefficient is negative and the p-value is very small so we reject the null hypothesis
```

```{r}
#To confirm, we plot Passengers per 100 cars with week of month
d.fp<- aggregate(data$Passenger_count~data$pickup_week,FUN = mean)
d.fp[,1]<-d.fp[,1]-35
d.fp2<-data.frame('Pickup_Week'= d.fp[,1],'Mean_passenger_count'=round(d.fp[,2]*100))
ggplot(d.fp2,aes(Pickup_Week,Mean_passenger_count)) + 
  geom_line()+ geom_point()+
  labs(title="Passengers per 100 cars by Week of Month") +
  labs(x= "Week of month", y="Passengers per 100 cars")
```

```{r}
#Testing this in anova
anvp<-aov(Passenger_count~pickup_week,data=data)
summary(anvp)

#Performing Hypothesis test on Trip Speed with pickup hour
#ANOVA
anv2<-aov(Trip_speed~pickup_hour,data=data)
summary(anv2)

#Linear Regression
lm.f2<-lm(Trip_speed~pickup_week,data=data)
summary(lm.f2)
```

```{r}
#Plotting a bar chart of Average Speed with hour of day with a mean line
d.f3<- aggregate(data$Trip_speed~data$pickup_hour,FUN = mean)
d.f3
d.f4<-data.frame('Time.of.Day'= d.f3[,1],'Trip.Speed'=d.f3[,2])
mean_line<-mean(d.f4[,2])
ggplot(d.f4,aes(Time.of.Day,Trip.Speed)) + 
  geom_bar(stat="identity",fill="mediumpurple")+ 
  labs(title="Average speed by Hour") +
  labs(x= "Hour of Day", y="Average Speed (miles/hour)")+geom_line(y=mean_line)
```
