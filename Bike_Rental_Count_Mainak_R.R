#BIKE RENTAL COUNT

#Clearing the workspace
rm(list = ls())

#Setting the working directory
setwd("C:/Users/Mainak Sarkar/Desktop/Edwisor/Project/Bike_Rental_Count")

#Importing the data skipping the blank lines if any
data = read.csv("day.csv", blank.lines.skip = TRUE)

#Checking the summary of the whole data set
summary(data)

#Checking class of each column
class(data$instant)                         #integer
class(data$dteday)                          #factor
class(data$season)                          #integer
class(data$yr)                              #integer
class(data$mnth)                            #integer
class(data$holiday)                         #integer
class(data$weekday)                         #integer
class(data$workingday)                      #integer
class(data$weathersit)                      #integer
class(data$temp)                            #numeric
class(data$atemp)                           #numeric
class(data$hum)                             #numeric
class(data$windspeed)                       #numeric
class(data$casual)                          #integer
class(data$registered)                      #integer
class(data$cnt)                             #integer





#***********************************DATA PREPROCESSING***********************************

#Data type conversion
#converting "season" as factor
data$season = as.factor(data$season)

#converting "mnth" as factor
data$mnth = as.factor(data$mnth)

#converting "holiday" as factor
data$holiday = as.factor(data$holiday)

#converting "weekday" as factor
data$weekday = as.factor(data$weekday)

#converting "workingday" as factor
data$workingday = as.factor(data$workingday)

#Converting "dteday" as date-time format
data$dteday = as.POSIXct(data$dteday, format="%Y-%m-%d")

#"yr" and "weathersit" is kept numerical
#The data set contains only 3 types of weathersit, but according to problem statement there are 4 types
#So we kept weathersit as numerical variable so that it can predict for instances of number 4
#We kept yr as numerical so that we can predict for future cases as well





#***********************************MISSING VALUE ANALYSIS***********************************

#Checking null values
missing_value = data.frame(apply(data, 2, function(x){sum(is.na(x))}))

#Calculating Percentage of missing values
missing_value$percentage = (missing_value[, 1]/nrow(data))*100

#SO the dataset has no missing values





#***********************************OUTLIER ANALYSIS***********************************

#Selecting numerical variables
numeric_index = sapply(data, is.numeric)
numeric_data = data[, numeric_index]
numerical_cnames = colnames(numeric_data)

#Imputing null values in place of outliers
for (i in numerical_cnames[-c(1,8,9,10)])                    #"instant" and all dependent variables are removed
{
  val = data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
  data[,i][data[,i]%in%val] = NA
}

#Checking number of outliers(null values)
outliers = data.frame(apply(data, 2, function(y){sum(is.na(y))}))

#Calculating Percentage of outliers(null values)
outliers$percentage = (outliers[,1]/nrow(data))*100

#As we know that presence of ouliers can affect our models a lot
#And the no. of observations in the dataset is small, so we can't delete the outliers
#Thus we have to opt for imputation





#***********************************ONLY FOR FINDING THE BEST METHOD***********************************
#Selecting variables containing NAs
NA_index = sapply(data, anyNA)
NA_data = data[, NA_index]
NA_cnames = colnames(NA_data)

#Choosing the best method for missing value imputation

#Making a sample to check which method works best
#Choosing a sample and saving its value
sample_NA = data[51, c(12,13)]

#Putting values of sample equal to NA for required columns
data[51,c(NA_cnames)] = NA

#duplicating data
data_duplicate = data

#MEAN Method
for(b in NA_cnames)
  data[, b][is.na(data[, b])] = mean(data[, b], na.rm = TRUE)

sample_NA_mean = data[51, c(12,13)]

#MEDIAN Method
data = data_duplicate
for(c in NA_cnames)
  data[, c][is.na(data[, c])] = median(data[, c], na.rm = TRUE)

sample_NA_median = data[51, c(12,13)]

#Comparing different imputing methods
sample = rbind(sample_NA, sample_NA_mean, sample_NA_median)

#Inserting a new blank row in "sample"
sample[nrow(sample)+1, ]=NA

#Changing row names
row.names(sample) = c("sample_NA","sample_NA_mean","sample_NA_median","Best Method")

#Finding the best method of imputation for each column
for (d in (1:ncol(sample)))
{
  if(abs(as.numeric(sample[1,d])-as.numeric(sample[2,d]))<abs(as.numeric(sample[1,d])-as.numeric(sample[3,d])))
  {
    sample[4,d] = "MEAN"
  } else {
    sample[4,d] = "MEDIAN"
  }
}

#From "sample" dataframe we can find the best method for each column





#**************************************************************************************

#Imputing the best fit method for each column
#Re-Run the data till-"ONLY FOR FINDING THE BEST METHOD"
data$hum[is.na(data$hum)] = median(data$hum, na.rm = TRUE)
data$windspeed[is.na(data$windspeed)] = mean(data$windspeed, na.rm = TRUE)





#***********************************DATA MANIPULATION***********************************

#Deleting unnecessary variables
data = data[, -c(1,2)]

#Creating functions to change all normalised variable to their actual values
Temp = function(t)
{
  t_max = 39
  t_min = -8
  temperature = t*(t_max - t_min)+t_min
  return(temperature)
}

ATemp = function(at)
{
  at_max = 50
  at_min = -16
  a_temperature = at*(at_max - at_min)+at_min
  return(a_temperature)
}

#Converting to actual values
#temp
data$temp = sapply(data$temp, Temp)

#atemp
data$atemp = sapply(data$atemp, ATemp)

#hum
data$hum = sapply(data$hum, function(x){x*100})

#windspeed
data$windspeed = sapply(data$windspeed, function(x){x*67})

#The total count of users is a summation of casual and registered users
#checking if there is any rows other than that
num = 0
for (j in (1:nrow(data)))
{
  if(data[j, 14] != (data[j, 13] + data[j, 12]))
  {
    num = num + 1
  }
}
#As n = 0 so we can assure that there is no irrelevant rows





#***********************************EXPOLATORY DATA ANALYSIS***********************************

library(ggplot2)
library(reshape2)

#Stacked barplots of season
means_season = aggregate(data[, c(12,13,14)], by = list(data$season), mean)
names(means_season)[1] = "season"

means_long_season = melt(means_season, id.vars = "season")

ggplot(means_long_season, aes(x = variable, y = value, fill = factor(season))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("season") +
  ylab("Mean of users")

#We can see that casual, registered and total count all are highest for season 3 and lowest for 1
#so when season is "fall" people are more likely to rent a bike




#Stacked barplots of year
means_yr = aggregate(data[, c(12,13,14)], by = list(data$yr), mean)
names(means_yr)[1] = "yr"

means_long_yr = melt(means_yr, id.vars = "yr")

ggplot(means_long_yr, aes(x = variable, y = value, fill = factor(yr))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Year") +
  ylab("Mean of users")

#From the plots we can say that the count of registered users had heavily increased in 2012 compared to 2011
#And casual users also increased a bit, so the rental company is becoming popular
#So the rental will be higher for future years as well



#Stacked barplots of month
means_mnth = aggregate(data[, c(12,13,14)], by = list(data$mnth), mean)
names(means_mnth)[1] = "mnth"

means_long_mnth = melt(means_mnth, id.vars = "mnth")

ggplot(means_long_mnth, aes(x = variable, y = value, fill = factor(mnth))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Month") +
  ylab("Mean of users")

#we can see that the number of users is comparetively more in months 5 to 9




#Stacked barplots of weekday
means_weekday = aggregate(data[, c(12,13,14)], by = list(data$weekday), mean)
names(means_weekday)[1] = "weekday"

means_long_weekday = melt(means_weekday, id.vars = "weekday")

ggplot(means_long_weekday, aes(x = variable, y = value, fill = factor(weekday))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("weekday") +
  ylab("Mean of users")

#we can see that the number of casual users in more during weekends as casual users take bikes for trips
#Number of registered users is more during weekdays as they mainly consists of office goers




#Stacked barplots of Holiday
means_holiday = aggregate(data[, c(12,13,14)], by = list(data$holiday), mean)
names(means_holiday)[1] = "holiday"

means_long_holiday = melt(means_holiday, id.vars = "holiday")

ggplot(means_long_holiday, aes(x = variable, y = value, fill = factor(holiday))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Holiday") +
  ylab("Mean of users")

#So if it is a holiday the number of casual users is more and registered users is less as compared to a non-holiday




#Stacked barplots of workingday
means_workingday = aggregate(data[, c(12,13,14)], by = list(data$workingday), mean)
names(means_workingday)[1] = "workingday"

means_long_workingday = melt(means_workingday, id.vars = "workingday")

ggplot(means_long_workingday, aes(x = variable, y = value, fill = factor(workingday))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Workingday") +
  ylab("Mean of users")

#Naturally if it is a working day the number of registered users is more as people goes to work renting a bike





#Stacked barplots of weathersit
means_weathersit = aggregate(data[, c(12,13,14)], by = list(data$weathersit), mean)
names(means_weathersit)[1] = "weathersit"

means_long_weathersit = melt(means_weathersit, id.vars = "weathersit")

ggplot(means_long_weathersit, aes(x = variable, y = value, fill = factor(weathersit))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("weathersit") +
  ylab("Mean of users")

#So we can conclude that weather situation no. 1 is most preferable for bike renting
#And weather situation no. 4 will be least as the weather condition is detoriating from 1 to 4





#Scatter plot of Year and Different type of users
ggplot(data, aes_string(x = data[, 2], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 2], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 2], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#The regression line shows a positive relationship between year and count of users
#So we can say that the no. of rentals is progressively increasing




#Scatter plot of Weather Situation and Different type of users
ggplot(data, aes_string(x = data[, 7], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Weather Condition") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 7], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Weather Condition") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 7], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Weather Condition") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#The regression line shows a negative relationship between weather and number of users
#So as the weather value is increasing the weather condition is getting worse so number of rentals is decreasing
#So for weather condition 4 the count should be lesser



#Scatter plot of Temperature and Different type of users
ggplot(data, aes_string(x = data[, 8], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Temperature") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 8], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Temperature") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 8], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Temperature") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#From the we can see that the curve is steeper in case of registered users as they have to rent bike for offices, thus we can see atleast some good number of users even in case of extremes of temperature
#In case of casual users, if temperature is too low then number of users is low and if temperature is too high then number of casual users decreases, and mainly we can see most casual users during moderate temperatures




#Scatter plot of Feeled Temperature and Different type of users
ggplot(data, aes_string(x = data[, 9], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Feeled Temperature") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 9], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Feeled Temperature") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 9], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Feeled Temperature") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#From the we can see that the curve is steeper in case of registered users as they have to rent bike for offices, thus we can see atleast some good number of users even in case of extremes of temperature
#In case of casual users, if temperature is too low then number of users is low and if temperature is too high then number of casual users decreases, and mainly we can see most casual users during moderate temperatures




#Scatter plot of Humidity and Different type of users
ggplot(data, aes_string(x = data[, 10], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Humidity") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 10], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Humidity") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 10], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Humidity") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#No. of users is very less dependent on humidity so change of humidity doesn't affect the rental count very much except in extreme cases
#Humidity is somehow negetively related to the number of users
#As humidity increases people are a bit less likely to rent a bike




#Scatter plot of Windspeed and Different type of users
ggplot(data, aes_string(x = data[, 11], y = data[, 12])) +
  geom_point() +
  theme_bw() +
  xlab("Wind Speed") +
  ylab("No. of Casual users") +
  geom_smooth(method = lm, colour = "red")

ggplot(data, aes_string(x = data[, 11], y = data[, 13])) +
  geom_point() +
  theme_bw() +
  xlab("Wind Speed") +
  ylab("No. of Registered users") +
  geom_smooth(method = lm, colour = "yellow")

ggplot(data, aes_string(x = data[, 11], y = data[, 14])) +
  geom_point() +
  theme_bw() +
  xlab("Wind Speed") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "orange")

#Wind Speed is also negatively related to the no. of users
#People are less likely to rent a bike if the wind speed is too high




#Histogram plot of Casual Users
ggplot(data, aes(x = data$casual)) +
  geom_histogram(fill = "blue", colour = "yellow") + geom_density() +
  theme_bw() + xlab("Casual Users") + ylab("Frequency") +
  ggtitle("Histogram plot of Casual Users")

#Plot is right skewed

#Histogram plot of logarithm of Casual Users
ggplot(data, aes(x = log(data$casual))) +
  geom_histogram(fill = "yellow", colour = "blue") + geom_density() +
  theme_bw() + xlab("Logarithm of casual Users") + ylab("Frequency") +
  ggtitle("Histogram plot of Log of Casual Users")

#Log of casual users have a smooth, bell-shaped curve compared to that of casual users
#so we would use log of casual for further analysis




#Histogram plot of Registered Users
ggplot(data, aes(x = data$registered)) +
  geom_histogram(fill = "blue", colour = "yellow") + geom_density() +
  theme_bw() + xlab("Registered Users") + ylab("Frequency") +
  ggtitle("Histogram plot of Registered Users")

#Histogram plot of logarithm of Registered Users
ggplot(data, aes(x = log(data$registered))) +
  geom_histogram(fill = "yellow", colour = "blue") + geom_density() +
  theme_bw() + xlab("Logarithm of Registered Users") + ylab("Frequency") +
  ggtitle("Histogram plot of Log of Registered Users")

#In this case taking logarithm makes the histogram a bit left skewed
#The original curve is much better to fit so we would retain that



#So taking the log of casual users for further calculations
for (z in (1:nrow(data)))
{
  data[z,15] = log(data[z,12])
}
#Naming the column
names(data)[15] = "log_casual"





#***********************************FEATURE SELECTION***********************************

#Correlation Analysis
cor_index = sapply(data, is.numeric)

library(corrgram)
corrgram(data[, cor_index], order = FALSE, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")



#From the correlation plot we can also see that variables "temp" and "atemp" are highly correlated
#So we need to remove one of them

#Checking correlation coefficient value with dependent variable
#temp
cor.test(data[,8], data[,14])
#atemp
cor.test(data[,9], data[,14])
#Correlation between dependent variable and "atemp" (0.63) is slightly more than that with "temp" (0.62)
#So we would delete "temp" from our dataset

#Removing "temp" to avoid multicollinearity
data = data[, -8]



#From the correlation plot we can also see that variables "weathersit" and "hum" are moderately correlated
#So we need to remove one of them

#Checking correlation coefficient value with dependent variable
#weathersit
cor.test(data[,7], data[,14])
#hum
cor.test(data[,9], data[,14])
#Correlation between dependent variable and "weathersit" (-0.30) is more than that with "hum" (-0.11)
#So we would delete "hum" from our dataset

#Removing "hum" to avoid multicollinearity
data = data[, -9]


#Other than these all the dependent variables are higly correlated which is completely normal




#Now to check the correlation between the categorical variables we would go for Chi_sq test

#From the EDA we have expected that there can be a correlation between "season" and "mnth" variables
#So checking Chi-sq value between "season" and "mnth"
chisq.test(table(data$season, data$mnth))

#As p_value is less than 0.05, so collinearity exists between the two variables
#So we will go for anova test later to check the better predictor and the other will be omitted



#Again from the EDA we have seen that "holiday" and "workingday" variables almost gives us the same information in alternate ways
#So checking Chi-sq value between "holiday" and "workingday"
chisq.test(table(data$holiday, data$workingday))

#As p_value is less than 0.05, so collinearity exists between the two variables
#So we will check the better predictor and the other will be omitted




#Now to check which is a better dependent and independent variables we will go for ANOVA test

#ANOVA(Analysis of Variance) Test

#ANOVA for season
plot(cnt ~ season, data = data)
summary(aov(cnt ~ season, data = data))

#ANOVA for month
plot(cnt ~ mnth, data = data)
summary(aov(cnt ~ mnth, data = data))

#So we can see that both "season" and "month" have p_value less than 0.05
#So both of them are significant predictors
#But we know because of multicollinearity we need to delete one of them
#So looking at the F_value we can see that F_value is more for "season" (129) than that of "mnth" (42)
#So "season" explains more variance of the dependent variable
#Thus we will delete "mnth" from our analysis

data = data[, -3]

#ANOVA for weekday
plot(cnt ~ weekday, data = data)
summary(aov(cnt ~ weekday, data = data))

#ANOVA for holiday
plot(cnt ~ holiday, data = data)
summary(aov(cnt ~ holiday, data = data))

#ANOVA for workingday
plot(cnt ~ workingday, data = data)
summary(aov(cnt ~ workingday, data = data))

#So we have seen that that p_values of "weekday", "holiday", "workingday" is greater than 0.05
#so these variables doesnot influence the total count of users
#But there is a chance that they have some effect on casual and registered users
#As we have checked from the bar plots earlier that count of casual and registered users changes on these variables
#So we will do anova tests on these variables using casual and registered user as dependent variable

#CASUAL users as dependent variable
#ANOVA for weekday
plot(casual ~ weekday, data = data)
summary(aov(casual ~ weekday, data = data))

#ANOVA for holiday
plot(casual ~ holiday, data = data)
summary(aov(casual ~ holiday, data = data))

#ANOVA for workingday
plot(casual ~ workingday, data = data)
summary(aov(casual ~ workingday, data = data))

#Only "holiday" has p_value greater than 0.05



#REGISTERED users as dependent variable
#ANOVA for weekday
plot(registered ~ weekday, data = data)
summary(aov(registered ~ weekday, data = data))

#ANOVA for holiday
plot(registered ~ holiday, data = data)
summary(aov(registered ~ holiday, data = data))

#ANOVA for workingday
plot(registered ~ workingday, data = data)
summary(aov(registered ~ workingday, data = data))


#All the 3 variables have p_value less than 0.05, so they are significant predictors


#Moreover we can see that the few variables are not correlated to the total count "cnt" variable
#But are correlated to "casual" and "registered" users
#So we would predict casual and registered users individually and then sum them up to find the total count of users





#***********************************FEATURE SCALING***********************************

#Checking the distribution of the numerical variables for choosing the scaling method
#atemp
qqnorm(data$atemp)
qqline(data$atemp)
#The q plot deviates from the q line and have a curvy nature
hist(data$atemp)
#Histogram doesn't show a normal distribution


#windspeed
qqnorm(data$windspeed)
qqline(data$windspeed)
#The q plot deviates from the q line
hist(data$windspeed)
#The histogram is very close to normal distribution


#Normalization would be applied to scale as the all the variables are not normally distributed



#year
#Normalization is not required in case of year as there are only two values 0 and 1
#If there are more than two values then normalization is required


#weathersit
#weathersit contains 4 conditions
#max = 4, min = 1
data[, 'weathersit'] = (data[, 'weathersit'] - 1) / (4 - 1)


#atemp
#max = 50, min = -16
data[, 'atemp'] = (data[, 'atemp'] - (- 16)) / (50 - (- 16))


#windspeed
#Normalization is done by dividing by 67(max)
data[, 'windspeed'] = data[, 'windspeed'] / 67





#***********************************TRAIN-TEST SPLIT***********************************
#Checking the number of values in each factor variable
table(data$season)                #almost equal in each groups
table(data$yr)                    #almost equal across groups
table(data$weekday)               #almost equal across groups
table(data$workingday)            #Slightly biased
table(data$weathersit)            #Highly biased

#So as we can see that the dataset is baised across some categories so if we apply random sampling in this case
#Then there might be a chance that no observations of the low count groups is included
#So we need to apply stratified spliting in this case taking the most correlated variable as reference variable

#From the correlation plot we can see that weathersit would be the best variable to create the strata as it is highly biased and good correlation with dependent variable
set.seed(123)
library(caret)
train.index = createDataPartition(data$weathersit, p = 0.85, list = FALSE)
training_set = data[train.index,]
test_set = data[-train.index,]

#As the dataset is very small so we have only taken a small part of our data as test set





#***********************************MODEL BUILDING***********************************

#We will predict separately for casual and registered users


#*********************A)CASUAL USERS


#So taking logarithm of casual users as dependent variable

#Removing other dependent variables from training_set
train_casual = training_set[, -c( 9, 10, 11)]
test_casual = test_set[, -c( 9, 10, 11)]



#Linear Regression
#Model 1:
LR_casual1 = lm(log_casual ~ ., data = train_casual)
summary(LR_casual1)

#VIF(Variance Inflation factor) Test
library(car)
vif(LR_casual1)

#As the VIF result shows presence of aliased coefficients
#As we know that there is multicollinearity between "holiday" and "workingday"
#So by trial and error method we saw eliminating "workingday" gives better accuracy
#Deleting "workingday" from the data

train_casual = train_casual[, -5]
test_casual = test_casual[, -5]

#Model 2:
LR_casual2 = lm(log_casual ~ ., data = train_casual)
summary(LR_casual2)

#VIF(Variance Inflation factor) Test
vif(LR_casual2)

#Now VIF of all the variables is below 5, so we can build our model with these variables

predictions_LR_casual = predict(LR_casual2, test_casual[, -8])

#Calculating different Error Metrics
library(DMwR)
LR_casual_EM = regr.eval(test_set[, 9], exp(predictions_LR_casual) , stats = c("mape", "rmse"))



#Decision Tree Regressor
library(rpart)
DT_casual = rpart(log_casual ~ ., data = train_casual, method = "anova")
predictions_DT_casual = predict(DT_casual , test_casual[, -8])

#Calculating different Error Metrics
DT_casual_EM = regr.eval(test_set[, 9], exp(predictions_DT_casual) , stats = c("mape", "rmse"))

#Visualising the decision tree
library(rpart.plot)
rpart.plot(DT_casual, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)



#Random Forest Regression
library(randomForest)
RF_casual = randomForest(x = train_casual[c(1,2,3,4,5,6,7)], y = train_casual$log_casual)
predictions_RF_casual = predict(RF_casual, test_casual[, -8])

#Calculating different Error Metrics
RF_casual_EM = regr.eval(test_set[, 9], exp(predictions_RF_casual) , stats = c("mape", "rmse"))



#Support Vector Regression(SVR)
library(e1071)
SVR_casual = svm(formula = log_casual ~ ., data = train_casual, type = "eps-regression")
predictions_SVR_casual = predict(SVR_casual, test_casual[, -8])

#Calculating different Error Metrics
SVR_casual_EM = regr.eval(test_set[, 9], exp(predictions_SVR_casual) , stats = c("mape", "rmse"))



#K Nearest Neighbors(KNN)

#Finding the optimum value of K by Elbow Method
k_value_casual = data.frame()

for (m in (1:20))
{
  k_value_casual[m,1] = m
  predictions_casual = knnregTrain(train_casual[,1:7], test_casual[,1:7], train_casual$log_casual, k = m)
  K_val = regr.eval(test_set[, 9], exp(predictions_casual) , stats = "rmse")
  k_value_casual[m,2] = K_val
}
#renaming the column
names(k_value_casual)[1] = "K"
names(k_value_casual)[2] = "RMSE"
plot(k_value_casual$K,k_value_casual$RMSE, type = "l", col = "red", xlab = "K", ylab = "RMSE")

#So from the graph we can see that k = 15 gives us lower rmse value for our model

#Predicting the results with K = 15
predictions_KNN_casual = knnregTrain(train_casual[,1:7], test_casual[,1:7], train_casual$log_casual, k = 15)

#Calculating different Error Metrics
KNN_casual_EM = regr.eval(test_set[, 9], exp(predictions_KNN_casual) , stats = c("mape", "rmse"))


#To compare all models
Error_metrics_casual = as.data.frame(cbind(LR_casual_EM,DT_casual_EM,RF_casual_EM,SVR_casual_EM,KNN_casual_EM))
#Changing row names
row.names(Error_metrics_casual) = c("MAPE","RMSE") 



#By compairing the models we found that SVR is the best model for casual counts




#*********************B)REGISTERED USERS


#So taking registered users as dependent variable

#Removing other dependent variables from training_set
train_reg = training_set[, -c( 9, 11, 12)]
test_reg = test_set[, -c( 9, 11, 12)]



#Linear Regression
#Model 1:
LR_reg1 = lm(registered ~ ., data = train_reg)
summary(LR_reg1)

#VIF(Variance Inflation factor) Test
vif(LR_reg1)

#We can see from that the VIF values of variables "mnth" and "season" is significantly higher
#This represents a clear picture of multi-collinearity among the dependent variables
#VIF of mnth bieng the highest we would first delete this variable and rebuild our model

train_reg = train_reg[, -5]
test_reg = test_reg[, -5]

#Model 2:
LR_reg2 = lm(registered ~ ., data = train_reg)
summary(LR_reg2)

#VIF(Variance Inflation factor) Test
vif(LR_reg2)

#Now VIF of all the variables is below 5, so we can build our model with these variables

predictions_LR_reg = predict(LR_reg2, test_reg[, -8])

#Calculating different Error Metrics
LR_reg_EM = regr.eval(test_set[, 10], predictions_LR_reg , stats = c("mape", "rmse"))



#Decision Tree Regressor
DT_reg = rpart(registered ~ ., data = train_reg, method = "anova")
predictions_DT_reg = predict(DT_reg , test_reg[, -8])

#Calculating different Error Metrics
DT_reg_EM = regr.eval(test_set[, 10], predictions_DT_reg , stats = c("mape", "rmse"))

#Visualising the decision tree
rpart.plot(DT_reg, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)



#Random Forest Regression
RF_reg = randomForest(x = train_reg[c(1,2,3,4,5,6,7)], y = train_reg$registered)
predictions_RF_reg = predict(RF_reg, test_reg[, -8])

#Calculating different Error Metrics
RF_reg_EM = regr.eval(test_set[, 10], predictions_RF_reg , stats = c("mape", "rmse"))



#Support Vector Regression(SVR)
SVR_reg = svm(formula = registered ~ ., data = train_reg, type = "eps-regression")
predictions_SVR_reg = predict(SVR_reg, test_reg[, -8])

#Calculating different Error Metrics
SVR_reg_EM = regr.eval(test_set[, 10], predictions_SVR_reg , stats = c("mape", "rmse"))



#K Nearest Neighbors(KNN)

#Finding the optimum value of K by Elbow Method
k_value_reg = data.frame()

for (n in (1:20))
{
  k_value_reg[n,1] = n
  predictions_reg = knnregTrain(train_reg[,1:7], test_reg[,1:7], train_reg$registered, k = n)
  K_val = regr.eval(test_set[, 10], predictions_reg , stats = "rmse")
  k_value_reg[n,2] = K_val
}
#renaming the column
names(k_value_reg)[1] = "K"
names(k_value_reg)[2] = "RMSE"
plot(k_value_reg$K,k_value_reg$RMSE, type = "l", col = "red", xlab = "K", ylab = "RMSE")

#So from the graph we can see that k = 14 gives us lower rmse value for our model

#Predicting the results with K = 14
predictions_KNN_reg = knnregTrain(train_reg[,1:7], test_reg[,1:7], train_reg$registered, k = 14)

#Calculating different Error Metrics
KNN_reg_EM = regr.eval(test_set[, 10], predictions_KNN_reg , stats = c("mape", "rmse"))



#To compare all models
Error_metrics_reg = as.data.frame(cbind(LR_reg_EM,DT_reg_EM,RF_reg_EM,SVR_reg_EM,KNN_reg_EM))
#Changing row names
row.names(Error_metrics_reg) = c("MAPE","RMSE")



#By compairing the models we found that SVR is the best model for registered counts



#So now we will add the casual and registered count and round it off to find the total count and compare it with the original data
predictions_cnt = round(predictions_SVR_reg + exp(predictions_SVR_casual))

cnt_EM = regr.eval(test_set[, 11], predictions_cnt , stats = c("mape", "rmse"))

#We got a fair result





#*************************************************************************


#TEST DATA


#Importing the data
test = read.csv("Input_sample.csv")





#***********************************DATA TYPE CONVERSION***********************************

test$season = as.factor(test$season)
test$yr = as.integer(test$yr)
test$holiday = as.factor(test$holiday)
test$weekday = as.factor(test$weekday)
test$weathersit = as.integer(test$weathersit)
test$atemp = as.numeric(test$atemp)
test$windspeed = as.numeric(test$windspeed)





#***********************************FEATURE SCALING***********************************

#weathersit
#weathersit contains 4 conditions
#max = 4, min = 1
test[, 'weathersit'] = (test[, 'weathersit'] - 1) / (4 - 1)


#atemp
#max = 50, min = -16
test[, 'atemp'] = (test[, 'atemp'] - (- 16)) / (50 - (- 16))


#windspeed
#Normalization is done by dividing by 67(max)
test[, 'windspeed'] = test[, 'windspeed'] / 67






#***********************************MODEL TUNING***********************************

#Now SVR model being the best model we will train it with the whole data set and apply some hyper parameter tuning


library(caret)
set.seed(123)
Best_casual = train(form = log_casual ~ ., data = data[, -c(5,9,10,11)], 
                   method = 'svmRadial', metric = 'RMSE')
Best_casual



Best_reg = train(form = registered ~ ., data = data[, -c(5,9,11,12)], 
                    method = 'svmRadial', metric = 'RMSE')
Best_reg





#***********************************PREDICTION OF TEST CASES***********************************

casual = round(exp(predict(Best_casual, test)))

registered = round(predict(Best_reg, test))

#Total output summing casual and registered counts
cnt = (casual + registered)



output = cbind(casual, registered, cnt)


#Saving as csv
write.csv(output, "output_sample_R.csv")

input_output = cbind(test, output)

write.csv(input_output, "input_output_sample_R.csv")





#***********************************TEST CASES VALIDATION***********************************


#Stacked barplots of season
means_season_io = aggregate(input_output[, c(8,9,10)], by = list(input_output$season), mean)
names(means_season_io)[1] = "season"

means_long_season_io = melt(means_season_io, id.vars = "season")

ggplot(means_long_season_io, aes(x = variable, y = value, fill = factor(season))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("season") +
  ylab("Mean of users")



#Stacked barplots of year
means_yr_io = aggregate(input_output[, c(8,9,10)], by = list(input_output$yr), mean)
names(means_yr_io)[1] = "yr"

means_long_yr_io = melt(means_yr_io, id.vars = "yr")

ggplot(means_long_yr_io, aes(x = variable, y = value, fill = factor(yr))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Year") +
  ylab("Mean of users")



#Stacked barplots of weekday
means_weekday_io = aggregate(input_output[, c(8,9,10)], by = list(input_output$weekday), mean)
names(means_weekday_io)[1] = "weekday"

means_long_weekday_io = melt(means_weekday_io, id.vars = "weekday")

ggplot(means_long_weekday_io, aes(x = variable, y = value, fill = factor(weekday))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("weekday") +
  ylab("Mean of users")



#Stacked barplots of Holiday
means_holiday_io = aggregate(input_output[, c(8,9,10)], by = list(input_output$holiday), mean)
names(means_holiday_io)[1] = "holiday"

means_long_holiday_io = melt(means_holiday_io, id.vars = "holiday")

ggplot(means_long_holiday_io, aes(x = variable, y = value, fill = factor(holiday))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Holiday") +
  ylab("Mean of users")



#Stacked barplots of weathersit
means_weathersit_io = aggregate(input_output[, c(8,9,10)], by = list(input_output$weathersit), mean)
names(means_weathersit_io)[1] = "weathersit"

means_long_weathersit_io = melt(means_weathersit_io, id.vars = "weathersit")

ggplot(means_long_weathersit_io, aes(x = variable, y = value, fill = factor(weathersit))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("weathersit") +
  ylab("Mean of users")



#Scatter plot of Year and Total counts
ggplot(input_output, aes_string(x = input_output[, 2], y = input_output[, 10])) +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "red")



#Scatter plot of Weather Situation and Total Counts
ggplot(input_output, aes_string(x = input_output[, 5], y = input_output[, 10])) +
  geom_point() +
  theme_bw() +
  xlab("Weather Condition") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "red")



#Scatter plot of Temperature and Different type of users
ggplot(input_output, aes_string(x = input_output[, 6], y = input_output[, 10])) +
  geom_point() +
  theme_bw() +
  xlab("Feeled Temperature") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "red")



#Scatter plot of Windspeed and Different type of users
ggplot(input_output, aes_string(x = input_output[, 7], y = input_output[, 10])) +
  geom_point() +
  theme_bw() +
  xlab("Wind Speed") +
  ylab("No. of Total users") +
  geom_smooth(method = lm, colour = "red")



#So almost all the variables shows the same curve as we expected except the weekday.