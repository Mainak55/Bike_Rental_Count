# -*- coding: utf-8 -*-
"""
Created on Wed Jun 19 20:17:56 2019

@author: Mainak Sarkar
"""

#BIKE RENTAL COUNT

#Setting the working directory
import os
os.chdir("C:/Users/Mainak Sarkar/Desktop/Edwisor/Project/Bike_Rental_Count")

#Importing the data skipping the blank lines if any
import pandas as pd
data = pd.read_csv("day.csv", skip_blank_lines = True)

#Checking the summary of the whole data set
data.describe(include = "all")

#Checking class of each column
data.dtypes





#***********************************DATA PREPROCESSING***********************************

#Data type conversion
#converting "season" as factor
data['season'] = data['season'].astype('category')

#converting "mnth" as factor
data['mnth'] = data['mnth'].astype('category')

#converting "holiday" as factor
data['holiday'] = data['holiday'].astype('category')

#converting "weekday" as factor
data['weekday'] = data['weekday'].astype('category')

#converting "workingday" as factor
data['workingday'] = data['workingday'].astype('category')

#Converting "dteday" as date-time format
data['dteday'] =  pd.to_datetime(data['dteday'], format='%Y-%m-%d')

#"yr" and "weathersit" is kept numerical
#The data set contains only 3 types of weathersit, but according to problem statement there are 4 types
#So we kept weathersit as numerical variable so that it can predict for instances of number 4
#We kept yr as numerical so that we can predict for future cases as well





#***********************************MISSING VALUE ANALYSIS***********************************

#Checking null values
missing_value = pd.DataFrame(data.isnull().sum())

#Resetting Index
missing_value = missing_value.reset_index()

#Renaming Variable
missing_value = missing_value.rename(columns = {'index':'Variable Name', 0 : 'Missing-Percentage'})

#Calculating Missing Value Percentage
missing_value['Missing-Percentage'] = (missing_value['Missing-Percentage']/len(data))*100

#So the dataset has no missing values
#The data set contains only 3 types of weathersit, but according to problem statement there are 4 types
#So we kept weathersit as numerical variable so that it can predict for instances of number 4
#We kept yr as numerical so that we can predict for future cases as well





#***********************************OUTLIER ANALYSIS***********************************

#Selecting numerical variables
numerical_cnames = ['temp', 'atemp', 'hum', 'windspeed']                       #"instant" and all dependent variables are removed
#passenger_count and fare_amount is dealt separately

import numpy as np

#Detecting Outliers and replacing them with NA's
for a in numerical_cnames:
    q75, q25 = np.percentile(data.loc[:,a], [75, 25])
    iqr = q75 - q25
    min = q25 - (iqr*1.5)
    max = q75 + (iqr*1.5)
    data.loc[data[a]<min, a] = np.nan
    data.loc[data[a]>max, a] = np.nan


#Checking null values
outliers = pd.DataFrame(data.isnull().sum())

#Resetting Index
outliers = outliers.reset_index()

#Renaming Variables
outliers = outliers.rename(columns = {'index':'Variable Name', 0 : 'Missing-Percentage'})

#Calculating Missing Value Percentage
outliers['Missing-Percentage'] = (outliers['Missing-Percentage']/len(data))*100

#As we know that presence of ouliers can affect our models a lot
#And the no. of observations in the dataset is small, so we can't delete the outliers
#Thus we have to opt for imputation





#***********************************ONLY FOR FINDING THE BEST METHOD***********************************

#Making a sample to check which method works best
#Choosing a sample and saving its value
sample_NA = data.iloc[50, 11:13]

#Putting values of sample equal to NA for required columns
data.iloc[50, 11:13] = np.nan


#MEAN Method
for b in numerical_cnames :
    data[b] = data[b].fillna(data[b].mean())
    
sample_NA_mean = data.iloc[50, 11:13]


#Re_Run the above part of code without the MEAN Method

#MEDIAN Method
for c in numerical_cnames :
    data[c] = data[c].fillna(data[c].median())

sample_NA_median = data.iloc[50, 11:13]   

#Comparing different imputing methods
sample = pd.concat([sample_NA, sample_NA_mean, sample_NA_median], axis = 1)

sample.columns = ['sample_NA', 'sample_NA_mean', 'sample_NA_median']
 
#Inserting a new blank row in "sample"
sample['Best Method'] = np.nan

#Finding the best method of imputation for each column
for d in range(sample.shape[0]):
    if  (abs(sample.iloc[d, 0]-sample.iloc[d, 1]) < abs(sample.iloc[d, 0]-sample.iloc[d, 2])):
        sample.iloc[d, 3] = "MEAN"
    else:
        sample.iloc[d, 3] = "MEDIAN"


#From "sample" dataframe we can find the best method for each column
        
 



#**************************************************************************************

#Imputing the best fit method for each column
#Re-Run the data till-"ONLY FOR FINDING THE BEST METHOD"
data['hum'] = data['hum'].fillna(data['hum'].median())   
data['windspeed'] = data['windspeed'].fillna(data['windspeed'].mean())





#***********************************DATA MANIPULATION***********************************

#Re-run the data upto line 54

#Deleting unnecessary variables
data = data.drop(["instant", "dteday"], axis = 1)

#Creating functions to change all normalised variable to their actual values
def Temp(t):
  t_max = 39
  t_min = -8
  temperature = t*(t_max - t_min)+t_min
  return(temperature)

def ATemp(at):
  at_max = 50
  at_min = -16
  a_temperature = at*(at_max - at_min)+at_min
  return(a_temperature)

def Hum(h):
    hum = h*100
    return(hum)

def WS(w):
    windspeed = w*67
    return(windspeed)

#Converting to actual values
#temp
data['temp'] = data[['temp']].apply(Temp, axis = 1)

#atemp
data['atemp'] = data[['atemp']].apply(ATemp, axis = 1)

#hum
data['hum'] = data[['hum']].apply(Hum, axis = 1)

#windspeed
data['windspeed'] = data[['windspeed']].apply(WS, axis = 1)


#The total count of users is a summation of casual and registered users
#checking if there is any rows other than that
num = 0
for i in range(len(data)):
    if data.iloc[i, 13] != (data.iloc[i, 12] + data.iloc[i, 11]) :
        num = num + 1

#As n = 0 so we can assure that there is no irrelevant rows





#***********************************EXPOLATORY DATA ANALYSIS***********************************

#Stacked barplots of season
#Mean of casual grouping by season
means_casual = pd.DataFrame(data['casual'].groupby(data['season']).mean())
means_casual['type'] = 'Casual'
means_casual['season'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'season']

#Mean of registered grouping by season
means_registered = pd.DataFrame(data['registered'].groupby(data['season']).mean())
means_registered['type'] = 'Registered'
means_registered['season'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'season']

#Mean of cnt grouping by season
means_cnt = pd.DataFrame(data['cnt'].groupby(data['season']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['season'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'season']

#Forming required dataframe for plotting
means_long_season = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'season', data = means_long_season)

#We can see that casual, registered and total count all are highest for season 3 and lowest for 1
#so when season is "fall" people are more likely to rent a bike




#Stacked barplots of year
#Mean of casual grouping by year
means_casual = pd.DataFrame(data['casual'].groupby(data['yr']).mean())
means_casual['type'] = 'Casual'
means_casual['yr'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'yr']

#Mean of registered grouping by year
means_registered = pd.DataFrame(data['registered'].groupby(data['yr']).mean())
means_registered['type'] = 'Registered'
means_registered['yr'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'yr']

#Mean of cnt grouping by year
means_cnt = pd.DataFrame(data['cnt'].groupby(data['yr']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['yr'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'yr']

#Forming required dataframe for plotting
means_long_year = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'yr', data = means_long_year)

#From the plots we can say that the count of registered users had heavily increased in 2012 compared to 2011
#And casual users also increased a bit, so the rental company is becoming popular
#So the rental will be higher for future years as well



#Stacked barplots of month
#Mean of casual grouping by month
means_casual = pd.DataFrame(data['casual'].groupby(data['mnth']).mean())
means_casual['type'] = 'Casual'
means_casual['mnth'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'mnth']

#Mean of registered grouping by month
means_registered = pd.DataFrame(data['registered'].groupby(data['mnth']).mean())
means_registered['type'] = 'Registered'
means_registered['mnth'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'mnth']

#Mean of cnt grouping by month
means_cnt = pd.DataFrame(data['cnt'].groupby(data['mnth']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['mnth'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'mnth']

#Forming required dataframe for plotting
means_long_month = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'mnth', data = means_long_month)

#we can see that the number of users is comparetively more in months 5 to 9




#Stacked barplots of weekday
#Mean of casual grouping by weekday
means_casual = pd.DataFrame(data['casual'].groupby(data['weekday']).mean())
means_casual['type'] = 'Casual'
means_casual['weekday'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'weekday']

#Mean of registered grouping by weekday
means_registered = pd.DataFrame(data['registered'].groupby(data['weekday']).mean())
means_registered['type'] = 'Registered'
means_registered['weekday'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'weekday']

#Mean of cnt grouping by weekday
means_cnt = pd.DataFrame(data['cnt'].groupby(data['weekday']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['weekday'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'weekday']

#Forming required dataframe for plotting
means_long_weekday = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'weekday', data = means_long_weekday)

#we can see that the number of casual users in more during weekends as casual users take bikes for trips
#Number of registered users is more during weekdays as they mainly consists of office goers




#Stacked barplots of holiday
#Mean of casual grouping by holiday
means_casual = pd.DataFrame(data['casual'].groupby(data['holiday']).mean())
means_casual['type'] = 'Casual'
means_casual['holiday'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'holiday']

#Mean of registered grouping by holiday
means_registered = pd.DataFrame(data['registered'].groupby(data['holiday']).mean())
means_registered['type'] = 'Registered'
means_registered['holiday'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'holiday']

#Mean of cnt grouping by holiday
means_cnt = pd.DataFrame(data['cnt'].groupby(data['holiday']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['holiday'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'holiday']

#Forming required dataframe for plotting
means_long_holiday = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'holiday', data = means_long_holiday)

#So if it is a holiday the number of casual users is more and registered users is less as compared to a non-holiday




#Stacked barplots of workingday
#Mean of casual grouping by workingday
means_casual = pd.DataFrame(data['casual'].groupby(data['workingday']).mean())
means_casual['type'] = 'Casual'
means_casual['workingday'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'workingday']

#Mean of registered grouping by workingday
means_registered = pd.DataFrame(data['registered'].groupby(data['workingday']).mean())
means_registered['type'] = 'Registered'
means_registered['workingday'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'workingday']

#Mean of cnt grouping by workingday
means_cnt = pd.DataFrame(data['cnt'].groupby(data['workingday']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['workingday'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'workingday']

#Forming required dataframe for plotting
means_long_workingday = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'workingday', data = means_long_workingday)

#Naturally if it is a working day the number of registered users is more as people goes to work renting a bike




#Stacked barplots of weathersit
#Mean of casual grouping by weathersit
means_casual = pd.DataFrame(data['casual'].groupby(data['weathersit']).mean())
means_casual['type'] = 'Casual'
means_casual['weathersit'] = means_casual.index
means_casual.columns = [ 'means', 'type', 'weathersit']

#Mean of registered grouping by weathersit
means_registered = pd.DataFrame(data['registered'].groupby(data['weathersit']).mean())
means_registered['type'] = 'Registered'
means_registered['weathersit'] = means_registered.index
means_registered.columns = [ 'means', 'type', 'weathersit']

#Mean of cnt grouping by weathersit
means_cnt = pd.DataFrame(data['cnt'].groupby(data['weathersit']).mean())
means_cnt['type'] = 'Cnt'
means_cnt['weathersit'] = means_cnt.index
means_cnt.columns = [ 'means', 'type', 'weathersit']

#Forming required dataframe for plotting
means_long_weathersit = pd.concat([means_casual, means_registered, means_cnt], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'weathersit', data = means_long_weathersit)

#So we can conclude that weather situation no. 1 is most preferable for bike renting
#And weather situation no. 4 will be least as the weather condition is detoriating from 1 to 4





#Scatter plot of Year and Different type of users
sns.regplot(x = 'yr', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'yr', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'yr', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#The regression line shows a positive relationship between year and count of users
#So we can say that the no. of rentals is progressively increasing




#Scatter plot of Weather Situation and Different type of users
sns.regplot(x = 'weathersit', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'weathersit', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'weathersit', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#The regression line shows a negative relationship between weather and number of users
#So as the weather value is increasing the weather condition is getting worse so number of rentals is decreasing
#So for weather condition 4 the count should be lesser




#Scatter plot of Temperature and Different type of users
sns.regplot(x = 'temp', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'temp', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'temp', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#From the we can see that the curve is steeper in case of registered users as they have to rent bike for offices, thus we can see atleast some good number of users even in case of extremes of temperature
#In case of casual users, if temperature is too low then number of users is low and if temperature is too high then number of casual users decreases, and mainly we can see most casual users during moderate temperatures




#Scatter plot of Feeled Temperature and Different type of users
sns.regplot(x = 'atemp', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'atemp', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'atemp', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#From the we can see that the curve is steeper in case of registered users as they have to rent bike for offices, thus we can see atleast some good number of users even in case of extremes of temperature
#In case of casual users, if temperature is too low then number of users is low and if temperature is too high then number of casual users decreases, and mainly we can see most casual users during moderate temperatures




#Scatter plot of Humidity and Different type of users
sns.regplot(x = 'hum', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'hum', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'hum', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#No. of users is very less dependent on humidity so change of humidity doesn't affect the rental count very much except in extreme cases
#Humidity is somehow negetively related to the number of users
#As humidity increases people are a bit less likely to rent a bike




#Scatter plot of Windspeed and Different type of users
sns.regplot(x = 'windspeed', y = 'casual', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})

sns.regplot(x = 'windspeed', y = 'registered', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'yellow'})

sns.regplot(x = 'windspeed', y = 'cnt', data = data, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'orange'})

#Wind Speed is also negatively related to the no. of users
#People are less likely to rent a bike if the wind speed is too high




#Histogram plot of Casual Users
sns.distplot(data['casual'])
#Plot is right skewed

#Histogram plot of logarithm of Casual Users
import numpy as np
sns.distplot(np.log(data['casual']))
#Log of casual users have a smooth, bell-shaped curve compared to that of casual users
#so we would use log of casual for further analysis




#Histogram plot of Registered Users
sns.distplot(data['registered'])

#Histogram plot of logarithm of Registered Users
sns.distplot(np.log(data['registered']))

#In this case taking logarithm makes the histogram a bit left skewed
#The original curve is much better to fit so we would retain that




#So taking the log of casual users for further calculations
data['log_casual'] = np.nan
data.iloc[:,14] = data.iloc[:,11].apply(np.log)




#***********************************FEATURE SELECTION***********************************

#Correlation analysis
import matplotlib.pyplot as plt
num_cnames = ['yr', 'weathersit', 'temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt', 'log_casual']
data_corr = data.loc[:, num_cnames]

#Set the height and width of the plot
f,ax = plt.subplots(figsize = (7,5))

#Generate correlation matrix
corr = data_corr.corr()

#plot using seaborn
import seaborn as sns
sns.heatmap(corr, mask = np.zeros_like(corr, dtype = np.bool),
            cmap = sns.diverging_palette(220, 10, as_cmap = True),
            square = True, ax = ax)

#From the correlation plot we can also see that variables "temp" and "atemp" are highly correlated
#So we need to remove one of them

#Checking correlation coefficient value with dependent variable
#temp
np.corrcoef(x = data['temp'], y = data['cnt'])

#atemp
np.corrcoef(x = data['atemp'], y = data['cnt'])

#Correlation between dependent variable and "atemp" (0.63) is slightly more than that with "temp" (0.62)
#So we would delete "temp" from our dataset

#Removing "temp" to avoid multicollinearity
data = data.drop(['temp'], axis = 1)



#From the correlation plot we can also see that variables "weathersit" and "hum" are moderately correlated
#So we need to remove one of them

#Checking correlation coefficient value with dependent variable
#weathersit
np.corrcoef(x = data['weathersit'], y = data['cnt'])

#hum
np.corrcoef(x = data['hum'], y = data['cnt'])
#Correlation between dependent variable and "weathersit" (-0.3) is more than that with "hum" (-0.12)
#So we would delete "hum" from our dataset

#Removing "hum" to avoid multicollinearity
data = data.drop(['hum'], axis = 1)

#Other than these all the dependent variables are higly correlated which is completely normal




#Now to check the correlation between the categorical variables we would go for Chi_sq test

#From the EDA we have expected that there can be a correlation between "season" and "mnth" variables
#So checking Chi-sq value between "season" and "mnth"
from scipy.stats import chi2_contingency
chi2_1,p_1,dof_1,ex_1 = chi2_contingency(pd.crosstab(data['season'], data['mnth']))
print(p_1)

#As p_value is less than 0.05, so collinearity exists between the two variables
#So we will go for anova test later to check the better predictor and the other will be omitted



#Again from the EDA we have seen that "holiday" and "workingday" variables almost gives us the same information in alternate ways
#So checking Chi-sq value between "holiday" and "workingday"
chi2_2,p_2,dof_2,ex_2 = chi2_contingency(pd.crosstab(data['holiday'], data['workingday']))
print(p_2)

#As p_value is less than 0.05, so collinearity exists between the two variables
#So we will check the better predictor and the other will be omitted





#Now to check which is a better dependent and independent variables we will go for ANOVA test

#ANOVA(Analysis of Variance) Test
import statsmodels.api as sm
from statsmodels.formula.api import ols

#ANOVA for season
data.boxplot('cnt', by = 'season')
cnt_season_ols = ols('cnt ~ season', data = data).fit()
sm.stats.anova_lm(cnt_season_ols, type = 1)

#ANOVA for month
data.boxplot('cnt', by = 'mnth')
cnt_month_ols = ols('cnt ~ mnth', data = data).fit()
sm.stats.anova_lm(cnt_month_ols, type = 1)

#So we can see that both "season" and "month" have p_value less than 0.05
#So both of them are significant predictors
#But we know because of multicollinearity we need to delete one of them
#So looking at the F_value we can see that F_value is more for "season" (129) than that of "mnth" (42)
#So "season" explains more variance of the dependent variable
#Thus we will delete "mnth" from our analysis

data = data.drop(['mnth'], axis=1)

#ANOVA for weekday
data.boxplot('cnt', by = 'weekday')
cnt_weekday_ols = ols('cnt ~ weekday', data = data).fit()
sm.stats.anova_lm(cnt_weekday_ols, type = 1)

#ANOVA for holiday
data.boxplot('cnt', by = 'holiday')
cnt_holiday_ols = ols('cnt ~ holiday', data = data).fit()
sm.stats.anova_lm(cnt_holiday_ols, type = 1)

#ANOVA for workingday
data.boxplot('cnt', by = 'workingday')
cnt_workingday_ols = ols('cnt ~ workingday', data = data).fit()
sm.stats.anova_lm(cnt_workingday_ols, type = 1)

#So we have seen that that p_values of "weekday", "holiday", "workingday" is greater than 0.05
#so these variables doesnot influence the total count of users
#But there is a chance that they have some effect on casual and registered users
#As we have checked from the bar plots earlier that count of casual and registered users changes on these variables
#So we will do anova tests on these variables using casual and registered user as dependent variable

#CASUAL users as dependent variable
#ANOVA for weekday
data.boxplot('casual', by = 'weekday')
casual_weekday_ols = ols('casual ~ weekday', data = data).fit()
sm.stats.anova_lm(casual_weekday_ols, type = 1)

#ANOVA for holiday
data.boxplot('casual', by = 'holiday')
casual_holiday_ols = ols('casual ~ holiday', data = data).fit()
sm.stats.anova_lm(casual_holiday_ols, type = 1)

#ANOVA for workingday
data.boxplot('casual', by = 'workingday')
casual_workingday_ols = ols('casual ~ workingday', data = data).fit()
sm.stats.anova_lm(casual_workingday_ols, type = 1)

#Only "holiday" has p_value greater than 0.05



#REGISTERED users as dependent variable
#ANOVA for weekday
data.boxplot('registered', by = 'weekday')
registered_weekday_ols = ols('registered ~ weekday', data = data).fit()
sm.stats.anova_lm(registered_weekday_ols, type = 1)

#ANOVA for holiday
data.boxplot('registered', by = 'holiday')
registered_holiday_ols = ols('registered ~ holiday', data = data).fit()
sm.stats.anova_lm(registered_holiday_ols, type = 1)

#ANOVA for workingday
data.boxplot('registered', by = 'workingday')
registered_workingday_ols = ols('registered ~ workingday', data = data).fit()
sm.stats.anova_lm(registered_workingday_ols, type = 1)

#All the 3 variables have p_value less than 0.05, so they are significant predictors


#Moreover we can see that the few variables are not correlated to the total count "cnt" variable
#But are correlated to "casual" and "registered" users
#So we would predict casual and registered users individually and then sum them up to find the total count of users





#***********************************FEATURE SCALING***********************************

#Checking the distribution of the numerical variables for choosing the scaling method
#atemp
sns.distplot(data['atemp'])
#Histogram doesn't show a normal distribution

#windspeed
sns.distplot(data['windspeed'])
#The histogram is very close to normal distribution

#Normalization would be applied to scale as the all the variables are not normally distributed



#year
#Normalization is not required in case of year as there are only two values 0 and 1
#If there are more than two values then normalization is required


#weathersit
#weathersit contains 4 conditions
#max = 4, min = 1
data['weathersit'] = (data['weathersit'] - 1) / (4 - 1)


#atemp
data['atemp'] = (data['atemp'] - (- 16)) / (50 - (- 16))


#windspeed
#Normalization is done by dividing by 67(max)
data['windspeed'] = data['windspeed'] / 67





#***********************************TRAIN-TEST SPLIT***********************************
#Checking the number of values in each factor variable
data['season'].value_counts()                   #almost equal in each groups
data['yr'].value_counts()                       #almost equal across groups
data['mnth'].value_counts()                     #almost equally distributed
data['holiday'].value_counts()                  #Highly biased
data['weekday'].value_counts()                  #almost equal across groups
data['workingday'].value_counts()               #slightly biased
data['weathersit'].value_counts()               #Highly biased

#So as we can see that the dataset is baised across some categories so if we apply random sampling in this case
#Then there might be a chance that no observations of the low count groups is included
#So we need to apply stratified spliting in this case taking the most correlated variable as reference variable

#From the correlation plot we can see that weathersit would be the best variable to create the strata as it is highly biased and good correlation with dependent variable
np.random.seed(555)

from sklearn.model_selection import train_test_split
#Categorical variable to be set as an array
y = np.array(data['weathersit'])
training_set,test_set = train_test_split(data, test_size = 0.15, stratify = y)

#As the dataset is very small so we have only taken a small part of our data as test set





#***********************************MODEL BUILDING***********************************

#Creating Error metrics calculation function:
def MAPE_RMSE(y_true, y_pred):
    mape = np.mean(np.abs((y_true - y_pred) / y_true))*100
    from sklearn.metrics import mean_squared_error
    from math import sqrt
    rmse = sqrt(mean_squared_error(y_true, y_pred))
    return [mape, rmse]

#VIF(Variance Inflation factor) Test
vif = pd.DataFrame()

from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tools.tools import add_constant

X = add_constant(training_set.iloc[:, 0:8])
vif["Variables"] = X.columns
vif["VIF_value"] = [variance_inflation_factor(np.array(X.values,dtype = float), k) for k in range(0,9)]

#We will predict separately for casual and registered users


#*********************A)CASUAL USERS



#So taking logarithm of casual users as dependent variable

#As we know that there is multicollinearity between "holiday" and "workingday"
#So by trial and error method we saw eliminating "workingday" gives better accuracy
#Deleting "workingday" from the data

#Removing other dependent variables from training_set
train_casual = training_set.drop(['workingday', 'casual', 'registered', 'cnt'], axis = 1)
test_casual = test_set.drop(['workingday', 'casual', 'registered', 'cnt'], axis = 1)

#Linear Regression
from sklearn import linear_model as lm
model = lm.LinearRegression()
LR_casual = model.fit(train_casual.iloc[:, 0:7], train_casual.iloc[:,7])
predictions_LR_casual = LR_casual.predict(test_casual.iloc[:, 0:7])

#Calculating different Error Metrics
LR_casual_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 8], np.exp((predictions_LR_casual).astype(float))))



#Decision Tree Regressor
from sklearn.tree import DecisionTreeRegressor
DT_casual = DecisionTreeRegressor(random_state = 0)
fit_DT_casual = DT_casual.fit(X = train_casual.iloc[:, 0:7], y = train_casual.iloc[:, 7])
predictions_DT_casual = fit_DT_casual.predict(test_casual.iloc[:, 0:7])

#Calculating different Error Metrics
DT_casual_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 8], np.exp((predictions_DT_casual).astype(float))))



#Random Forest Regression
from sklearn.ensemble import RandomForestRegressor
RF_casual = RandomForestRegressor(n_estimators = 200)
fit_RF_casual = RF_casual.fit(train_casual.iloc[:, 0:7], train_casual.iloc[:, 7])
predictions_RF_casual = fit_RF_casual.predict(test_casual.iloc[:, 0:7])

#Calculating different Error Metrics
RF_casual_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 8], np.exp((predictions_RF_casual).astype(float))))



#Support Vector Regression(SVR)
from sklearn.svm import SVR
SVR_casual = SVR(kernel = 'rbf').fit(train_casual.iloc[:, 0:7], train_casual.iloc[:, 7])
predictions_SVR_casual = SVR_casual.predict(test_casual.iloc[:, 0:7])

#Calculating different Error Metrics
SVR_casual_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 8], np.exp((predictions_SVR_casual).astype(float))))



#K Nearest Neighbors(KNN)
from sklearn.neighbors import KNeighborsRegressor

#Finding the optimum value of K
k_value_casual = []

for l in range(1,10):
    l = l+1
    model_casual = KNeighborsRegressor(n_neighbors = l).fit(train_casual.iloc[:, 0:7], train_casual.iloc[:, 7])
    predicitons_casual = model_casual.predict(test_casual.iloc[:, 0:7])
    from sklearn.metrics import mean_squared_error
    from math import sqrt
    error_casual = sqrt(mean_squared_error(test_set.iloc[:, 8], np.exp((predicitons_casual).astype(float))))
    k_value_casual.append(error_casual)

#Elbow Curve
elbow_casual = pd.DataFrame(k_value_casual)
elbow_casual.plot()

#So from the graph we can see that the elbow is at k = 3, but here we want lower rmse values for our model
#So we will choose k=3 as a fair value

#Building the model with k = 3
KNN_casual = KNeighborsRegressor(n_neighbors = 3).fit(train_casual.iloc[:, 0:7], train_casual.iloc[:, 7])
predictions_KNN_casual = KNN_casual.predict(test_casual.iloc[:, 0:7])

#Calculating different Error Metrics
KNN_casual_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 8], np.exp((predictions_KNN_casual).astype(float))))



#To compare all models
Error_Metrics_casual_EM = pd.concat([LR_casual_EM, DT_casual_EM, RF_casual_EM, SVR_casual_EM, KNN_casual_EM], axis = 1)
Error_Metrics_casual_EM.columns = [ 'LR', 'DT', 'RF', 'SVR', 'KNN']
Error_Metrics_casual_EM.rename(index = {0:'mape',1:'rmse'}, inplace = True)

#By compairing the error metrics we can see that KNN gives the best accuracy and less error








#*********************B)REGISTERED USERS



#So taking registered users as dependent variable

#As we know that there is multicollinearity between "holiday" and "workingday"
#So by trial and error method we saw eliminating "workingday" gives better accuracy
#Deleting "workingday" from the data

#Removing other dependent variables from training_set
train_reg = training_set.drop(['workingday', 'casual', 'log_casual', 'cnt'], axis = 1)
test_reg = test_set.drop(['workingday', 'casual', 'log_casual', 'cnt'], axis = 1)



#Linear Regression
import statsmodels.api as sm
LR_reg = sm.OLS(train_reg.iloc[:, 7], train_reg.iloc[:, 0:7].astype(float)).fit()
predictions_LR_reg = LR_reg.predict(test_reg.iloc[:, 0:7])

#Calculating different Error Metrics
LR_reg_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 9], predictions_LR_reg))



#Decision Tree Regressor
from sklearn.tree import DecisionTreeRegressor
DT_reg = DecisionTreeRegressor(random_state = 0)
fit_DT_reg = DT_reg.fit(X = train_reg.iloc[:, 0:7], y = train_reg.iloc[:, 7])
predictions_DT_reg = fit_DT_reg.predict(test_reg.iloc[:, 0:7])

#Calculating different Error Metrics
DT_reg_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 9], predictions_DT_reg))



#Random Forest Regression
from sklearn.ensemble import RandomForestRegressor
RF_reg = RandomForestRegressor(n_estimators = 500)
fit_RF_reg = RF_reg.fit(train_reg.iloc[:, 0:7], train_reg.iloc[:, 7])
predictions_RF_reg = fit_RF_reg.predict(test_reg.iloc[:, 0:7])

#Calculating different Error Metrics
RF_reg_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 9], predictions_RF_reg))
RF_reg_EM


#Support Vector Regression(SVR)
from sklearn.svm import SVR
SVR_reg = SVR(kernel = 'rbf').fit(train_reg.iloc[:, 0:7], train_reg.iloc[:, 7])
predictions_SVR_reg = SVR_reg.predict(test_reg.iloc[:, 0:7])

#Calculating different Error Metrics
SVR_reg_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 9], predictions_SVR_reg))



#K Nearest Neighbors(KNN)
from sklearn.neighbors import KNeighborsRegressor

#Finding the optimum value of K
k_value_reg = []

for l in range(1,10):
    l = l+1
    model_reg = KNeighborsRegressor(n_neighbors = l).fit(train_reg.iloc[:, 0:7], train_reg.iloc[:, 7])
    predicitons_reg = model_reg.predict(test_reg.iloc[:, 0:7])
    from sklearn.metrics import mean_squared_error
    from math import sqrt
    error_reg = sqrt(mean_squared_error(test_set.iloc[:, 9], predicitons_reg))
    k_value_reg.append(error_reg)

#Elbow Curve
elbow_reg = pd.DataFrame(k_value_reg)
elbow_reg.plot()

#So from the graph we can see that the elbow is at k = 4, but here we want lower rmse values for our model
#So we will choose k=4 as a fair value

#Building the model with k = 4
KNN_reg = KNeighborsRegressor(n_neighbors = 4).fit(train_reg.iloc[:, 0:7], train_reg.iloc[:, 7])
predictions_KNN_reg = KNN_reg.predict(test_reg.iloc[:, 0:7])

#Calculating different Error Metrics
KNN_reg_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 9], predictions_KNN_reg))



#To compare all models
Error_Metrics_reg_EM = pd.concat([LR_reg_EM, DT_reg_EM, RF_reg_EM, SVR_reg_EM, KNN_reg_EM], axis = 1)
Error_Metrics_reg_EM.columns = [ 'LR', 'DT', 'RF', 'SVR', 'KNN']
Error_Metrics_reg_EM.rename(index = {0:'mape',1:'rmse'}, inplace = True)

#By compairing the error metrics we can see that Random Forest gives the best accuracy and less error





#So now we will add the casual and registered count and round it off to find the total count and compare it with the original data
predictions_cnt = (predictions_RF_reg + np.exp(predictions_KNN_casual)).round()

cnt_EM = pd.DataFrame(MAPE_RMSE(test_set.iloc[:, 10], predictions_cnt))
cnt_EM.rename(index = {0:'mape',1:'rmse'}, inplace = True)

#We got a fair result





#*************************************************************************


#TEST DATA


#Importing the data
test = pd.read_csv("Input_sample.csv")





#***********************************DATA TYPE CONVERSION***********************************

#converting "season" as factor
test['season'] = test['season'].astype('category')

#converting "holiday" as factor
test['holiday'] = test['holiday'].astype('category')

#converting "weekday" as factor
test['weekday'] = test['weekday'].astype('category')





#***********************************FEATURE SCALING***********************************

#weathersit
#weathersit contains 4 conditions
#max = 4, min = 1
test['weathersit'] = (test['weathersit'] - 1) / (4 - 1)


#atemp
test['atemp'] = (test['atemp'] - (- 16)) / (50 - (- 16))


#windspeed
#Normalization is done by dividing by 67(max)
test['windspeed'] = test['windspeed'] / 67





#***********************************MODEL TUNING***********************************

from sklearn.model_selection import GridSearchCV

#************************CASUAL
from sklearn.neighbors import KNeighborsRegressor
Best_KNN_casual = KNeighborsRegressor(n_neighbors = 3).fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['log_casual'])

parameters_casual = [{'algorithm' : ['auto'],
                      'p' : [1,2],
                      'n_neighbors' : [1,2,3,4,5,6,7,8,9,10]}]

grid_casual = GridSearchCV(estimator = Best_KNN_casual,
                           param_grid = parameters_casual,
                           cv = 10,
                           n_jobs = - 1)

grid_casual = grid_casual.fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['log_casual'])

best_parameter_casual = grid_casual.best_params_

#Training the model with best parameters
Best_casual = grid_casual.best_estimator_.fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['log_casual'])





#************************REGISTERED
from sklearn.ensemble import RandomForestRegressor
Best_RF_reg = RandomForestRegressor(n_estimators = 500).fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['registered'])

parameters_reg = [{'bootstrap' : ['True', 'False'],
                      'oob_score' : ['True', 'False'],
                      'warm_start' : ['True', 'False'],
                      'n_estimators' : [80,90,100,110,120]}]

grid_reg = GridSearchCV(estimator = Best_RF_reg,
                           param_grid = parameters_reg,
                           cv = 10,
                           n_jobs = - 1)

grid_reg = grid_reg.fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['registered'])

best_parameter_reg = grid_reg.best_params_

#Training the model with best parameters
Best_reg = grid_reg.best_estimator_.fit(data[['season', 'yr', 'holiday', 'weekday', 'weathersit', 'atemp', 'windspeed']], data['registered'])





#***********************************PREDICTION OF TEST CASES***********************************

casual = pd.DataFrame(np.round(np.exp(Best_casual.predict(test))))

registered = pd.DataFrame(np.round(Best_reg.predict(test)))

#Total output summing casual and registered counts
cnt = pd.DataFrame(casual + registered)


output = pd.concat([casual, registered, cnt], axis = 1)
output.columns = [ 'casual', 'registered', 'cnt']



#Saving as csv
output.to_csv(r'C:/Users/Mainak Sarkar/Desktop/Edwisor/Project/Bike_Rental_Count/output_sample_Python.csv')

input_output = pd.concat([test, output], axis = 1)

output.to_csv(r'C:/Users/Mainak Sarkar/Desktop/Edwisor/Project/Bike_Rental_Count/input_output_sample_Python.csv')





#***********************************TEST CASES VALIDATION***********************************


#Stacked barplots of season
#Mean of casual grouping by season
means_casual_io = pd.DataFrame(input_output['casual'].groupby(input_output['season']).mean())
means_casual_io['type'] = 'Casual'
means_casual_io['season'] = means_casual_io.index
means_casual_io.columns = [ 'means', 'type', 'season']

#Mean of registered grouping by season
means_registered_io = pd.DataFrame(input_output['registered'].groupby(input_output['season']).mean())
means_registered_io['type'] = 'Registered'
means_registered_io['season'] = means_registered_io.index
means_registered_io.columns = [ 'means', 'type', 'season']

#Mean of cnt grouping by season
means_cnt_io = pd.DataFrame(input_output['cnt'].groupby(input_output['season']).mean())
means_cnt_io['type'] = 'Cnt'
means_cnt_io['season'] = means_cnt_io.index
means_cnt_io.columns = [ 'means', 'type', 'season']

#Forming required dataframe for plotting
means_long_season_io = pd.concat([means_casual_io, means_registered_io, means_cnt_io], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'season', data = means_long_season_io)





#Stacked barplots of year
#Mean of casual grouping by year
means_casual_io = pd.DataFrame(input_output['casual'].groupby(input_output['yr']).mean())
means_casual_io['type'] = 'Casual'
means_casual_io['yr'] = means_casual_io.index
means_casual_io.columns = [ 'means', 'type', 'yr']

#Mean of registered grouping by year
means_registered_io = pd.DataFrame(input_output['registered'].groupby(input_output['yr']).mean())
means_registered_io['type'] = 'Registered'
means_registered_io['yr'] = means_registered_io.index
means_registered_io.columns = [ 'means', 'type', 'yr']

#Mean of cnt grouping by year
means_cnt_io = pd.DataFrame(input_output['cnt'].groupby(input_output['yr']).mean())
means_cnt_io['type'] = 'Cnt'
means_cnt_io['yr'] = means_cnt_io.index
means_cnt_io.columns = [ 'means', 'type', 'yr']

#Forming required dataframe for plotting
means_long_year_io = pd.concat([means_casual_io, means_registered_io, means_cnt_io], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'yr', data = means_long_year_io)





#Stacked barplots of weekday
#Mean of casual grouping by weekday
means_casual_io = pd.DataFrame(input_output['casual'].groupby(input_output['weekday']).mean())
means_casual_io['type'] = 'Casual'
means_casual_io['weekday'] = means_casual_io.index
means_casual_io.columns = [ 'means', 'type', 'weekday']

#Mean of registered grouping by weekday
means_registered_io = pd.DataFrame(input_output['registered'].groupby(input_output['weekday']).mean())
means_registered_io['type'] = 'Registered'
means_registered_io['weekday'] = means_registered_io.index
means_registered_io.columns = [ 'means', 'type', 'weekday']

#Mean of cnt grouping by weekday
means_cnt_io = pd.DataFrame(input_output['cnt'].groupby(input_output['weekday']).mean())
means_cnt_io['type'] = 'Cnt'
means_cnt_io['weekday'] = means_cnt_io.index
means_cnt_io.columns = [ 'means', 'type', 'weekday']

#Forming required dataframe for plotting
means_long_weekday_io = pd.concat([means_casual_io, means_registered_io, means_cnt_io], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'weekday', data = means_long_weekday_io)





#Stacked barplots of holiday
#Mean of casual grouping by holiday
means_casual_io = pd.DataFrame(input_output['casual'].groupby(input_output['holiday']).mean())
means_casual_io['type'] = 'Casual'
means_casual_io['holiday'] = means_casual_io.index
means_casual_io.columns = [ 'means', 'type', 'holiday']

#Mean of registered grouping by holiday
means_registered_io = pd.DataFrame(input_output['registered'].groupby(input_output['holiday']).mean())
means_registered_io['type'] = 'Registered'
means_registered_io['holiday'] = means_registered_io.index
means_registered_io.columns = [ 'means', 'type', 'holiday']

#Mean of cnt grouping by holiday
means_cnt_io = pd.DataFrame(input_output['cnt'].groupby(input_output['holiday']).mean())
means_cnt_io['type'] = 'Cnt'
means_cnt_io['holiday'] = means_cnt_io.index
means_cnt_io.columns = [ 'means', 'type', 'holiday']

#Forming required dataframe for plotting
means_long_holiday_io = pd.concat([means_casual_io, means_registered_io, means_cnt_io], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'holiday', data = means_long_holiday_io)





#Stacked barplots of weathersit
#Mean of casual grouping by weathersit
means_casual_io = pd.DataFrame(input_output['casual'].groupby(input_output['weathersit']).mean())
means_casual_io['type'] = 'Casual'
means_casual_io['weathersit'] = means_casual_io.index
means_casual_io.columns = [ 'means', 'type', 'weathersit']

#Mean of registered grouping by weathersit
means_registered_io = pd.DataFrame(input_output['registered'].groupby(input_output['weathersit']).mean())
means_registered_io['type'] = 'Registered'
means_registered_io['weathersit'] = means_registered_io.index
means_registered_io.columns = [ 'means', 'type', 'weathersit']

#Mean of cnt grouping by weathersit
means_cnt_io = pd.DataFrame(input_output['cnt'].groupby(input_output['weathersit']).mean())
means_cnt_io['type'] = 'Cnt'
means_cnt_io['weathersit'] = means_cnt_io.index
means_cnt_io.columns = [ 'means', 'type', 'weathersit']

#Forming required dataframe for plotting
means_long_weathersit_io = pd.concat([means_casual_io, means_registered_io, means_cnt_io], axis = 0)


import seaborn as sns
sns.barplot(x = 'type', y = 'means', hue = 'weathersit', data = means_long_weathersit_io)





#Scatter plot of Year and Total Count of users
sns.regplot(x = 'yr', y = 'cnt', data = input_output, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})



#Scatter plot of Weather Situation and Total Count of users
sns.regplot(x = 'weathersit', y = 'cnt', data = input_output, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})



#Scatter plot of Feeled Temperature and Different type of users
sns.regplot(x = 'atemp', y = 'cnt', data = input_output, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})



#Scatter plot of Windspeed and Different type of users
sns.regplot(x = 'windspeed', y = 'cnt', data = input_output, scatter_kws = {"color" : 'black'}, line_kws = {"color" : 'red'})




