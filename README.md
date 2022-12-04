# BikeRentalPrediction
A practice assignment where I develop a model to predict demand for rental bikes

A.	Data Exploration

My first step to build a model explaining the relationship between the independent variables and the dependent variable was to examine each variable individually. After examining each variable I used that information to construct a model describing their effect on the dependent variable. I started with the timestamp variable and found it to describe the date and hour for each observation. I built a table with unique values and their number of occurrences to confirm there weren’t any duplicate values present in the data. I then created three new variables from the timestamp variable called Day, Hour and Year. This allowed me to build tables showing which days and hours were present in the data and which were missing. These variables were also very useful when creating charts to summarize the data. I found the data included most of the days and hours of the time period covered by the dataset but key dates were missing such as the Christmas holiday. I next investigated the season variable included in the data. I found the dates were not typical of what I associate with the season. In order to investigate further I built a table showing the min, max and average temp and temp_feel for each season. Since winter was not the coldest season and summer was not the warmest I concluded the season variable would need to be changed in order to be useful. I ended up discarding the variable and using only the temp_feel variable to account for seasonality. I decided the actual temperature for a given day is likely to be more important than what season it belonged to. I next built a table to identify which days were marked as holidays by the holiday variable. I found a number of problems with the holiday variable. I found different holidays were recognized for 2017 as compared to 2018. I also found some of the dates did not appear to mark the correct day for important holidays while others occurred on the weekend rather than the day the holiday was observed. As a result I concluded the holiday variable wasn’t going to be very helpful. I considered the possibility of using an outside data source to correct the holiday variable or the possibility that it represents holidays used by the location that rents the bikes. At one point I added it to my model as a dummy variable but it was not statistically significant so I dropped the variable. 
I investigated the working day variable next. I built a table showing the number of days identified as a working day broken out by the day of the week and whether or not the day was flagged as a holiday by the holiday variable. I used the R package lubridate in order to create another column identifying during which day of the week each observation occurred. Except for Monday, I found all weekdays were not identified as a working day when flagged as a holiday but were considered to be a working days when not a holiday. Saturday was sometimes a working day even when flagged as a holiday. Sunday was never a working day and no holidays took place on Sunday. Given the variable did not label all weekdays as working days except when flagged as a holiday, I wasn’t confident in the accuracy of the variable. I later found the working day variable to be the key to explaining the behavior shown by days of the week explained below. 
I then investigated the temp and the temp_feel variables. I first plotted both variables against demand, but I did not find the plots to be very useful since time of day affects the temperature of a given hour. As a result I calculated the average temp and temp_feel for each day and then plotted them against demand. This revealed an obvious linear relationship between the temperature and the demand for bike rentals: the higher the temperature the greater the demand for bikes. I chose to use the temp_feel variable in my model since it had less missing values which meant I did not have to remove as many observations from the dataset. I chose to remove observations missing values since they made up a small share of the dataset. Temp_feel also had a slightly higher correlation with demand. I found humidity and windspeed have the same problem since they vary greatly by time of day. When plotted against the hour of the day, I found humidity to form an S-shaped curve. Average humidity was greatest at 5:00 AM and lowest at 3:00 PM. Windspeed shared a similar relationship with the time of day. Humidity appeared to be negatively correlated with demand (-.33) but most of the correlation disappeared once I aggregated humidity (it decreased to -.07 correlation). Windspeed first appeared to show a positive correlation with demand (.119) but once I aggregated windspeed by day it showed a negative correlation (-.248). I concluded it shares a negative correlation with demand. It only appeared to have a positive correlation since it is correlated with the time of day, a form of omitted variable bias. 
After examining each of the independent variables, I then investigated demand, the dependent variable. I first plotted average demand for each day across the full time period covered by the dataset. This revealed a clear cyclical pattern where demand would increase during the spring as temperatures rise until it reaches a peak at the start of July. It would then decrease until it reaches its’ lowest value in January. It also became obvious there is a positive trend such that, holding everything else constant, demand increases over time. What made this obvious is that 2018 shows similar cyclical patterns as 2017 but demand was greater on average in 2018 than in 2017. There also appeared to be a weekly cyclical as average daily demand rose and fell in repeating patterns within seasons. I then plotted average demand across each day of the week from Monday through Sunday. To my surprise there appeared to be a different pattern in 2017 as compared to 2018. Thursday appeared to have the lowest average demand in 2017 while Monday had the lowest average demand for 2018. Both years showed higher demand on Saturday and Sunday which was not a surprise. 2018 showed greater demand than 2017, once again suggesting a positive trend for demand. I tried making changes to the graphs such as removing outliers and changing the dates covered but this did not solve the problem. I did not identify the cause of this difference until I broke out average demand by the workingday variable instead of the day of week. I next plotted average demand versus hour of the day. This showed a parabolic shape (upside-down U) with a dip during lunch time. This made it obvious to me that the equation would need to model a quadratic relationship between time of day and demand. I made a key insight when I broke out demand by the day of the week and created separate graphs for 2017 and 2018. I found most weekdays had a strictly parabolic shape (upside-down U) while for some days demand cratered between 10 AM and 3 PM, resulting in there being a parabolic shape in the other direction (right-side up U) between 8 AM and 5 PM. Breaking the results out by the workingday variable showed days that were labeled a workday showed a crater around lunch time while other days showed a parabolic shape. These insights greatly informed my time series model I used to describe the data.

B.	Model Creation

From my data exploration, I knew it was most important to accurately account for the relationship between time of day and demand. This was important since it was also correlated with several other variables. There is also great variation in demand based on the time of day. I knew the model needed to be nonlinear since there was a parabolic relationship between demand and time of day. I also knew there was a different relationship between the time of day and demand based on whether or not it was a workday. Finally, I needed to account for a positive trend for demand over time. The other variables, temp_feel, humidity and windspeed showed a linear relationship with demand. As a result I decided to create a series of splines to describe the relationship between time of day and demand. If it was not a workday the model would simply be a parabola with a peak around lunchtime. In the case of workdays there would be a curve showing rising demand from 4 AM to 8 AM. There would be another curve in the shape of a parabola (upside-down U) from 8 AM to 12 PM. There would be a similar curve showing an increase in demand from 1 PM to 5 PM. Finally the model would return to the original parabola shape from 6 PM to 4 AM. This required that I create a new hour variable. Instead of 0 corresponding to 12:00 AM and 23 to 11:00 PM, 0 would correspond to 4 AM and 23 would represent 3 AM. I also needed to create a categorical variable indicating whether it was morning (8 AM to 12 PM) or afternoon (1 PM to 5 PM). Both these variables being zero would indicate the default time period of 6 PM to 7 AM. I knew I needed to use the workday variable as a categorical variable. Finally I created a variable counting the number of days since the start of the dataset to account for demand’s positive trend. As a result I developed the following nonlinear time series model:

demand~DayIndex + DayofWeek + Rain + Mist + temp_feel + humidity + windspeed + (HourB*workingdayB) + (HourB2*workingdayB) + ((Morning-8)*workingdayB) + ((Morning2-9)*workingdayB) + ((Afternoon-13)*workingdayB) + ((Afternoon2-13)*workingdayB)

DayIndex: counts the number of days since January 1st 2017
DayofWeek: Categorical variable for each day of the week
Rain: Categorical variable indicating whether it rained (1 or 0)
Mist: Categorical variable indicating whether there was mist (1 or 0)
temp_feel: Continuous variable indicating what the temperature felt like
humidity: Continuous variable measuring humidity
windspeed: Continuous variable measuring windspeed
HourB: Integer value ranging  from 0 (4 AM) to 23 (3 AM)
workingdayB: Categorical variable indicating whether or not it was a work day (1 or 0)
Morning: Categorical variable ranging from 1 (8 AM) to 5 (12 PM)
Afternoon: Categorical variable ranging from 1 (1 PM) to 5 (5 PM)
 
  ![image](https://user-images.githubusercontent.com/103590679/205515529-3040f9b9-d0f1-46e8-b736-a6b74849147e.png)

C.	Model Diagnostics

Given my exploratory analysis and the output from my model I am confident I captured the key relationships between the independent variables and the dependent variables. This is shown by the high R-squared value of 0.8567, indicating the model captures most of the variability in the dependent variable. I calculated an RMSE of 0.566, which may be more meaningful for a nonlinear model. The strength of the model is supported by the low p-value generated by the F-test (testing whether the model is a better predictor of the dependent variable than no model at all) and the fact that all of the variables are statistically significant except for the categorical variable indicating whether it is a Sunday. To confirm that my model fits the data I generated a QQ-Plot, plotted fitted values against residuals and plotted the demand variable against the predicted values. The QQ-Plot suggested the residuals may not be normally distributed, but this is typical for a nonlinear model. The second chart shows the distribution of residuals follows the distribution of fitted values. The last chart shows the distribution of fitted values follows the demand variable. These both suggest the model fits the data. I created another chart showing average demand and the average predicted value by day across the full time period and another version covering the first month of the dataset. Finally I produced a chart showing demand and predicted demand by hour for the first four weeks of the dataset. These suggest the model fits the data.

![image](https://user-images.githubusercontent.com/103590679/205515591-e154aa2b-3c44-46df-87fd-933fcb7dfc7d.png)

![image](https://user-images.githubusercontent.com/103590679/205515595-6567dc6f-23ba-4359-8cd2-0b13c29bc0ac.png)

![image](https://user-images.githubusercontent.com/103590679/205515601-f07821a8-313c-4638-8f3a-9ede48d55a3b.png)

![image](https://user-images.githubusercontent.com/103590679/205515606-5b3d108e-ff7c-462a-a386-8ad2481ab244.png)

High Curves= 2018, Low Curves=2017, Green=Prediction, Red=demand

![image](https://user-images.githubusercontent.com/103590679/205515619-e5a0342d-c970-40f3-a923-41404622d87e.png)

January 2017, Green=Prediction, Red=demand

![image](https://user-images.githubusercontent.com/103590679/205515627-885a9671-4c1c-4734-84b4-610df0357c2b.png)

2017-01-01 thru 2017-01-04, Green=Prediction, Red=demand

Part 2: Tables and Charts

Season Table:

![image](https://user-images.githubusercontent.com/103590679/205515827-1d8722a6-8d1d-43c8-9e1a-a33ed189d4dd.png)

Holiday Table:

![image](https://user-images.githubusercontent.com/103590679/205515832-712bc3b4-74b7-4dba-b50e-7a516021642b.png)

WorkingDay Table:

![image](https://user-images.githubusercontent.com/103590679/205515845-0c9ce21e-c936-4a9e-872c-52b7245fc9ff.png)

![image](https://user-images.githubusercontent.com/103590679/205515856-3dc810e0-c969-4910-8c45-dc3f1709d432.png)

![image](https://user-images.githubusercontent.com/103590679/205515863-e3dd936e-64fd-4204-964e-219b9f811480.png)

![image](https://user-images.githubusercontent.com/103590679/205515867-1735c3f8-f35a-4e9b-8299-a65552bc8773.png)

![image](https://user-images.githubusercontent.com/103590679/205515870-f24ca4db-106b-4969-b010-ed26fee284d0.png)

![image](https://user-images.githubusercontent.com/103590679/205515873-b99afb8d-9981-4496-8e9f-f769041845e9.png)

![image](https://user-images.githubusercontent.com/103590679/205515877-af5a8f16-3599-4efc-b61a-e56adca2b05d.png)

![image](https://user-images.githubusercontent.com/103590679/205515882-db417fb6-f23b-4bf4-913e-17e61fd72747.png)

![image](https://user-images.githubusercontent.com/103590679/205515885-f3dac8a6-8e36-4a63-9b6b-f18feb7a98f8.png)

![image](https://user-images.githubusercontent.com/103590679/205515888-1aa7e622-0c52-4c19-b75f-47713b226176.png)

![image](https://user-images.githubusercontent.com/103590679/205515891-478066f7-eec2-4a7c-aabb-9e912484c4a6.png)

![image](https://user-images.githubusercontent.com/103590679/205515895-6a6bd302-00b6-4e7b-ba1d-a81a859dd555.png)

![image](https://user-images.githubusercontent.com/103590679/205515900-8afd2f4f-5ee8-42ca-90ba-b3da1b1397c0.png)

![image](https://user-images.githubusercontent.com/103590679/205515908-82a57737-1e38-4496-a092-75bba0860441.png)

![image](https://user-images.githubusercontent.com/103590679/205515919-8a777a0b-37a8-43d7-a731-2a89e7440cf6.png)

![image](https://user-images.githubusercontent.com/103590679/205515925-458f61f9-ad01-4a3c-b9e9-1ad38e5346f6.png)

![image](https://user-images.githubusercontent.com/103590679/205515934-57896852-eca4-43bd-bc43-48e30f569c8b.png)

![image](https://user-images.githubusercontent.com/103590679/205515938-187e60d6-4a31-40bd-b251-ed56548875e6.png)

![image](https://user-images.githubusercontent.com/103590679/205515941-7f421e29-81e5-4bbe-a678-fcab87e2051f.png)

![image](https://user-images.githubusercontent.com/103590679/205515946-be0e06f0-a05a-4fff-a6ad-8274aab96058.png)

![image](https://user-images.githubusercontent.com/103590679/205515953-67d0eecf-4d3d-43bf-b6c3-5a87eb99b985.png)

![image](https://user-images.githubusercontent.com/103590679/205515963-98c22c38-b76d-49df-be5b-f19e648ccef5.png)

![image](https://user-images.githubusercontent.com/103590679/205515969-536c47f6-3c80-48b7-9732-a66e991525ca.png)

![image](https://user-images.githubusercontent.com/103590679/205515972-231eb24d-58e8-4e8a-ac4b-939066256c18.png)

![image](https://user-images.githubusercontent.com/103590679/205515980-aaa586c6-23b1-49fd-8c83-327f344b76f1.png)

![image](https://user-images.githubusercontent.com/103590679/205515984-176d1623-ef01-45a8-8d6e-bc47db91f3b3.png)




