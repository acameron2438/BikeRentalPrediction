#-----------------Load Packages-----------------------------
library(tidyverse)
library(readr)
library(chron)
library(lubridate)
library(stringr)
#-----------------Import Data-------------------------------
Data <- read_csv("C:/Users/acameron/Desktop/Project/Data.csv")
#Remove observations with missing values- there aren't very many so best to throw them out
#I don't exclude observations missing temp since I don't use it below
Data=subset(Data,is.na(temp_feel)==FALSE & is.na(humidity)==FALSE & is.na(windspeed)==FALSE)
str(Data)
#Change Weather into 3 dummy variables
Data$weather=ifelse(Data$weather=='heavy rain/ice pellets/snow + fog','Light snow or rain',Data$weather)
Data$Clear=ifelse(Data$weather=='Clear or partly cloudy',1,0)
Data$Rain=ifelse(Data$weather=='Light snow or rain',1,0)
Data$Mist=ifelse(Data$weather=='Mist',1,0)
#-----------------Exploratory Analysis------------------------------
#Inspect variables and data types
head(Data)
###Timestamp variable###
#Check for duplicate values in timestamp variable
TimeTable=as.data.frame(table(Data$timestamp))
max(TimeTable$Freq)
min(TimeTable$Freq)
#Create Day, Hour, Year and Full Date variables
Data$Day=word(Data$timestamp)
Data$Hour=word(Data$timestamp,-1)
Data$Hour=substr(Data$Hour,1,2)
Data$Hour=gsub(pattern=':',replacement='',Data$Hour,fixed=TRUE)
Data$Hour=as.numeric(Data$Hour)
Data$Year=substr(Data$Day, nchar(Data$Day)-4+1, nchar(Data$Day))
Data$Day=substr(Data$Day,0,nchar(Data$Day)-5)
Data$Day=as.Date(Data$Day,format='%m/%d')
Data$Day=format(Data$Day,'%m/%d')
Data$FullDate=paste(Data$Day,Data$Year,sep = "/")
Data$FullDate=as.Date(Data$FullDate,format='%m/%d/%Y')
Data$Day=word(Data$timestamp)
Data$Day=substr(Data$Day,0,nchar(Data$Day)-5)
Data$Day=as.Date(Data$Day,format='%m/%d')
#Determine which Years, Days, times of day are included in the data set
Data=arrange(Data,Year,Day,Hour)
table(Data$Year)
table(Data$Day)
table(Data$Hour)
#Conclusion: some days and key holidays are missing from the dataset- data collectors on vacation?
###Season Variable###
SeasonTable=subset(Data,select=c(season,temp,temp_feel))
SeasonTable=summarize(temp_avg=mean(temp,na.rm=TRUE),
                      temp_min=min(temp,na.rm=TRUE),
                      temp_max=max(temp,na.rm=TRUE),
                      tempfeel_avg=mean(temp_feel,na.rm=TRUE),
                      tempfeel_min=min(temp_feel,na.rm=TRUE),
                      tempfeel_max=max(temp_feel,na.rm=TRUE),
                      group_by(.data=Data,season))
#Conclusion: Season variable should be discarded or changed to reflect the correct time period
###Holiday Variable###
HolidayTable=subset(Data,select=c(holiday,Day,Year),holiday=='Yes')
HolidayTable=summarize(Count=length(holiday),
                       group_by(.data=HolidayTable,Day,Year))
#Conclusion: Data set is once again unreliable, showing incorrect dates for holidays, others occur on weekends anyway
#and some holidays are identified for one year but not the other
#Data set tries to identify the following holidays: MLK day, Columbus Day,
#Veteran's Day, July 4th, Labor Day, Easter, New Year's day
#Veteran's Day was observed Friday not the actual day (2017)
#MLK day was celebrated Jan 15 2018 (Monday) but the data shows Jan 16 2018 (Tues)
#Day after New Years is shown for 2018 (why no New Year's day?)
#Easter occurred April 16th 2017 but it shows April 15th. It occurred April 1st 2018 but data shows April 16th 2018
###Working Day Variable###
Data$DayofWeek=weekdays(Data$FullDate)
DayofWeekTable=subset(Data,select=c(FullDate,DayofWeek,holiday,workingday,Year))
DayofWeekTable=summarize(workingdayYes=length(workingday[workingday=='Yes']),
                         workingdayNo=length(workingday[workingday=='No']),
                         group_by(.data=DayofWeekTable,DayofWeek,holiday))
#Conclusion: Except for Monday, all weekdays are not working days when there is a holiday, are working days when 
#there isn't a holiday. Saturday is sometimes a working day when there is a holiday. Sunday is never a working day
#and there are no holidays on Sunday
###Weather Variable
#There is no way to confirm the accuracy of the weather data without consulting outside sources which is outside the scope of the project.
#For the sake of time I will wait and see if there is a correlation with the dependent variable.
###Temp###
ggplot(Data, aes(y=temp,x=''))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+
  geom_jitter(size=.000001,shape=16, position=position_jitter(0.3))+
  ggtitle("Temp BoxPlot")
Data2017=subset(Data,Year==2017)
ggplot(data=Data2017, aes(x=temp, y=demand, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("Demand Versus Temp, 2017 Only")
Data2018=subset(Data,Year==2018)
ggplot(data=Data2018, aes(x=temp, y=demand, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("Demand Versus Temp, 2018 Only")
#Time of day affects temp. As a result I aggregate average temp by day to isolate the 
#impact of temperature on demand. It turns out there is a linear relationship between temp and demand.
TempTable=subset(Data,select = c(Year,Day,temp,demand))
TempTable=summarize(demand=mean(demand,na.rm=TRUE),
                    temp=mean(temp,na.rm=TRUE),
                      group_by(.data=TempTable,Day,Year))
ggplot(data=TempTable, aes(x=temp, y=demand, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("Average Daily demand Versus Average Daily temp")
cor(Data$temp,Data$demand,use='complete.obs')
cor(TempTable$temp,TempTable$demand,use='complete.obs')
###Temp_Feel###
ggplot(Data, aes(y=temp_feel,x=''))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+
  geom_jitter(size=.000001,shape=16, position=position_jitter(0.3))+
  ggtitle("temp_feel BoxPlot")
#I aggregate temp_feel by day to more clearly observe it's correlation with demand
TempfeelTable=subset(Data,select = c(Year,Day,temp_feel,demand))
TempfeelTable=summarize(demand=mean(demand,na.rm=TRUE),
                    temp_feel=mean(temp_feel,na.rm=TRUE),
                    group_by(.data=TempfeelTable,Day,Year))
ggplot(data=TempfeelTable, aes(x=temp_feel, y=demand, group=Year)) +
  geom_point(aes(color=Year)) +
  ggtitle("Average Daily demand Versus Average Daily temp_feel")
cor(Data$temp_feel,Data$demand,use='complete.obs')
cor(TempfeelTable$temp_feel,TempfeelTable$demand)
cor(Data$temp,Data$temp_feel,use='complete.obs')
#temp and temp_feel are almost perfectly correlated so only one variable is needed for the model.
#temp_feel has a slightly higher correlation with the dependant variable when aggregated by day.
###Humidity###
ggplot(Data, aes(y=humidity,x=''))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+
  geom_jitter(size=.000001,shape=16,position=position_jitter(0.3))+
  ggtitle("Humidity BoxPlot")
ggplot(data=Data, aes(x=humidity, y=demand, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("demand Versus humidity")
HumidityTable=subset(Data,select = c(Year,Day,humidity,demand))
HumidityTable=summarize(demand=mean(demand,na.rm=TRUE),
                        humidity=mean(humidity,na.rm=TRUE),
                        group_by(.data=HumidityTable,Day,Year))
ggplot(data=HumidityTable, aes(x=humidity, y=demand, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("Average Daily demand Versus Average Daily humidity")
cor(Data$humidity,Data$demand,use='complete.obs')
cor(HumidityTable$humidity,HumidityTable$demand,use='complete.obs')
#Humidity appears to be negatively correlated with demand but most of the correlation disappears when aggregated by day
#To better understand the impact of time of day on humidity I create a plot
HumidityHourTable=subset(Data,select = c(Year,Hour,humidity))
HumidityHourTable=summarize(humidity=mean(humidity,na.rm=TRUE),
                             group_by(.data=HumidityHourTable,Hour,Year))
ggplot(data=HumidityHourTable, aes(x=humidity, y=Hour, group=Year)) +
  geom_point(aes(color=Year)) +
  ggtitle("Average humidity Versus Time of Day")
#It appears humidity is least at the time of day where demand is greatest. As a result agreggating humdidity by day
#removes most of the negative correlation between humidity and demand
###Windspeed###
ggplot(Data, aes(y=windspeed,x=''))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+
  geom_jitter(size=.000001,shape=16, position=position_jitter(0.30)) +
  ggtitle("windspeed BoxPlot")
ggplot(data=Data, aes(x=windspeed, y=demand, group=Year)) +
  geom_point(aes(color=Year)) +
  ggtitle("demand Versus windspeed")
WindspeedTable=subset(Data,select = c(Year,Day,windspeed,demand))
WindspeedTable=summarize(demand=mean(demand,na.rm=TRUE),
                        windspeed=mean(windspeed,na.rm=TRUE),
                        group_by(.data=WindspeedTable,Day,Year))
ggplot(data=WindspeedTable, aes(x=windspeed, y=demand, group=Year)) +
  geom_point(aes(color=Year)) +
  ggtitle("Average Daily demand Versus Average Daily windspeed")
cor(Data$windspeed,Data$demand,use='complete.obs')
cor(WindspeedTable$windspeed,WindspeedTable$demand,use='complete.obs')
cor(Data$windspeed,Data$temp,use='complete.obs')
cor(Data$windspeed,Data$temp_feel,use='complete.obs')
#I notice windspeed has a modest possitive correlation with demand, but when I aggregate it by day it
#has a larger negative correlation with demand. This makes me wonder if time of day is correlated with
#windspeed and is hiding the true relationship between windspeed and demand
cor(Data$windspeed,Data$Hour,use='complete.obs')
#Correlation does not likely capture the complete relationship between windspeed and time of day so I create a plot
WindspeedHourTable=subset(Data,select = c(Year,Hour,windspeed,demand))
WindspeedHourTable=summarize(windspeed=mean(windspeed,na.rm=TRUE),
                         group_by(.data=WindspeedHourTable,Hour,Year))
ggplot(data=WindspeedHourTable, aes(x=windspeed, y=Hour, group=Year)) +
  geom_point(aes(color=Year))+
  ggtitle("Average windspeed by Time of Day")
#It turns out windspeed is in fact correlated with time of day, meaning the model has to control
#for the time of day to identify the correct relationship between windspeed and demand. This implies
#windspeed has a negative correlation with demand, not positive.
###Demand###
#Boxplots
ggplot(Data, aes(y=demand,x=''))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+
  geom_jitter(size=.000001,shape=16, position=position_jitter(0.3))+
  ggtitle("demand BoxPlot")
ggplot(Data, aes(y=demand,x=DayofWeek))+ 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4,notch=TRUE)+
  geom_jitter(size=.000001,shape=16, position=position_jitter(0.3))+
  ggtitle("demand BoxPlot by Day of Week")
###Line Plot by Day###
DailyTable=subset(Data,select = c(FullDate,Day,Year,demand))
DailyTable=summarize(FullDate=max(FullDate),
                     demand=mean(demand,na.rm=TRUE),
                     group_by(.data=DailyTable,Year,Day))
ggplot(data=DailyTable, aes(x=Day, y=demand, group=Year)) +
  geom_line(aes(color=Year))+
  scale_x_date(date_breaks='4 weeks',minor_breaks = '1 week',date_labels='%m/%d')+
  ggtitle("Average Daily demand Versus Day")
###Line Plot by Day of Week###
DayofWeekTable=subset(Data,select = c(Year,DayofWeek,demand))
DayofWeekTable=summarize(demand=mean(demand,na.rm=TRUE),
                     group_by(.data=DayofWeekTable,Year,DayofWeek))
DayofWeekTable$DayofWeek=factor(DayofWeekTable$DayofWeek,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
DayofWeekTable=DayofWeekTable[order(DayofWeekTable$Year,DayofWeekTable$DayofWeek),]
ggplot(data=DayofWeekTable, aes(x=DayofWeek, y=demand, group=Year)) +
  geom_line(aes(color=Year))+
  ggtitle("Average demand By Day of Week")
###Line Plot by Day of Week V2###
DayofWeekTable=subset(Data,select = c(Year,Day,DayofWeek,demand))
DayofWeekTable=subset(DayofWeekTable,Day<'2021-07-26')
DayofWeekTable=summarize(demand=mean(demand,na.rm=TRUE),
                         group_by(.data=DayofWeekTable,Year,DayofWeek))
DayofWeekTable$DayofWeek=factor(DayofWeekTable$DayofWeek,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
DayofWeekTable=DayofWeekTable[order(DayofWeekTable$Year,DayofWeekTable$DayofWeek),]
ggplot(data=DayofWeekTable, aes(x=DayofWeek, y=demand, group=Year)) +
  geom_line(aes(color=Year))+
  ggtitle("Average demand by Day of Week, Excluding Dates After 2021-07-25")
###Line Plot by Day of Week No Outliers###
#Calculate upper and lower limits used to identify outliers
DayofWeekTable=subset(Data,select = c(Year,Day,DayofWeek,demand))
DayofWeekAvgTable=summarize(demand_avg=mean(demand,na.rm=TRUE),
                            IQR=IQR(demand,na.rm=TRUE),
                         group_by(.data=DayofWeekTable,Year,DayofWeek))
DayofWeekAvgTable$DayofWeek=factor(DayofWeekAvgTable$DayofWeek,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
DayofWeekAvgTable=DayofWeekAvgTable[order(DayofWeekAvgTable$Year,DayofWeekAvgTable$DayofWeek),]
DayofWeekAvgTable$Upper=DayofWeekAvgTable$demand_avg+(.5*DayofWeekAvgTable$IQR)
DayofWeekAvgTable$Lower=DayofWeekAvgTable$demand_avg-(.5*DayofWeekAvgTable$IQR)
#Store upper and lower limits as variables
Monday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Monday','Upper'])
Monday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Monday','Lower'])
Tuesday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Tuesday','Upper'])
Tuesday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Tuesday','Lower'])
Wednesday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Wednesday','Upper'])
Wednesday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Wednesday','Lower'])
Thursday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Thursday','Upper'])
Thursday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Thursday','Lower'])
Friday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Friday','Upper'])
Friday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Friday','Lower'])
Saturday2017Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Saturday','Upper'])
Saturday2017Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2017 & DayofWeekAvgTable$DayofWeek=='Saturday','Lower'])
Monday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Monday','Upper'])
Monday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Monday','Lower'])
Tuesday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Tuesday','Upper'])
Tuesday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Tuesday','Lower'])
Wednesday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Wednesday','Upper'])
Wednesday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Wednesday','Lower'])
Thursday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Thursday','Upper'])
Thursday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Thursday','Lower'])
Friday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Friday','Upper'])
Friday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Friday','Lower'])
Saturday2018Upper=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Saturday','Upper'])
Saturday2018Lower=as.numeric(DayofWeekAvgTable[DayofWeekAvgTable$Year==2018 & DayofWeekAvgTable$DayofWeek=='Saturday','Lower'])
#Filter out Outliers
DayofWeekTableMonday2017=subset(DayofWeekTable,DayofWeek=='Monday' & demand < Monday2017Upper & demand > Monday2017Lower)
DayofWeekTableTuesday2017=subset(DayofWeekTable,DayofWeek='Tuesday' & demand < Tuesday2017Upper & demand > Tuesday2017Lower)
DayofWeekTableWednesday2017=subset(DayofWeekTable,DayofWeek='Wednesday' & demand < Wednesday2017Upper & demand > Wednesday2017Lower)
DayofWeekTableThursday2017=subset(DayofWeekTable,DayofWeek='Thursday' & demand < Thursday2017Upper & demand > Thursday2017Lower)
DayofWeekTableFriday2017=subset(DayofWeekTable,DayofWeek='Friday' & demand < Friday2017Upper & demand > Friday2017Lower)
DayofWeekTableSaturday2017=subset(DayofWeekTable,DayofWeek='Saturday' & demand < Saturday2017Upper & demand > Saturday2017Lower)
DayofWeekTableSunday2017=subset(DayofWeekTable,DayofWeek='Sunday' & demand < Sunday2017Upper & demand > Sunday2017Lower)
DayofWeekTableMonday2018=subset(DayofWeekTable,DayofWeek='Monday' & demand < Monday2018Upper & demand > Monday2018Lower)
DayofWeekTableTuesday2018=subset(DayofWeekTable,DayofWeek='Tuesday' & demand < Tuesday2018Upper & demand > Tuesday2018Lower)
DayofWeekTableWednesday2018=subset(DayofWeekTable,DayofWeek='Wednesday' & demand < Wednesday2018Upper & demand > Wednesday2018Lower)
DayofWeekTableThursday2018=subset(DayofWeekTable,DayofWeek='Thursday' & demand < Thursday2018Upper & demand > Thursday2018Lower)
DayofWeekTableFriday2018=subset(DayofWeekTable,DayofWeek='Friday' & demand < Friday2018Upper & demand > Friday2018Lower)
DayofWeekTableSaturday2018=subset(DayofWeekTable,DayofWeek='Saturday' & demand < Saturday2018Upper & demand > Saturday2018Lower)
DayofWeekTableSunday2018=subset(DayofWeekTable,DayofWeek='Sunday' & demand < Sunday2018Upper & demand > Sunday2018Lower)
DayofWeekTable=rbind(
  DayofWeekTableMonday2017, DayofWeekTableTuesday2017, DayofWeekTableWednesday2017, 
  DayofWeekTableThursday2017, DayofWeekTableFriday2017, DayofWeekTableSaturday2017,
  DayofWeekTableSunday2017, DayofWeekTableMonday2018, DayofWeekTableTuesday2018,
  DayofWeekTableWednesday2018, DayofWeekTableThursday2018, DayofWeekTableFriday2018,
  DayofWeekTableSaturday2018, DayofWeekTableSunday2018)
#Calculate mean by day of week excluding outliers
DayofWeekTable=summarize(demand=mean(demand,na.rm=TRUE),
                         group_by(.data=DayofWeekTable,Year,DayofWeek))
DayofWeekTable$DayofWeek=factor(DayofWeekTable$DayofWeek,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
DayofWeekTable=DayofWeekTable[order(DayofWeekTable$Year,DayofWeekTable$DayofWeek),]
#generate line plot
ggplot(data=DayofWeekTable, aes(x=DayofWeek, y=demand, group=Year)) +
  geom_line(aes(color=Year)) +
  ggtitle("Average Demand by Day of Week") +
  ggtitle("Average demand by Day of Week, Outliers Excluded")
###Line Plot by Hour###
HourlyTable=subset(Data,select = c(Year,Hour,demand))
HourlyTable=summarize(demand=mean(demand,na.rm=TRUE),
                     group_by(.data=HourlyTable,Hour,Year))
ggplot(data=HourlyTable, aes(x=Hour, y=demand, group=Year)) +
  geom_line(aes(color=Year))+
  scale_x_continuous(breaks=seq(from=0,to=24,by=3),labels=c('12 AM','3','6','9','12 PM','3','6','9','12 AM')) +
  ggtitle("Average demand By Time of Day")
#Break out hour by day of the week
#2017
HourlyTable2=subset(Data,select = c(DayofWeek,Hour,demand),Year==2017)
HourlyTable2=summarize(demand=mean(demand,na.rm=TRUE),
                      group_by(.data=HourlyTable2,Hour,DayofWeek))
ggplot(data=HourlyTable2, aes(x=Hour, y=demand, group=DayofWeek)) +
  labs(title='2017')+
  geom_line(aes(color=DayofWeek))+
  scale_x_continuous(breaks=seq(from=0,to=23,by=2),labels=c('12','2 AM','4 AM','6 AM','8 AM','10 AM','12','2 PM','4 PM','6 PM','8 PM','10 PM')) +
  ggtitle("Average demand Versus Time of Day, Broken Out By Week, 2017 Only")
#2018
HourlyTable2=subset(Data,select = c(DayofWeek,Hour,demand),Year==2018)
HourlyTable2=summarize(demand=mean(demand,na.rm=TRUE),
                       group_by(.data=HourlyTable2,Hour,DayofWeek))
ggplot(data=HourlyTable2, aes(x=Hour, y=demand, group=DayofWeek)) +
  labs(title='2018')+
  geom_line(aes(color=DayofWeek))+
  scale_x_continuous(breaks=seq(from=0,to=23,by=2),labels=c('12','2 AM','4 AM','6 AM','8 AM','10 AM','12','2 PM','4 PM','6 PM','8 PM','10 PM')) +
  ggtitle("Average demand Versus Time of Day, Broken Out By Week, 2018 Only")
#Both Years
HourlyTable2=subset(Data,select = c(DayofWeek,Hour,demand))
HourlyTable2=summarize(demand=mean(demand,na.rm=TRUE),
                       group_by(.data=HourlyTable2,Hour,DayofWeek))
ggplot(data=HourlyTable2, aes(x=Hour, y=demand, group=DayofWeek)) +
  labs(title='Both Years')+
  geom_line(aes(color=DayofWeek))+
  scale_x_continuous(breaks=seq(from=0,to=23,by=2),labels=c('12','2 AM','4 AM','6 AM','8 AM','10 AM','12','2 PM','4 PM','6 PM','8 PM','10 PM')) +
  ggtitle("Average demand Versus Time of Day, Broken Out By Week, Both Years")
#WorkingDay
HourlyTable2=subset(Data,select = c(workingday,Hour,demand))
HourlyTable2=summarize(demand=mean(demand,na.rm=TRUE),
                       group_by(.data=HourlyTable2,Hour,workingday))
ggplot(data=HourlyTable2, aes(x=Hour, y=demand, group=workingday)) +
  labs(title='Both Years')+
  geom_line(aes(color=workingday))+
  scale_x_continuous(breaks=seq(from=0,to=23,by=2),labels=c('12','2 AM','4 AM','6 AM','8 AM','10 AM','12','2 PM','4 PM','6 PM','8 PM','10 PM')) +
  ggtitle("Avg demand Versus Time of Day, by workingday Variable")

#-----------------Build Model------------------------------
###Data preperation###
#Create new version of hour variable to make it parabolic
Data$HourB=ifelse(Data$Hour>=4,Data$Hour-4,ifelse(Data$Hour<4,Data$Hour+20,99))
#Recode workingday variable into 1 and 0
Data$workingdayB=ifelse(Data$workingday=='Yes',1,0)
#Create morning variable and afternoon variable
Data$Morning=ifelse(Data$Hour>=8&Data$Hour<=12,Data$Hour-7,0)
Data$Afternoon=ifelse(Data$Hour>=13&Data$Hour<=17,Data$Hour-12,0)
#Create variable to mark passage of time (days)
Time=as.data.frame(unique(Data$FullDate))
Time$DayIndex=''
for (i in 1:nrow(Time))
{
  Time[i,2]=i
}
colnames(Time)=c('FullDate','DayIndex')
Data=merge(Data,Time,by.x='FullDate',by.y='FullDate',all.x=FALSE)
Data$DayIndex=as.numeric(Data$DayIndex)

#Create actual model
options(scipen = 100)

Model=lm(demand~DayIndex+DayofWeek+Rain+Mist+temp_feel+humidity+windspeed+HourB*workingdayB+I(HourB^2)*workingdayB+I(Morning-8)*workingdayB+I(Morning^2-8)*workingdayB+I(Afternoon-13)*workingdayB+I(Afternoon^2-13)*workingdayB,data=Data)

summary(Model)
sq_residuals=as.data.frame(Model$residuals)^2
avg_sq_residuals=mean(sq_residuals$`Model$residuals`)
RMSError=sqrt(avg_sq_residuals)
#-----------------Model Diagnostics------------------------------
qqnorm(Model$residuals)
abline(0,1,col="red")
#Model doesn't seem to be normally distributed but it could be worse

plot(Model$fitted.values,Model$residuals)
abline(h=0,col="red")
#Residuals appear to be randomly distributed

plot(Data$demand,Model$fitted.values)
abline(0,1,col="red")
#Predicted values increase and fall as the dependant variable increases and falls suggesting the model
#fits the data 

#-----------------Plot Model------------------------------
#Plot daily averages from model predictors and actual demand from the dataset
Data$Prediction=predict(Model)
DailyTable=subset(Data,select = c(FullDate,Day,Year,demand,Prediction))
DailyTable=summarize(FullDate=max(FullDate),
                     demand=mean(demand,na.rm=TRUE),
                     Prediction=mean(Prediction,na.rm=TRUE),
                     group_by(.data=DailyTable,Year,Day))
ggplot(data=DailyTable) +
  geom_line(aes(color='Green',x=Day, y=Prediction, group=Year))+
  geom_line(aes(color='Red', x=Day, y=demand, group=Year))+
  scale_x_date(date_breaks='4 weeks',minor_breaks = '1 week',date_labels='%m/%d')
#The model appears to follow the pattern in the data
ggplot(data=Data) +
  geom_line(aes(color='Green',x=timestamp, y=Prediction, group=Year))+
  geom_line(aes(color='Red', x=timestamp, y=demand, group=Year))+
  scale_x_date(date_breaks='4 weeks',minor_breaks = '1 week',date_labels='%m/%d')

#Month worth of data
DataSubset=subset(Data,FullDate<'2017-02-01')
ggplot(data=DataSubset) +
  geom_line(aes(color='Green',x=timestamp, y=Prediction, group=Year))+
  geom_line(aes(color='Red', x=timestamp, y=demand, group=Year))+
  scale_x_discrete(breaks=seq(from=1,to=417,by=1))

#Four days of data
DataSubset=subset(Data,FullDate<'2017-01-05')
ggplot(data=DataSubset) +
  geom_line(aes(color='Green',x=timestamp, y=Prediction, group=Year))+
  geom_line(aes(color='Red', x=timestamp, y=demand, group=Year))+
  scale_x_discrete(breaks=seq(from=1,to=417,by=1))