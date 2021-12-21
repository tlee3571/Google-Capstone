# Downloading necessary packages
install.packages('lubridate')
library('lubridate')
install.packages("tidyverse")
library("tidyverse")
library(ggplot2)
library(scales)
install.packages('reshape2')
library('reshape2')


# Set Directory to download Excel files to R 
setwd("C:/Users/tlee3/OneDrive/Desktop/Documents/Google Data Analyst Certificate/Bike Sharing Cyclistic Files/BSC CSV Files")

# Load Excel files
Aug2020<-read.csv("202008-divvy-tripdata.csv")
Sep2020<-read.csv("202009-divvy-tripdata.csv")
Oct2020<-read.csv("202010-divvy-tripdata.csv")
Nov2020<-read.csv("202011-divvy-tripdata.csv")
Dec2020<-read.csv("202012-divvy-tripdata.csv")
Jan2021<-read.csv("202101-divvy-tripdata.csv")
Feb2021<-read.csv("202102-divvy-tripdata.csv")
Mar2021<-read.csv("202103-divvy-tripdata.csv")
Apr2021<-read.csv("202104-divvy-tripdata.csv")
May2021<-read.csv("202105-divvy-tripdata.csv")
Jun2021<-read.csv("202106-divvy-tripdata.csv")
Jul2021<-read.csv("202107-divvy-tripdata.csv")

# Select only necessary columns for analysis and because R does not have enough space/memory 
Aug2020<- select(Aug2020, 1:4, 13:15)
Sep2020<- select(Sep2020, 1:4, 13:15)
Oct2020<- select(Oct2020, 1:4, 13:15)
Nov2020<- select(Nov2020, 1:4, 13:15)
Dec2020<- select(Dec2020, 1:4, 13:15)
Jan2021<- select(Jan2021, 1:4, 13:15)
Feb2021<- select(Feb2021, 1:4, 13:15)
Mar2021<- select(Mar2021, 1:4, 13:15)
Apr2021<- select(Apr2021, 1:4, 13:15)
May2021<- select(May2021, 1:4, 13:15)
Jun2021<- select(Jun2021, 1:4, 13:15)
Jul2021<- select(Jul2021, 1:4, 13:15)

# Creating a new column - Month-Year
Aug2020<-Aug2020 %>% mutate(started_at2=started_at)
names(Aug2020)[names(Aug2020) == 'started_at2'] <- 'Month-Year'

# Extracting only necessary information (month and year) by converting ride_length column to Factor, then Posixlt, and then Date format
b<-as.factor(Aug2020$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Aug2020$`Month-Year`<-format(as.Date(e), '%m-%Y')

# Changing ride_length column units into number format (hms)
Aug2020$ride_length<-hms(Aug2020$ride_length)

# Shows that the NA in the ride_length columns is because the ride_length is zero based on the subtraction of start time and end time
which(is.na(Aug2020$ride_length), arr.ind = TRUE)

Sep2020<-Sep2020 %>% mutate(started_at2=started_at)
names(Sep2020)[names(Sep2020) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Sep2020$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Sep2020$`Month-Year`<- format(as.Date(e), '%m-%Y')
Sep2020$ride_length<-hms(Sep2020$ride_length)

Oct2020<-Oct2020 %>% mutate(started_at2=started_at)
names(Oct2020)[names(Oct2020) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Oct2020$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Oct2020$`Month-Year`<- format(as.Date(e), '%m-%Y')
Oct2020$ride_length<-hms(Oct2020$ride_length)

Nov2020<-Nov2020 %>% mutate(started_at2=started_at)
names(Nov2020)[names(Nov2020) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Nov2020$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Nov2020$`Month-Year`<- format(as.Date(e), '%m-%Y')
Nov2020$ride_length<-hms(Nov2020$ride_length)

Dec2020<-Dec2020 %>% mutate(started_at2=started_at)
names(Dec2020)[names(Dec2020) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Dec2020$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Dec2020$`Month-Year`<- format(as.Date(e), '%m-%Y')
Dec2020$ride_length<-hms(Dec2020$ride_length)

Jan2021<-Jan2021 %>% mutate(started_at2=started_at)
names(Jan2021)[names(Jan2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Jan2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Jan2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Jan2021$ride_length<-hms(Jan2021$ride_length)

Feb2021<-Feb2021 %>% mutate(started_at2=started_at)
names(Feb2021)[names(Feb2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Feb2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Feb2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Feb2021$ride_length<-hms(Feb2021$ride_length)

Mar2021<-Mar2021 %>% mutate(started_at2=started_at)
names(Mar2021)[names(Mar2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Mar2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Mar2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Mar2021$ride_length<-hms(Mar2021$ride_length)

Apr2021<-Apr2021 %>% mutate(started_at2=started_at)
names(Apr2021)[names(Apr2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Apr2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Apr2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Apr2021$ride_length<-hms(Apr2021$ride_length)

May2021<-May2021 %>% mutate(started_at2=started_at)
names(May2021)[names(May2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(May2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
May2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
May2021$ride_length<-hms(May2021$ride_length)

Jun2021<-Jun2021 %>% mutate(started_at2=started_at)
names(Jun2021)[names(Jun2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Jun2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Jun2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Jun2021$ride_length<-hms(Jun2021$ride_length)

Jul2021<-Jul2021 %>% mutate(started_at2=started_at)
names(Jul2021)[names(Jul2021) == 'started_at2'] <- 'Month-Year'
b<-as.factor(Jul2021$`Month-Year`)
d<-strptime(b, format = '%m/%d/%Y')
e<-as.Date(format(as.Date(d, '%Y-%m-%d'), '%m/%d/%Y'), format = '%m/%d/%Y')
Jul2021$`Month-Year`<- format(as.Date(e), '%m-%Y')
Jul2021$ride_length<-hms(Jul2021$ride_length)

#creating a master data frame from all the data frames of the individual months
Comb_BSC<-list (Aug2020,Sep2020,Oct2020,Nov2020,Dec2020,Jan2021,Feb2021,Mar2021,Apr2021,May2021,Jun2021,Jul2021)
Comb_BSC<-Comb_BSC %>% reduce(full_join, all=TRUE)


#Changing Month-Year column from character class to date class for plotting
Comb_BSC$`Month-Year`<-as.Date(paste0('01-', Comb_BSC$`Month-Year`), format = '%d-%m-%Y')

#Adding Season column to data set
Comb_BSC[ ,'Season'] <- NA
Comb_BSC <- Comb_BSC %>% mutate(Season = case_when(`Month-Year` == '2021-06-01' ~ 'Summer', `Month-Year` == '2021-07-01' ~ 'Summer', `Month-Year` == '2020-08-01' ~ 'Summer', `Month-Year`== '2020-09-01' ~ 'Fall', `Month-Year`== '2020-10-01' ~ 'Fall', `Month-Year`== '2020-11-01' ~ 'Fall', `Month-Year`== '2020-12-01' ~ 'Winter', `Month-Year`== '2021-01-01' ~ 'Winter', `Month-Year`== '2021-02-01' ~ 'Winter', `Month-Year`== '2021-03-01' ~ 'Spring', `Month-Year`== '2021-04-01' ~ 'Spring', `Month-Year`== '2021-05-01' ~ 'Spring'))   

#Changing day_of_week from numbers to characters
Comb_BSC$day_of_week <- as.character(Comb_BSC$day_of_week) 
Comb_BSC<- Comb_BSC %>%  
  mutate(day_of_week = case_when(day_of_week == '1' ~ 'Sunday', day_of_week == '2' ~ 'Monday',day_of_week == '3' ~ 'Tuesday', day_of_week == '4' ~ 'Wednesday', day_of_week == '5' ~ 'Thursday', day_of_week == '6' ~ 'Friday', day_of_week == '7' ~ 'Saturday'))

#Creating column rl_min (ridelength min), where the units are in minutes, not the standard conversion to seconds
rl_min<- (hour(Comb_BSC$ride_length) * 60) + (minute(Comb_BSC$ride_length)) + (second(Comb_BSC$ride_length)/60)
rl_min

#Creating column rl_hour (ridelength hour), where the units are in hours, not the standard conversion to seconds
rl_hour <- (hour(Comb_BSC$ride_length)) + (minute(Comb_BSC$ride_length)/60) + (second(Comb_BSC$ride_length)/3600)
rl_hour

#Adding column rl_min, and rl_hour to dataframe Comb_BSC
Comb_BSC<- cbind(Comb_BSC, rl_min, rl_hour)

#Counting number of riders with missing (NA because difference is zero) ride length values by membership type
miss_na<- Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(NA_ride_length_count = sum(is.na(ride_length)),
            total_count=n(),
            NA_percent = paste0(round((NA_ride_length_count/total_count)*100, 4), '%'), 
            Non_NA_ride_length_count= sum(!is.na(ride_length)),
            Non_NA_percent = paste0(round((Non_NA_ride_length_count/total_count)*100, 4), '%'))  %>%
    as.data.frame()
miss_na

miss_na_2<- Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(NA_ride_length_count = sum(is.na(ride_length)),
            Non_NA_ride_length_count= sum(!is.na(ride_length)))  %>%
  as.data.frame()
miss_na_2

#Melting data frame miss_na_2 (to obtain the descriptive variable of the sum of the rows with vs. without NA in the ride length column) based on member_casual grouping
miss_na_3<-melt(miss_na_2, id.vars = c( 'member_casual'))
miss_na_3

#Log scale of total number of rides with vs. without NA in the ride_length column  
ggplot(data=miss_na_3, (aes(x=member_casual , y= value, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(value)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Number of Rides', title = 'Number of Missing vs. Non-Missing Ride Length') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~variable) 

#Dropping rows where ride length is NA
Comb_BSC<- Comb_BSC %>%
 drop_na(ride_length)

#Check to confirm that all rows that had ride length of NA are dropped
which(is.na(Comb_BSC$ride_length), arr.ind = TRUE)

#Adding <or> to 90 minutes column 
Comb_BSC[ ,'LG60'] <- NA
Comb_BSC<-Comb_BSC%>% mutate(LG60 = case_when(rl_min<60 ~ 'Less', rl_min>=60 ~'Greater'))

#Adding <or> to 90 minutes column 
Comb_BSC[ ,'LG90'] <- NA
Comb_BSC<- Comb_BSC %>% mutate(LG90 = case_when(rl_min<90 ~ 'Less', rl_min>=90 ~ 'Greater'))

#Adding <or> to 120 minutes column 
Comb_BSC[ ,'LG120'] <- NA
Comb_BSC <- Comb_BSC %>% mutate(LG120 = case_when(rl_min<120 ~ 'Less', rl_min>=120 ~ 'Greater'))

#Adding <or> to 180 minutes column 
Comb_BSC[ ,'LG180'] <- NA
Comb_BSC <- Comb_BSC %>% mutate(LG180 = case_when(rl_min<180 ~ 'Less', rl_min>=180 ~ 'Greater'))


table(format(Comb_BSC$`Month-Year`, '%b-%Y'))

#Graphing the total number of casual vs. yearly riders
tot1<-Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(count=n()) %>%
  as.data.frame()
tot1

ggplot(data=tot1, aes(x=member_casual, y=count, fill=member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=count), vjust = -0.5) + labs(y= 'Number of Rides', title = 'Number of Rides of Annual vs. Casual Riders') + ylim(0,max(tot1$count)*1.05)

#Graphing the percentage of casual vs. yearly rides
tot2<-Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(member_casual = factor(member_casual, levels = c('casual', 'member')),
         cumulative = cumsum(count),
         midpoint = cumulative - count/2,
         label = paste0(member_casual, ' ', round(count/sum(count)*100, digits=0),'%')) %>%
  as.data.frame()
tot2


ggplot(data=tot2, aes(x=1, y= count, fill=member_casual)) + geom_bar(stat = 'identity', width = 1, color= 'black') + coord_polar('y', start = 0) + geom_text(aes(x=1, y= midpoint, label = label, size = 6)) + theme_void() + labs(title = 'Percent of Total Rides: Annual vs. Casual', hjust=0.5) + theme(plot.title = element_text(hjust = 0.5))

#Graphing the percentage of casual vs. yearly riders by month
m1<- Comb_BSC %>% 
  group_by(`Month-Year`, member_casual) %>% 
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0),'%')) %>%
  as.data.frame() 
m1

ggplot(data=m1, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=m1, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(m1$count)*1.03) 
ggplot(data=m1, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_line(aes(color=member_casual)) + geom_point(aes(color= member_casual)) + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(m1$count)*1.03) 

#Graphing the percentage of casual vs. yearly riders by season
m2<-Comb_BSC %>%
  group_by(Season, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0),'%')) %>%
  as.data.frame() %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring')))
m2

ggplot(data=m2, (aes(x=`Season`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Seasom') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=m2, (aes(x=`Season`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(m2$count)*1.03) 

#Graphing the percentage of casual vs. yearly riders by day of week
dow_order<-c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

dow_1 <- Comb_BSC %>%
  group_by(day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  as.data.frame()
dow_1

ggplot(data=dow_1, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=dow_1, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_1$count)*1.03)

#Graphing the percentage of casual vs. yearly riders by day of week by Month

dow_2 <- Comb_BSC %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame() %>%
  mutate(across(Month_Year, factor, levels = c('Aug-2020', 'Sep-2020', 'Oct-2020', 'Nov-2020', 'Dec-2020', 'Jan-2021', 'Feb-2021', 'Mar-2021', 'Apr-2021', 'May-2021', 'Jun-2021', 'Jul-2021'))) %>%
  mutate(across (day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')))
dow_2

ggplot(data=dow_2, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =2, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~Month_Year)
ggplot(data=dow_2, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=1.5, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_2$count)*1.1) +  facet_wrap(~Month_Year) 

#Graphing the percentage of casual vs. yearly riders by day of week by Month (individually) 
dow_Aug <- Comb_BSC %>%
  filter(`Month-Year`== '2020-08-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Aug

ggplot(data=dow_Aug, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in August') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Aug, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in August') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Aug$count)*1.1)  

dow_Sep <- Comb_BSC %>%
  filter(`Month-Year`== '2020-09-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Sep

ggplot(data=dow_Sep, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in September') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sep, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in September') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sep$count)*1.1) 

dow_Oct <- Comb_BSC %>%
  filter(`Month-Year`== '2020-10-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Oct

ggplot(data=dow_Oct, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in October') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Oct, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in October') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Oct$count)*1.1)  

dow_Nov <- Comb_BSC %>%
  filter(`Month-Year`== '2020-11-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Nov

ggplot(data=dow_Nov, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in November') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Nov, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in November') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Nov$count)*1.1) 

dow_Dec <- Comb_BSC %>%
  filter(`Month-Year`== '2020-12-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Dec

ggplot(data=dow_Dec, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in December') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Dec, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in December') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Dec$count)*1.1)  

dow_Jan <- Comb_BSC %>%
  filter(`Month-Year`== '2021-01-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Jan

ggplot(data=dow_Jan, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in January') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Jan, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in January') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Jan$count)*1.1)  

dow_Feb <- Comb_BSC %>%
  filter(`Month-Year`== '2021-02-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Feb

ggplot(data=dow_Feb, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in February') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Feb, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in February') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Feb$count)*1.1)  

dow_Mar <- Comb_BSC %>%
  filter(`Month-Year`== '2021-03-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Mar

ggplot(data=dow_Mar, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in March') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Mar, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in March') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Mar$count)*1.1)  

dow_Apr <- Comb_BSC %>%
  filter(`Month-Year`== '2021-04-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Apr

ggplot(data=dow_Apr, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in April') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Apr, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in April') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Apr$count)*1.1) 

dow_May <- Comb_BSC %>%
  filter(`Month-Year`== '2021-05-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_May

ggplot(data=dow_May, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in May') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_May, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in May') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_May$count)*1.1)  

dow_Jun <- Comb_BSC %>%
  filter(`Month-Year`== '2021-06-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Jun

ggplot(data=dow_Jun, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in June') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Jun, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in June') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Jun$count)*1.1)  

dow_Jul <- Comb_BSC %>%
  filter(`Month-Year`== '2021-07-01') %>%
  group_by(`Month-Year`,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  mutate(Month_Year = format(`Month-Year`, '%b-%Y')) %>%
  select(Month_Year, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Jul

ggplot(data=dow_Jul, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in July') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Jul, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in July') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Jul$count)*1.1)  

#Graphing the percentage of casual vs. yearly riders by day of week by Season 
dow_Win <- Comb_BSC %>%
  filter(Season == 'Winter') %>%
  group_by(Season,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Win

ggplot(data=dow_Win, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Winter') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Win, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Winter') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Win$count)*1.1)  

dow_Spr<- Comb_BSC %>%
  filter(Season == 'Spring') %>%
  group_by(Season,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Spr

ggplot(data=dow_Spr, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Spring') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Spr, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Spring') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Spr$count)*1.1)  

dow_Sum<- Comb_BSC %>%
  filter(Season == 'Summer') %>%
  group_by(Season,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Sum

ggplot(data=dow_Sum, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Summer') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sum, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Summer') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sum$count)*1.1)  

dow_Fal<- Comb_BSC %>%
  filter(Season == 'Fall') %>%
  group_by(Season,  day_of_week, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season,  day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, day_of_week, member_casual, count, percent) %>%
  as.data.frame()
dow_Fal

ggplot(data=dow_Fal, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Fall') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Fal, (aes(x=day_of_week, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Day of Week in Fall') + scale_x_discrete(limit = dow_order) + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Fal$count)*1.1)  

#Graphing the percentage of Casual vs. yearly riders by month for every day of the week
dow_Sat_Months <- Comb_BSC %>%
  filter(day_of_week == 'Saturday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Sat_Months

ggplot(data=dow_Sat_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Saturday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sat_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Saturday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sat_Months$count)*1.1)  

dow_Sun_Months <- Comb_BSC %>%
  filter(day_of_week == 'Sunday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Sun_Months

ggplot(data=dow_Sun_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Sunday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sun_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Sunday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sun_Months$count)*1.1)  

dow_Mon_Months <- Comb_BSC %>%
  filter(day_of_week == 'Monday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Mon_Months

ggplot(data=dow_Mon_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Monday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Mon_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Monday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Mon_Months$count)*1.1)  

dow_Tue_Months <- Comb_BSC %>%
  filter(day_of_week == 'Tuesday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Tue_Months

ggplot(data=dow_Tue_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Tuesday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Tue_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Tuesday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Tue_Months$count)*1.1)  

dow_Wed_Months <- Comb_BSC %>%
  filter(day_of_week == 'Wednesday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Wed_Months

ggplot(data=dow_Wed_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Wednesday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Wed_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Wednesday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Wed_Months$count)*1.1)  

dow_Thu_Months <- Comb_BSC %>%
  filter(day_of_week == 'Thursday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Thu_Months

ggplot(data=dow_Thu_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Thursday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Thu_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Thursday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Thu_Months$count)*1.1)  

dow_Fri_Months <- Comb_BSC %>%
  filter(day_of_week == 'Friday') %>%
  group_by(`Month-Year`, member_casual) %>%
  summarize(count=n()) %>%
  group_by(`Month-Year`) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(`Month-Year`, member_casual, count, percent) %>%
  as.data.frame()
dow_Fri_Months

ggplot(data=dow_Fri_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Friday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Fri_Months, (aes(x=`Month-Year`, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Month on Friday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Fri_Months$count)*1.1)  

#Graphing the percentage of Casual vs. yearly riders by season for every day of the week
dow_Sat_Season <- Comb_BSC %>%
  filter(day_of_week == 'Saturday') %>%
  group_by(Season, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, member_casual, count, percent) %>%
  as.data.frame()
dow_Sat_Season

ggplot(data=dow_Sat_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Saturday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sat_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Saturday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sat_Season$count)*1.1)  

dow_Sun_Season <- Comb_BSC %>%
  filter(day_of_week == 'Sunday') %>%
  group_by(Season, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, member_casual, count, percent) %>%
  as.data.frame()
dow_Sun_Season

ggplot(data=dow_Sun_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Sunday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Sun_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Sunday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Sun_Season$count)*1.1)  

dow_Mon_Season <- Comb_BSC %>%
  filter(day_of_week == 'Monday') %>%
  group_by(Season, member_casual) %>%
  summarize(count=n()) %>%
  group_by(Season) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, member_casual, count, percent) %>%
  as.data.frame()
dow_Mon_Season

ggplot(data=dow_Mon_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Monday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) 
ggplot(data=dow_Mon_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season on Monday') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(dow_Mon_Season$count)*1.1)  


dow_All_Season <- Comb_BSC %>%
  group_by(Season, member_casual, day_of_week) %>%
  summarize(count=n()) %>%
  group_by(Season, day_of_week) %>%
  mutate(percent = paste0(round((prop.table(count)*100), digits=0), '%')) %>%
  select(Season, member_casual, count, percent, day_of_week) %>%
  as.data.frame() %>%
  mutate(across (day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')))
dow_All_Season

ggplot(data=dow_All_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =3, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season Across Days of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~day_of_week)
ggplot(data=dow_All_Season, (aes(x=Season, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=3, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Season Across Days of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.35)) + ylim(0,max(dow_All_Season$count)*1.1) + facet_wrap(~day_of_week)  


#Graphing the percentage of casual vs. yearly riders by ride type
type1<- Comb_BSC %>%
  group_by(rideable_type, member_casual) %>%
  summarize(count = n()) %>%
  group_by(rideable_type) %>%
  mutate(percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  as.data.frame()
type1  

ggplot(data=type1, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=type1, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(type1$count)*1.03)

#Determining if and when classic bikes was in the market/used at a later date
type1_1_subset<- Comb_BSC %>% subset(rideable_type == 'classic_bike') %>% as.data.frame() 
unique(type1_1_subset$`Month-Year`)

#Graphing the percentage of casual vs. yearly riders by ride type before the introduction of the classic bike
type1_1<- Comb_BSC %>%
  filter(!(`Month-Year` %in% unique(type1_1_subset$`Month-Year`))) %>%
  group_by(rideable_type, member_casual) %>%
  summarize(count = n()) %>%
  group_by(rideable_type) %>%
  mutate(percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  as.data.frame() 
type1_1

ggplot(data=type1_1, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type Before the Introduction of the Classic Bike') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=type1_1, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type Before the Introduction of the Classic Bike') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 9)) + ylim(0,max(type1_1$count)*1.03)

#Graphing the percentage of casual vs. yearly riders by ride type after the introduction of the classic bike
type1_2<- Comb_BSC %>%
  filter(`Month-Year` %in% unique(type1_1_subset$`Month-Year`)) %>%
  group_by(rideable_type, member_casual) %>%
  summarize(count = n()) %>%
  group_by(rideable_type) %>%
  mutate(percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  as.data.frame() 
type1_2

ggplot(data=type1_2, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent) ), position = position_stack(vjust=0.2)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type After the Introduction of the Classic Bike') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))
ggplot(data=type1_2, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent) ), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type After the Introduction of the Classic Bike') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 9)) + ylim(0,max(type1_2$count)*1.03)


#Graphing the percentage of casual vs. yearly riders by ride type by Month
type2<- Comb_BSC %>%
  group_by (`Month-Year`, member_casual, rideable_type) %>%
  summarize(count = n()) %>%
  group_by(`Month-Year`, rideable_type) %>%
  mutate (percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  mutate (Month_Year = format(`Month-Year`, '%b-%Y'))  %>%
  select (Month_Year, rideable_type, member_casual, count, percent) %>%
  as.data.frame() %>%
  mutate(across (Month_Year, factor, levels = c('Aug-2020', 'Sep-2020', 'Oct-2020', 'Nov-2020', 'Dec-2020', 'Jan-2021', 'Feb-2021', 'Mar-2021', 'Apr-2021', 'May-2021', 'Jun-2021', 'Jul-2021')))
type2  

ggplot(data=type2, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =2, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~Month_Year)
ggplot(data=type2, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=1.5, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(type2$count)*1.1) +  facet_wrap(~Month_Year) 

#Graphing the percentage of casual vs. yearly riders by ride type by Season
type3<- Comb_BSC %>%
  group_by (Season, member_casual, rideable_type) %>%
  summarize(count = n()) %>%
  group_by(Season, rideable_type) %>%
  mutate (percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  select (Season, rideable_type, member_casual, count, percent) %>%
  as.data.frame() %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring'))) 
type3

ggplot(data=type3, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =4, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~Season)
ggplot(data=type3, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=2.5, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(type3$count)*1.1) +  facet_wrap(~Season) 

#Proof that there are no docked bike rides by annual members in the season of spring 
Proof_Docked_Spring_Member<- subset(Comb_BSC, Season == 'Spring' & rideable_type == 'docked_bike' & member_casual == 'member', 
                                    select = ride_id , member_casual, Season, rideable_type)
Proof_Docked_Spring_Member

which((Comb_BSC$Season == 'Spring' & Comb_BSC$rideable_type == 'docked_bike' & Comb_BSC$member_casual == 'member'), arr.ind = TRUE)


#Graphing the percentage of casual vs. yearly riders by ride type by day of week
type4<- Comb_BSC %>%
  group_by (day_of_week, member_casual, rideable_type) %>%
  summarize(count = n()) %>%
  group_by(day_of_week, rideable_type) %>%
  mutate (percent=paste0(round((prop.table(count)*100), digits = 0), '%')) %>%
  select (day_of_week, rideable_type, member_casual, count, percent) %>%
  as.data.frame() %>%
  mutate(across (day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')))
type4

ggplot(data=type4, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(percent)), size =3.7, position = position_stack(vjust=.5)) + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~day_of_week)
ggplot(data=type4, (aes(x=rideable_type, y= count, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(percent)), size=1.5, position = position_dodge(width = 1), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', title = 'Annual vs. Casual Riders By Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + ylim(0,max(type4$count)*1.1) +  facet_wrap(~day_of_week) 


#Graphing the average ride length of casual vs. yearly riders 
time1 <- Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(mean = mean(rl_min)) %>%
  as.data.frame()
time1

ggplot(data = time1, aes(x=member_casual, y = mean, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label = mean), vjust=-0.5) + labs(y='Ride Length (Minutes)', x = 'Membership Type', title = 'Mean Ride Length of Annual vs. Casual Riders') + ylim(0,max(time1$mean)*1.05) + scale_fill_discrete(name = 'Membership Type')

#Graphing the median ride length of casual vs. yearly riders 
time2<-Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(median = median(rl_min)) %>%
  as.data.frame()
time2

ggplot(data = time2, aes(x=member_casual, y = median, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label = median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x = 'Membership Type', title = 'Median Ride Length of Annual vs. Casual Riders') + ylim(0,max(time2$median)*1.05) + scale_fill_discrete(name = 'Membership Type')

#Total Ride length of annual vs casual members
time3<-Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(sum = sum(rl_min)) %>%
  as.data.frame()
time3

ggplot(data = time3, aes(x=member_casual, y = sum, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label = sum, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Total Ride Length of Annual vs. Casual Riders', x= 'Membership Type') + ylim(0,max(time3$sum)*1.05) + scale_fill_discrete(name = 'Membership Type')

#Percentage of total ride length
tot3_1<-Comb_BSC %>%
  group_by(member_casual) %>%
  summarize(count= sum(rl_min)) %>%
  arrange(desc(member_casual)) %>%
  mutate(member_casual = factor(member_casual, levels = c('casual', 'member')),
         cumulative = cumsum(count),
         midpoint = cumulative - count/2,
         label = paste0(member_casual, ' ', round(count/sum(count)*100, digits=0),'%')) %>%
  as.data.frame()
tot3_1

ggplot(data=tot3_1, aes(x=1, y=count, fill = member_casual)) + geom_bar(stat = 'identity', width = 1, color = 'black') + coord_polar('y', start = 0) + geom_text(aes(x=1, y=midpoint, label = label, size = 6)) + theme_void() + labs(title = 'Percentange of Total Ride Length: Annual vs. Casual', hjust = 1) + theme(plot.title = element_text(hjust =-3)) + scale_fill_discrete(name = 'Membership Type')
  
#Graphing the average ride length of casual vs. yearly riders by ride type
time_ridetype1<- Comb_BSC %>%
  group_by(member_casual, rideable_type) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean= round(mean, digits = 2)) %>%
  as.data.frame()
time_ridetype1

ggplot(data= time_ridetype1, aes(x=member_casual, y= mean, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x = 'Membership Type', title = 'Mean Ride Length of Annual vs. Casual Riders by Ride Type') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~rideable_type) + ylim(0,max(time_ridetype1$mean)*1.05) + scale_fill_discrete(name = 'Membership Type')

#Graphing the median ride length of casual vs. yearly riders by ride type
time_ridetype2<- Comb_BSC %>%
  group_by(member_casual, rideable_type) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median=round(median, digits = 2)) %>%
  as.data.frame()
time_ridetype2

ggplot(data= time_ridetype2, aes(x=member_casual, y= median, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x = 'Membership Type', title = 'Median Ride Length of Annual vs. Casual Riders by Ride Type') + theme(plot.title = element_text(hjust=0.25)) + facet_wrap(~rideable_type) + ylim(0,max(time_ridetype2$median)*1.05) + scale_fill_discrete(name = 'Membership Type')

#Graphing the total ride length of casual vs. yearly riders by ride type
time_ridetype3<-Comb_BSC %>%
  group_by(member_casual, rideable_type) %>%
  summarize(sum = scientific(sum(rl_min))) %>%
  as.data.frame()
time_ridetype3

ggplot(data = time_ridetype3, aes(x=member_casual, y = sum, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label = sum, vjust=-0.5), size = 3) + labs(y='Ride Length (Minutes)', x = 'Membership Type', title = 'Total Ride Length of Annual vs. Casual Riders') + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(~rideable_type) + scale_fill_discrete(name = 'Membership Type')


#Graphing the average ride length of casual vs. yearly riders by day of week
time_dow1<- Comb_BSC %>%
  group_by(member_casual, day_of_week) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean = (round(mean, digits = 2))) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%
  as.data.frame()
time_dow1

ggplot(data= time_dow1, aes(x=member_casual, y= mean, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Mean Ride Length of Annual vs. Casual Riders by Day of Week') + theme(plot.title = element_text(hjust=0.2)) + facet_wrap(~day_of_week) + ylim(0, max(time_dow1$mean)*1.2) + scale_fill_discrete(name = 'Membership Type')   

#Graphing the median ride length of casual vs. yearly riders by day of week
time_dow2<- Comb_BSC %>%
  group_by(member_casual, day_of_week) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median = (round(median, digits = 2))) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%
as.data.frame()
time_dow2

ggplot(data= time_dow2, aes(x=member_casual, y= median, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Median Ride Length of Annual vs. Casual Riders by Day of Week') + theme(plot.title = element_text(hjust=0.2)) + facet_wrap(~day_of_week) + ylim(0, max(time_dow2$median)*1.2) + scale_fill_discrete(name = 'Membership Type')   

#Graphing the total ride length of casual vs. yearly riders by DoW
time_dow3<- Comb_BSC %>%
  group_by(member_casual, day_of_week) %>%
  summarize(sum = (sum(rl_min))) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%
  as.data.frame()
time_dow3

ggplot(data= time_dow3, aes(x=member_casual, y= sum, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label= scientific(sum), vjust=-0.5), size = 3) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Total Ride Length of Annual vs. Casual Riders by Day of Week') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~day_of_week) + ylim(0, max(time_dow3$sum)*1.2)  + scale_fill_discrete(name = 'Membership Type') 

#Graphing the average ride length of casual vs. yearly riders by season
time_season1<- Comb_BSC %>%
  group_by(member_casual, Season) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean = (round(mean, digits = 2))) %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  as.data.frame()
time_season1

ggplot(data= time_season1, aes(x=member_casual, y= mean, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Mean Ride Length of Annual vs. Casual Riders by Season') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~Season) + ylim(0, max(time_season1$mean)*1.1) + scale_fill_discrete(name = 'Membership Type') 
ggplot(data= time_season1, aes(x=Season, y= mean, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 14)) + ylim(0, max(time_season1$mean)*1.2) + scale_fill_discrete(name = 'Membership Type') 


#Graphing the median ride length of casual vs. yearly riders by day of week
time_season2<- Comb_BSC %>%
  group_by(member_casual, Season) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median = (round(median, digits = 2))) %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  as.data.frame()
time_season2

ggplot(data= time_season2, aes(x=member_casual, y= median, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Median Ride Length of Annual vs. Casual Riders by Season') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~Season) + ylim(0, max(time_season2$median)*1.1)  + scale_fill_discrete(name = 'Membership Type') 

#Graphing the total ride length of casual vs. yearly riders by season
time_season3<- Comb_BSC %>%
  group_by(member_casual, Season) %>%
  summarize(sum = (sum(rl_min))) %>%
  as.data.frame()
time_season3

ggplot(data= time_season3, aes(x=member_casual, y= sum, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label= scientific(sum), vjust=-0.5)) + labs(y='Ride Length (Minutes)', x='Membership Type', title = 'Total Ride Length of Annual vs. Casual Riders by Season') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~Season) + ylim(0, max(time_season3$sum)*1.1)  + scale_fill_discrete(name = 'Membership Type') 

#Graphing the average ride length of casual vs. yearly riders by season
time_month1<- Comb_BSC %>%
  group_by(member_casual, `Month-Year`) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean = (round(mean, digits = 2))) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_month1

ggplot(data= time_month1, aes(x=member_casual, y= mean, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders by Month') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~`Month-Year`) + ylim(0, max(time_month1$mean)*1.2) + scale_fill_discrete(name = 'Membership Type') 
ggplot(data= time_month1, aes(x=`Month-Year`, y= mean, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=mean, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 14)) + ylim(0, max(time_month1$mean)*1.2) + scale_fill_discrete(name = 'Membership Type') 

#Graphing the average ride length of casual vs. yearly riders by season
time_month2<- Comb_BSC %>%
  group_by(member_casual, `Month-Year`) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median = (round(median, digits = 2))) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_month2

ggplot(data= time_month2, aes(x=member_casual, y= median, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Median Ride Length of Annual vs. Casual Riders by Month') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~`Month-Year`) + ylim(0, max(time_month2$median)*1.2)  
ggplot(data= time_month2, aes(x=`Month-Year`, y= median, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=median, vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Median Ride Length of Annual vs. Casual Riders by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 14)) + ylim(0, max(time_month2$median)*1.2) + scale_fill_discrete(name = 'Membership Type') 

#Graphing the total ride length of casual vs. yearly riders by month
time_month3<- Comb_BSC %>%
  group_by(member_casual, `Month-Year`) %>%
  summarize(sum = (sum(rl_min))) %>%
  as.data.frame()
time_month3

ggplot(data= time_month3, aes(x=`Month-Year`, y= (sum), fill = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Month-Year', title = 'Total Ride Length of Annual vs. Casual Riders by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12.5)) + ylim(0, max(time_month3$sum)*1.03)
ggplot(data= time_month3, aes(x=member_casual, y= sum, fill = member_casual)) + geom_bar(stat = 'identity') + geom_text(aes(label=scientific(sum), vjust=-0.5)) + labs(y='Ride Length (Minutes)', title = 'Total Ride Length of Annual vs. Casual Riders by Month') + theme(plot.title = element_text(hjust=0.3)) + facet_wrap(~`Month-Year`) + ylim(0, max(time_month3$sum)*1.05)  

#Graphing the average ride length of casual vs. yearly riders by ride type per Season
time_ridetype_season1<- Comb_BSC %>%
  group_by(rideable_type, member_casual, Season) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean=round(mean, digits = 1)) %>%
  as.data.frame()
time_ridetype_season1

ggplot(data= time_ridetype_season1, aes(x=rideable_type, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Mean Ride Length of Annual vs. Casual Riders by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12)) + facet_wrap(~Season) + ylim(0,max(time_ridetype_season1$mean)*1.12)


#Graphing the median ride length of casual vs. yearly riders by ride type per Season
time_ridetype_season2<- Comb_BSC %>%
  group_by(rideable_type, member_casual, Season) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median=round(median, digits = 1)) %>%
  as.data.frame()
time_ridetype_season2

ggplot(data= time_ridetype_season2, aes(x=rideable_type, y= median, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Median Ride Length of Annual vs. Casual Riders by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 11.5)) + facet_wrap(~Season) + ylim(0,max(time_ridetype_season2$median)*1.12)

#Graphing the total ride length of casual vs. yearly riders by ride type per Season
time_ridetype_season3<- Comb_BSC %>%
  group_by(rideable_type, member_casual, Season) %>%
  summarize(sum = (sum(rl_min))) %>%
  as.data.frame()
time_ridetype_season3

ggplot(data= time_ridetype_season3, aes(x=rideable_type, y= (sum), fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12.5)) + facet_wrap(~Season) + ylim(0, max(time_ridetype_season3$sum)*1.05)


#Graphing the average ride length of casual vs. yearly riders by ride type per Month
time_ridetype_month1<- Comb_BSC %>%
  group_by(rideable_type, member_casual, `Month-Year`) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean=round(mean, digits = 1)) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_ridetype_month1

ggplot(data= time_ridetype_month1, aes(x=rideable_type, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3)) + facet_wrap(~`Month-Year`) + ylim(0,max(time_ridetype_month1$mean)*1.12)


#Graphing the average ride length of casual vs. yearly riders by month per ride type
time_month_ridetype1<- Comb_BSC %>%
  group_by(rideable_type, member_casual, `Month-Year`) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean=round(mean, digits = 1)) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_month_ridetype1

ggplot(data= time_month_ridetype1, aes(x=`Month-Year`, y= mean, fill = member_casual, , group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12)) + facet_wrap(~rideable_type) 

#Graphing the median ride length of casual vs. yearly riders by month per ride type
time_month_ridetype2<- Comb_BSC %>%
  group_by(rideable_type, member_casual, `Month-Year`) %>%
  summarize(median = median(rl_min)) %>%
  mutate(median=round(median, digits = 1)) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_month_ridetype2

ggplot(data= time_month_ridetype2, aes(x=`Month-Year`, y= median, fill = member_casual, , group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', title = 'Median Ride Length of Annual vs. Casual Riders by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12)) + facet_wrap(~rideable_type) 

#Graphing the total ride length of casual vs. yearly riders by ride type per month
time_month_ridetype3<- Comb_BSC %>%
  group_by(rideable_type, member_casual, `Month-Year`) %>%
  summarize(sum = (sum(rl_min))) %>%
  as.data.frame()
time_month_ridetype3

ggplot(data= time_month_ridetype3, aes(x=`Month-Year`, y= (sum), fill = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12.5)) + facet_wrap(~rideable_type) + ylim(0, max(time_month_ridetype3$sum)*1.05)
ggplot(data= time_month_ridetype3, aes(x=rideable_type, y= (sum), fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12.5)) + facet_wrap(~`Month-Year`) + ylim(0, max(time_month_ridetype3$sum)*1.1)

#Graphing the average ride length of casual vs. yearly riders by ride type per day of week
time_ridetype_dow1<- Comb_BSC %>%
  group_by(rideable_type, member_casual, day_of_week) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate (mean=round(mean, digits = 1)) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%  
  as.data.frame()
time_ridetype_dow1

ggplot(data= time_ridetype_dow1, aes(x=rideable_type, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Mean Ride Length of Annual vs. Casual Riders by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 11.5)) + facet_wrap(~day_of_week) + ylim(0,max(time_ridetype_dow1$mean)*1.12)


#Graphing the median ride length of casual vs. yearly riders by ride type per day of week
time_ridetype_dow2<- Comb_BSC %>%
  group_by(rideable_type, member_casual, day_of_week) %>%
  summarize(median = median(rl_min)) %>%
  mutate (median=round(median, digits = 1)) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%  
  as.data.frame()
time_ridetype_dow2

ggplot(data= time_ridetype_dow2, aes(x=rideable_type, y= median, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Median Ride Length of Annual vs. Casual Riders by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 11.5)) + facet_wrap(~day_of_week) + ylim(0,max(time_ridetype_dow2$median)*1.12)

#Graphing the total ride length of casual vs. yearly riders by ride type per month
time_ridetype_dow3<- Comb_BSC %>%
  group_by(rideable_type, member_casual, day_of_week) %>%
  summarize(sum = (sum(rl_min))) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%  
  as.data.frame()
time_ridetype_dow3

ggplot(data= time_ridetype_dow3, aes(x=rideable_type, y= (sum), fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.35, size = 11.5)) + facet_wrap(~day_of_week) + ylim(0, max(time_ridetype_dow3$sum)*1.1) + scale_fill_discrete(name = 'Membership Type')


#Graphing the average ride length of casual vs. yearly riders by day of week per month
time_dow_month1<- Comb_BSC %>%
  group_by(day_of_week, member_casual, `Month-Year`) %>%
  summarize(mean = mean(rl_min)) %>%
  mutate(mean=round(mean)) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame() %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) 
time_dow_month1

ggplot(data= time_dow_month1, aes(x=day_of_week, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x = 'Day of Week', title = 'Mean Ride Length of Annual vs. Casual Riders by Day of Week Per Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 12)) + facet_wrap(~`Month-Year`) + ylim(0,max(time_dow_month1$mean)*1.08) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_dow_month1, aes(x=`Month-Year`, y= mean, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color= member_casual)) + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x = 'Day of Week', title = 'Mean Ride Length of Annual vs. Casual Riders by Day of Week Per Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 12)) + facet_wrap(~day_of_week) + ylim(0,max(time_dow_month1$mean)*1.08) 

#Graphing the median ride length of casual vs. yearly riders by day of week per month
time_dow_month2<- Comb_BSC %>%
  group_by(day_of_week, member_casual, `Month-Year`) %>%
  summarize(median = median(rl_min)) %>%
  mutate(median=round(median)) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame() %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) 
time_dow_month2

ggplot(data= time_dow_month2, aes(x=`Month-Year`, y= median, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color= member_casual)) + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x = 'Day of Week', title = 'Median Ride Length of Annual vs. Casual Riders by Day of Week Per Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 12)) + facet_wrap(~day_of_week) + ylim(0,max(time_dow_month2$median)*1.08) 

#Graphing the total ride length of casual vs. yearly riders by day of week per month
time_dow_month3<- Comb_BSC %>%
  group_by(day_of_week, member_casual, `Month-Year`) %>%
  summarize(sum = (sum(rl_min))) %>%
  as.data.frame() %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) 
time_dow_month3

ggplot(data= time_dow_month3, aes(x=`Month-Year`, y= sum, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color= member_casual))  + labs(y='Ride Length (Minutes)', x = 'Day of Week', title = 'Total Ride Length of Annual vs. Casual Riders by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 12)) + facet_wrap(~day_of_week) + ylim(0,max(time_dow_month3$sum)*1.08) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_dow_month3, aes(x=`Month-Year`, y= (sum), fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Day of Week', title = 'Total Ride Length of Annual vs. Casual Riders by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 12.5)) + facet_wrap(~day_of_week) + ylim(0, max(time_dow_month3$sum)*1.1) + scale_fill_discrete(name = 'Membership Type')


#Graphing the frequency of casual vs yearly member's ride length 
time_x1<- Comb_BSC %>%
  group_by(rl_min, member_casual) %>%
  summarize(count = n()) %>%
  as.data.frame()
time_x1

ggplot(data=time_x1, (aes(x=rl_min, y= count, color = member_casual, log = 'x'))) + geom_point() + scale_x_log10(breaks = trans_breaks('log10', function(x)10^x), labels = trans_format('log10', function(x)10^x)) + labs(x= 'Ride Length (Minutes)', y='Number of Rides', title = 'Annual vs. Casual Riders By Ride Length') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + geom_jitter() + geom_smooth() 
ggplot(data=time_x1, (aes(x=rl_min, y= count, color = member_casual, log = 'x'))) + geom_point() + scale_x_log10(breaks = trans_breaks('log10', function(x)10^x), labels = trans_format('log10', function(x)10^x)) + labs(x= 'Ride Length (Minutes)', y='Number of Rides', title = 'Annual vs. Casual Riders By Ride Length') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + geom_jitter() + geom_smooth() 

#Segmenting customers by ride length, 60 minutes
time_x60<-Comb_BSC %>%
  group_by(member_casual, LG60) %>%
  summarize(ride_sum = round(sum(rl_min)),
            median = median(rl_min),
            mean = round(mean(rl_min)),
            ride_id = n()) %>%
  as.data.frame()
time_x60

ggplot(data=time_x60, (aes(x=member_casual, y= ride_sum, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_sum)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Total Length of Rides Less than or Greater Than or Equal to 60 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG60) + ylim(0, max(time_x60$ride_sum)*1.05) + scale_fill_discrete(name = 'Membership Type') 
ggplot(data=time_x60, (aes(x=member_casual, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Number of Rides Less than or Greater Than or Equal to 60 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG60) + scale_fill_discrete(name = 'Membership Type') + ylim(0,max(data=time_x60$ride_id)*1.03)
ggplot(data=time_x60, (aes(x=member_casual, y= mean, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(mean)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Ride Length (Minutes)', title = 'Average Ride Length Less Than on Greater Than and Equal to 60 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~LG60) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x60, (aes(x=member_casual, y= median, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(median)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Membership Type', y='Ride Length (Minutes)', title = 'Median Ride Length Less Than on Greater Than and Equal to 60 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5, size = 12))  + facet_wrap(~LG60) + scale_fill_discrete(name = 'Membership Type')

#Segmenting customers by ride length, 90 minutes
time_x90<-Comb_BSC %>%
  group_by(member_casual, LG90) %>%
  summarize(ride_sum = round(sum(rl_min)),
            median = median(rl_min),
            mean = round(mean(rl_min)),
            ride_id = n()) %>%
  as.data.frame()
time_x90

ggplot(data=time_x90, (aes(x=member_casual, y= ride_sum, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_sum)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Total Length of Rides Less than or Greater Than or Equal to 90 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG90) + ylim(0, max(time_x90$ride_sum)*1.05) + scale_fill_discrete(name = 'Membership Type') 
ggplot(data=time_x90, (aes(x=member_casual, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Number of Rides Less than or Greater Than or Equal to 90 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG90) + scale_fill_discrete(name = 'Membership Type') + ylim(0,max(data=time_x90$ride_id)*1.03)
ggplot(data=time_x90, (aes(x=member_casual, y= mean, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(mean)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Ride Length (Minutes)', title = 'Average Ride Length Less Than on Greater Than and Equal to 90 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~LG90) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x90, (aes(x=member_casual, y= median, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(median)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Ride Length (Minutes)', title = 'Median Ride Length Less Than on Greater Than and Equal to 90 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~LG90) + scale_fill_discrete(name = 'Membership Type')

#Segmenting customers by ride length, 120 minutes
time_x2<-  Comb_BSC %>%
  group_by(member_casual, LG120) %>%
  summarize(ride_sum = round(sum(rl_min)),
            median = median(rl_min),
            mean = round(mean(rl_min)),
            ride_id = n()) %>%
  as.data.frame()
time_x2

ggplot(data=time_x2, (aes(x=member_casual, y= mean, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(mean)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x = 'Membership Type', y='Ride Length (Minutes)', title = 'Mean Ride Length Less Than or Greater Than and Equal to 120 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5, size = 12))  + facet_wrap(~LG120) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x2, (aes(x=member_casual, y= median, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(median)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x = 'Membership Type', y='Ride Length (Minutes)', title = 'Median Ride Length Less Than or Greater Than and Equal to 120 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5, size = 12))  + facet_wrap(~LG120) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x2, (aes(x=member_casual, y= ride_sum, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_sum)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Total Length of Rides Less than or Greater Than or Equal to 120 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG120) + ylim(0, max(time_x2$ride_sum)*1.05) + scale_fill_discrete(name = 'Membership Type') 
ggplot(data=time_x2, (aes(x=member_casual, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Number of Rides Less than or Greater Than or Equal to 120 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.3, size = 12)) + facet_wrap(~LG120) + scale_fill_discrete(name = 'Membership Type') + ylim(0,max(data=time_x2$ride_id)*1.03)


#Segmenting customers by ride length, 180 minutes
time_x3<-  Comb_BSC %>%
  group_by(member_casual, LG180) %>%
  summarize(ride_sum = round(sum(rl_min)),
            median = round(median(rl_min)),
            mean = round(mean(rl_min)),
            ride_id = n()) %>%
  as.data.frame()
time_x3

ggplot(data=time_x3, (aes(x=member_casual, y= mean, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(mean)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Ride Length (Minutes)', title = 'Average Ride Length Less Than on Greater Than and Equal to 180 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~LG180) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x3, (aes(x=member_casual, y= median, fill = member_casual, log = 'y'))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(median)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + scale_y_log10(breaks = trans_breaks('log10', function(y)10^y), labels = trans_format('log10', function(y)10^y)) + labs(x= 'Casual vs. Member', y='Ride Length (Minutes)', title = 'Median Ride Length Less Than on Greater Than and Equal to 180 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))  + facet_wrap(~LG180) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x3, (aes(x=member_casual, y= ride_sum, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_sum)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Ride Length of Rides Less than or Greater Than or Equal to 180 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~LG180) + ylim(0,max(time_x3$ride_sum)*1.05) + scale_fill_discrete(name = 'Membership Type')
ggplot(data=time_x3, (aes(x=member_casual, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + labs(y= 'Number of Rides', x = 'Membership Type', title = 'Number of Rides Less than or Greater Than or Equal to 180 Minutes') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~LG180) + scale_fill_discrete(name = 'Membership Type')

#Figuring out whether greater than 180 minutes members are converting to annual membership
time_x4<-  Comb_BSC %>%
  group_by(member_casual, LG180, `Month-Year`) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Less') %>%
  as.data.frame()
time_x4 

ggplot(data=time_x4, (aes(x=`Month-Year`, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +facet_wrap(~member_casual) + ylim(0, max(time_x4$ride_id)*1.03)

#Number of Rides >180min by Month
time_x5<-  Comb_BSC %>%
  group_by(member_casual, LG180, `Month-Year`) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Greater') %>%
  as.data.frame()
time_x5 

ggplot(data=time_x5, (aes(x=`Month-Year`, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5)  + scale_x_date(date_breaks = '1 month' , date_labels = '%b %Y') + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +facet_wrap(~member_casual) + ylim(0, max(time_x5$ride_id)*1.03)

#Number of Rides >180min by season
time_x6<-  Comb_BSC %>%
  group_by(member_casual, LG180, Season) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Greater') %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  as.data.frame()
time_x6 

ggplot(data=time_x6, (aes(x=Season, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +facet_wrap(~member_casual) + ylim(0, max(time_x6$ride_id)*1.03)

#Number of Rides <180min by season
time_x7<-  Comb_BSC %>%
  group_by(member_casual, LG180, Season) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Less') %>%
  mutate(across(Season, factor, levels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  as.data.frame()
time_x7 

ggplot(data=time_x7, (aes(x=Season, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +facet_wrap(~member_casual) + ylim(0, max(time_x7$ride_id)*1.03)

#Number of Rides >180min by Dow
time_x8<-  Comb_BSC %>%
  group_by(member_casual, LG180, day_of_week) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Greater') %>%
  mutate(across (day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')))
  as.data.frame()
time_x8 

ggplot(data=time_x8, (aes(x=day_of_week, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) +facet_wrap(~member_casual) + ylim(0, max(time_x8$ride_id)*1.03)

#Number of Rides >180min by Dow
time_x9<-  Comb_BSC %>%
  group_by(member_casual, LG180, day_of_week) %>%
  summarize(ride_id = n()) %>%
  filter(LG180 == 'Less') %>%
  mutate(across (day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')))
as.data.frame()
time_x9 

ggplot(data=time_x9, (aes(x=day_of_week, y= ride_id, fill = member_casual))) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label = paste0(ride_id)), position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y= 'Number of Rides', title = 'Number of Rides Less than 180 Minutes by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5)) + facet_wrap(~member_casual) + coord_cartesian(ylim=c(200000,525000))

#Comparing mean, median, total ride length, and number of rides by ride type and month for Less than 120 Minutes
time_x120_month_ridetype1<- Comb_BSC %>%
  group_by(LG120, rideable_type, member_casual, `Month-Year`) %>%
  filter(LG120 == 'Less') %>%
  summarize(mean = round(mean(rl_min), digits = 1),
            median = round(median(rl_min), digits = 1),
            ride_sum = sum(rl_min),
            ride_id = n()) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame()
time_x120_month_ridetype1

ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= mean, fill = member_casual, group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', title = 'Mean Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~rideable_type) 
ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= median, fill = member_casual, group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', title = 'Median Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~rideable_type) 
ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= ride_sum, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=scientific(ride_sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10.5)) + facet_wrap(~rideable_type) + ylim(0, max(time_x120_month_ridetype1$ride_sum)*1.05)
ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= ride_id, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=scientific(ride_id, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Number of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10.5)) + facet_wrap(~rideable_type) + ylim(0, max(time_x120_month_ridetype1$ride_id)*1.05)
ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= ride_sum, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10.5)) + facet_wrap(~rideable_type) + ylim(0, max(time_x120_month_ridetype1$ride_sum)*1.05)
ggplot(data= time_x120_month_ridetype1, aes(x=`Month-Year`, y= ride_id, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Number of Annual vs. Casual Riders Minutes<120 by Ride Type by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10.5)) + facet_wrap(~rideable_type) + ylim(0, max(time_x120_month_ridetype1$ride_id)*1.05)

#Comparing mean, median, total ride length, and number of rides by ride type and season for Less than 120 Minutes
time_x120_season_ridetype1<- Comb_BSC %>%
  group_by(LG120, rideable_type, member_casual, Season) %>%
  filter(LG120 == 'Less') %>%
  summarize(mean = round(mean(rl_min), digits = 1),
            median = round(median(rl_min), digits = 1),
            ride_sum = sum(rl_min),
            ride_id = n()) %>%
  as.data.frame()
time_x120_season_ridetype1

ggplot(data= time_x120_season_ridetype1, aes(x=rideable_type, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Mean Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~Season) + ylim(0,max(time_x120_season_ridetype1$mean)*1.12) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_season_ridetype1, aes(x=rideable_type, y= median, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Median Ride Length of Annual vs. Casual Riders by Minutes<120 Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 10)) + facet_wrap(~Season) + ylim(0,max(time_x120_season_ridetype1$median)*1.12) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_season_ridetype1, aes(x=rideable_type, y= ride_sum, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(ride_sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~Season) + ylim(0, max(time_x120_season_ridetype1$ride_sum)*1.05) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_season_ridetype1, aes(x=rideable_type, y= ride_id, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(ride_id, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Number of Annual vs. Casual Riders Minutes<120 by Ride Type by Season') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~Season) + ylim(0, max(time_x120_season_ridetype1$ride_id)*1.05) + scale_fill_discrete(name = 'Membership Type')


#Graphing the average ride length of casual vs. yearly riders by ride type per day of week
time_x120_dow_ridetype1<- Comb_BSC %>%
  group_by(LG120, rideable_type, member_casual, day_of_week) %>%
  filter(LG120 == 'Less') %>%
  summarize(mean = round(mean(rl_min), digits = 1),
            median = round(median(rl_min), digits = 1),
            ride_sum = sum(rl_min),
            ride_id = n()) %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) %>%  
  as.data.frame()
time_x120_dow_ridetype1

ggplot(data= time_x120_dow_ridetype1, aes(x=rideable_type, y= mean, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2.5) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Mean Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 10)) + facet_wrap(~day_of_week) + ylim(0,max(time_x120_dow_ridetype1$mean)*1.2) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_dow_ridetype1, aes(x=rideable_type, y= median, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 3) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Median Ride Length of Annual vs. Casual Riders by Minutes<120 Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.22, size = 10)) + facet_wrap(~day_of_week) + ylim(0,max(time_x120_dow_ridetype1$median)*1.2) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_dow_ridetype1, aes(x=rideable_type, y= ride_sum, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(ride_sum, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Length of Annual vs. Casual Riders Minutes<120 by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_ridetype1$ride_sum)*1.2) + scale_fill_discrete(name = 'Membership Type')
ggplot(data= time_x120_dow_ridetype1, aes(x=rideable_type, y= ride_id, fill = member_casual)) + geom_bar(stat = 'identity', position = 'dodge') + geom_text(aes(label=scientific(ride_id, digits = 2)),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size = 2) + labs(y='Ride Length (Minutes)', x= 'Ride Type', title = 'Total Ride Number of Annual vs. Casual Riders Minutes<120 by Ride Type by Day of Week') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_ridetype1$ride_id)*1.2) + scale_fill_discrete(name = 'Membership Type')



#Graphing the average ride length of casual vs. yearly riders by day of week per month
time_x120_dow_month1<- Comb_BSC %>%
  group_by(LG120, day_of_week, member_casual, `Month-Year`) %>%
  filter(LG120 == 'Less') %>%
  summarize(mean = round(mean(rl_min), digits = 1),
            median = round(median(rl_min), digits = 1),
            ride_sum = sum(rl_min),
            ride_id = n()) %>%
  mutate(`Month-Year` = case_when(`Month-Year` == '2020-08-1' ~ 'August 2020', `Month-Year` == '2020-09-1' ~ 'September 2020', `Month-Year` == '2020-10-01' ~ 'October 2020', `Month-Year` == '2020-11-01' ~ 'November 2020', `Month-Year` == '2020-12-01' ~ 'December 2020', `Month-Year` == '2021-01-01' ~ 'January 2021', `Month-Year` == '2021-02-01' ~ 'February 2021', `Month-Year` == '2021-03-01' ~ 'March 2021', `Month-Year` == '2021-04-01' ~ 'April 2021', `Month-Year` == '2021-05-01' ~ 'May 2021', `Month-Year` == '2021-06-01' ~ 'June 2021', `Month-Year` == '2021-07-01' ~ 'July 2021')) %>%
  mutate(across(`Month-Year`, factor, levels = c('August 2020', 'September 2020', 'October 2020', 'November 2020', 'December 2020', 'January 2021', 'February 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021' ))) %>%
  as.data.frame() %>%
  mutate(across(day_of_week, factor, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))) 
time_x120_dow_month1

ggplot(data= time_x120_dow_month1, aes(x=`Month-Year`, y= mean, fill = member_casual, group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=mean),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x = 'Month-Year', title = 'Mean Ride Length of Annual vs. Casual Riders Minutes<120 by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.2, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_month1$mean)*1.15)
ggplot(data= time_x120_dow_month1, aes(x=`Month-Year`, y= median, fill = member_casual, group= member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + geom_text(aes(label=median),  position = position_dodge(width = 0.9), vjust=-0.5, hjust = 0.5, size=2) + labs(y='Ride Length (Minutes)', x = 'Month-Year', title = 'Median Ride Length of Annual vs. Casual Riders Minutes<120 by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_month1$median)*1.15)
ggplot(data= time_x120_dow_month1, aes(x=`Month-Year`, y= ride_sum, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + labs(y='Ride Length (Minutes)', x= 'Month-Year', title = 'Total Ride Length of Annual vs. Casual Riders Minutes<120 by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_month1$ride_sum)*1.05)
ggplot(data= time_x120_dow_month1, aes(x=`Month-Year`, y= ride_id, group = member_casual)) + geom_line(aes(color=member_casual)) + geom_point(aes(color = member_casual)) + labs(y='Ride Length (Minutes)', x= 'Month-Year', title = 'Total Ride Number of Annual vs. Casual Riders Minutes<120 by Day of Week by Month') + theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust=0.3, size = 10)) + facet_wrap(~day_of_week) + ylim(0, max(time_x120_dow_month1$ride_id)*1.05)
