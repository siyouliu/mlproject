
#------------------------set the directory--------------------------------------
setwd("~/Downloads/master_student_phone_use_data")
#------------------------Load packages -----------------------------------------

install.packages("chron")
install.packages('ggcorrplot')
install.packages("FactoMineR")
install.packages('factoextra')
install.packages('corrplot')

library(dplyr)
library(ggplot2)
library(lubridate)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(reshape2)
library(data.table)
library(corrplot)

#---------------------------Load data ------------------------------------------
mood <-unique(read.delim("mood_sampling_data.csv",sep = ",", stringsAsFactors = FALSE))
phone <- unique(read.delim("phone_use_data.csv",sep = ",", stringsAsFactors = FALSE))
app_category <- unique(read.delim("app_categories.csv",sep = ",", stringsAsFactors = FALSE))

#------------------Preprocessing + feature engineering--------------------------
#1. Predictor
##1.1 set the categories for all application
app_category$categories <- 
  as.factor(case_when(app_category$better_category %in% c('Food_&_Drinks', 'Music_&_Audio', 'Dating', 'Personal_Fitness', 'Sports') ~ "lifestyle",
                      app_category$better_category %in% c('Social_Networking') ~ "social network",
                      app_category$better_category %in% c('Email', 'Messages', 'Messaging', 'Instant_Messaging') ~ "communication",
                      app_category$better_category %in% c('Background_Process','Camera','Dialer','Phone','Phone_Assistant' ,'Phone_Optimization', 'Phone_Personalization' ,'Phone_Tools' ,'Auto_&_Vehicles', 'Book_Readers' ,'Calendar', 'Coupons' ,'Document_Editor' ,'Drawing' ,'Time_Tracker' ,'To_Do_List' ,'Travel_Planning','Video_Players_&_Editors', 'Wearables', 'Weather', 'Remote_Administration', 'Security', 'Portfolio/Trading', 'Home_Automation', 'House_Search', 'Internet_Browser', 'Maps', 'Job_Search', 'Business_Management', 'Personal_Finance', 'Medical', 'Family_Planning', 'Mechanical_Turk', 'online_Shopping') ~ "utility & tools",
                      app_category$better_category %in% c('Game_Multiplayer', 'Game_Singleplayer', 'Entertainment','Streaming_Services') ~ "games & entertainment",
                      app_category$better_category %in% c('News','Education') ~ 'news & information outlet'))

app_category <- app_category %>%
  select(app_id,categories) %>%
  rename(application = app_id)

#1.2 match phone dataset with app category:
phone_cleaned <- inner_join(phone, app_category)
phone_cleaned <- select(phone_cleaned,application,endTime,startTime,user_id,categories)
phone_cleaned <- phone_cleaned %>%
  na.omit(application,endTime,startTime,user_id,categories) 

#2.2 create app usage features

# duration
phone_cleaned$date <- as.Date(phone_cleaned$startTime)
phone_cleaned <- phone_cleaned %>%
  mutate_at(vars(startTime, endTime), ~as.POSIXct(strptime(.x, format = c("%Y-%m-%dT%H:%M:%OS")))) %>%
  mutate(duration_time = endTime - startTime) 

# 2.3 Recency

rfm_recency<- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(recency=difftime(lubridate::ceiling_date(max(endTime), unit='day'), max(endTime), units='secs'))

# 2.4 starting and end seconds

phone_cleaned$endTime <- strftime(phone_cleaned$endTime , format="%H:%M:%S")
phone_cleaned$startTime <- strftime(phone_cleaned$startTime , format="%H:%M:%S")
phone_cleaned$end_second <- period_to_seconds(hms(phone_cleaned$endTime))
phone_cleaned$start_second  <-period_to_seconds(hms(phone_cleaned$startTime))


# 2.5 frequency & proportion

daily_duration <- phone_cleaned %>%
  group_by(user_id,date) %>%
  summarise(daily_duration= sum(duration_time), daily_access = n())

category_duration <- phone_cleaned %>%
  group_by(user_id,date,categories) %>% 
  summarise(frequency= n(), cat_duration= sum(duration_time)) %>%
  left_join(daily_duration, by = c("user_id", "date")) %>% 
  mutate(dur_pro = as.numeric(cat_duration, units = "secs") / as.numeric(daily_duration, units = "secs"),
         freq_pro = frequency/daily_access)

#2.6 earliest time
earliest_time <- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(earliest_time = min(start_second))


# 2.7 monetary

rfm_monetary<- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(monetary= mean(duration_time)) 

#2.8 max/min/std,variance duration time

max_duration <- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(max_duration = max(duration_time))

min_duration <- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(min_duration = min(duration_time))

std_duration <- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(std_duration = sd(duration_time))

variance_duration <- phone_cleaned %>% 
  group_by(user_id, date, categories) %>% 
  summarise(var_duration = var(duration_time))

max_duration$date <-as.character(max_duration$date)
min_duration$date <-as.character(min_duration$date)
std_duration$date <-as.character(std_duration$date)
variance_duration$date <-as.character(variance_duration$date)

##  put all predictors in one dataframe;

category_duration$date <-as.character(category_duration$date)
category_duration <-category_duration %>%
  select(user_id, date, categories, frequency, cat_duration, dur_pro, freq_pro)

rfm_recency$date <-as.character(rfm_recency$date)
rfm_monetary$date <-as.character(rfm_monetary$date)
earliest_time$date <-as.character(earliest_time$date)


phone_usage <- merge(merge(merge(merge(merge(merge(merge(category_duration, rfm_recency),rfm_monetary),earliest_time),variance_duration),std_duration),min_duration),max_duration)


## 5.4 cast it into wide format and fill with 0;

phone_usage_cast <- dcast(setDT(phone_usage), user_id + date ~ categories, value.var = c(names(phone_usage[,4:10])))

phone_usage_cast[is.na(phone_usage_cast)] <- 0


##5.target variable

mood$duration <- as.numeric(mood$duration) 

mood_cleaned <- mood %>%
  na.omit(anxious,bored,gloomy,stressed,tired,upset,envious,inferior) %>% 
select(user_id,response_time,anxious,bored,gloomy,stressed,tired,upset,envious,inferior) %>% 
filter(anxious<=5 & bored<=5 & gloomy<=5 & stressed<=5 & tired<=5 &upset<=5 &envious<=5 & inferior<=5)%>%
  rename(date = response_time)
mood_cleaned$tired <- as.numeric(mood_cleaned$tired)
mood_cleaned$date <- as.Date(mood_cleaned$date)  

## caculate daily mood:

anxious <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(anxious=(mean(anxious)))
bored <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(bored=(mean(bored)))
gloomy<- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(gloomy=(mean(gloomy)))
stressed <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(stressed=(mean(stressed)))
upset <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(upset=(mean(upset)))
tired <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(tired=(mean(tired)))
envious <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(envious=(mean(envious)))
inferior <- mood_cleaned %>%
  group_by(user_id,date) %>%
  summarise(inferior=(mean(inferior)))

mood_cleaned<- merge(merge(merge(merge(merge(merge(merge(anxious, bored),gloomy),stressed),upset),tired),envious),inferior)
rm(anxious,bored,gloomy,stressed,upset,tired,envious,inferior)


## match phone and mood dataset  
mood_cleaned <- mood_cleaned[(mood_cleaned$user_id %in% phone_usage_cast$user_id),]



## 5.5 join mood and phone usage
phone_usage_cast$date <- as.character(phone_usage_cast$date)
mood_cleaned$date <- as.character(mood_cleaned$date)
final_data <- inner_join(phone_usage_cast,mood_cleaned, by = c(user_id = "user_id", date= "date")) 

## 5.7 write into file

write.csv(final_data, file = 'final_data.csv')


##6.target: PCA negative mood --------------------------------------------------


#6.1 EDA
mood_test <- final_data[45:52]
library(ggcorrplot)
ggcorrplot(cor(mood_test))
cor(mood_test)

##6.2 PCA

mood_pca <- PCA(mood_test)
mood_pca$eig
fviz_eig(mood_pca, addlabels = TRUE, ylim = c(0, 100))
mood_pca$var$cos2[, 0:3]
corrplot(mood_pca$var$cos2)

############################end of the script###################################

