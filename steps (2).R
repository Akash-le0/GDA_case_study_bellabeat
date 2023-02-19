daily_activity <- read.csv(file="/cloud/project/dailyActivity_merged - dailyActivity_merged.csv")
daily_cals <- read.csv(file="/cloud/project/dailyCalories_merged - dailyCalories_merged.csv")
daily_Intensity <- read.csv(file="/cloud/project/dailyIntensities_merged - dailyIntensities_merged.csv")
daily_Steps <- read.csv(file="/cloud/project/dailySteps_merged - dailySteps_merged.csv")
daily_sleep <- read.csv(file="/cloud/project/dailysleep_merged - dailysleep_merged.csv")
daily_sleep_avg_idletime <- read.csv(file="/cloud/project/dailysleep_merged - Average idle time in bed.csv")

str(daily_activity)
head(daily_activity)
head(daily_cals)
head(daily_Intensity)
head(daily_Steps)
head(daily_sleep)
head(daily_sleep_avg_idletime)

install.packages('tidyverse')
library(tidyverse)

length(unique(daily_activity $Id))
length(unique(daily_cals $Id))
length(unique(daily_Intensity $Id))
length(unique(daily_sleep $Id))
length(unique(daily_Steps $Id))

sum(duplicated(daily_activity))
sum(duplicated(daily_cals))
sum(duplicated(daily_Intensity))
sum(duplicated(daily_sleep))
sum(duplicated(daily_sleep_avg_idletime))
sum(duplicated(daily_Steps))

daily_sleep <- daily_sleep %>% distinct() %>% drop_na()

install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages('ggplot2')

library(janitor)
library(skimr)
library(here)
library(ggplot2)


clean_names(daily_activity)
daily_activity<- rename_with(daily_activity, tolower)

clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)

clean_names(daily_Intensity)
daily_Intensity <- rename_with(daily_Intensity, tolower)

clean_names(daily_Steps)
hourly_steps <- rename_with(daily_Steps, tolower)

daily_sleep_avg_idletime = daily_sleep_avg_idletime[-1, ]
head(daily_sleep_avg_idletime)
colnames(daily_sleep_avg_idletime) <- c ("id","Average")

daily_sleep_final = select(daily_sleep, -c(time,am))
head(daily_sleep_final)

colnames(daily_sleep_final) [colnames(daily_sleep_final) == "sleepday"] <- "date"
head(daily_sleep_final)

colnames(daily_activity) [colnames(daily_activity) == "activitydate"] <- "date"
head(daily_activity)

merged_daily_records_of_bellabeat <- merge (daily_activity, daily_sleep_final, by=c("id"))
head(merged_daily_records_of_bellabeat)

merged_daily_records_1 <- merge (daily_activity, daily_sleep_final, by=c("id","date"))
head(merged_daily_records_1)
colnames(merged_daily_records_1)
write.csv(merged_daily_records_1,'merged_daily_records_1.csv')

install.packages('ggplot2')
library(ggplot2)

summary(merged_daily_records_1)

daily_activity$date <- format(daily_activity$date, format = "%m/%d/%y")
str(daily_activity)
head(daily_activity)

#grouping_users_based_on_steps

daily_steps_average <- merged_daily_records_1 %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))

head(daily_steps_average)

daily_steps_average_final <- daily_steps_average %>% 
  mutate(user_type = case_when( mean_daily_steps < 3000 ~ "less_active",
                                mean_daily_steps >= 3000 & mean_daily_steps < 5999 ~ "fairly_active", 
                                mean_daily_steps >= 6000 & mean_daily_steps < 9999 ~ "active",
                                mean_daily_steps >= 10000 ~ "very_active"))
head(daily_steps_average_final)

user_type_percent <- daily_steps_average_final %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type , levels = c("very_active", "active","fairly_active", "less_active"))
head(user_type_percent)

user_type_percent %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = scales::percent(round(total_percent,2))), position = position_stack(vjust = 0.5))+
  labs(title='User_classification_based_on_steps')

head(merged_daily_records_1)


# relationship_between_no_of_steps&calories_burnt

ggplot(merged_daily_records_1, aes(x=totalsteps, y=calories))+
  geom_jitter() +
  geom_smooth(color = "blue") + 
  labs(title = "Daily steps vs Calories burnt", x = "Daily steps", y= "Calories burnt")

#users_actualsleeptime_vs_actual_timeinbed

head(daily_sleep)

daily_act_sleep <- daily_sleep_final %>%
  mutate(date = as.Date(date,format ="%m/%d/%Y" , tz=Sys.timezone()))

head(daily_act_sleep)
str(daily_sleep_final)

das <- daily_sleep_final %>% 
  daily_sleep_final$date <- as.Date(daily_sleep_final$date)
daily_act_sleep <- daily_sleep_final %>%
  mutate(weekday = weekdays(date))
daily_act_sleep$weekday <-ordered(daily_act_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                         "Friday", "Saturday", "Sunday"))

head(daily_act_sleep)
colnames(daily_act_sleep) <- c("id","date","totalsleeprecords","totminsasleep","tottimeinbed","idletimeinbed","weekday")

daily_act_sleep_final <-daily_act_sleep%>%
  group_by(weekday) %>%
  summarize (idle_time_in_bed = mean(idletimeinbed),minsasleep = mean(totminsasleep))

ggplot(data=daily_act_sleep_final)+
  geom_col(aes(weekday,idle_time_in_bed,fill=weekday))
  labs(title="average idle time in bed" , x= " ", y=" ")

head(daily_act_sleep_final)

daily_act_sleep_final <- daily_act_sleep_final %>% 
  mutate(hrs_asleep = minsasleep/60)

#ggp <- ggplot(data=daily_act_sleep_final)+
  geom_col(aes(weekday,hrs_asleep,fill=weekday))
labs(title="average sleep per day" , x= " ", y=" ")

#ggp + ylim(0,8)

rm(daily_act_sleep_fina_1)

head(daily_act_sleep_final)

head(daily_steps_average)


# correlation_between_sleep_&_total_distance

str(merged_daily_records_1)

ggplot(merged_daily_records_1, aes(x=totalminutesasleep, y=totaldistance))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Daily distance vs Minutes asleep", y = "total distance", x= " minutes asleep")

#frequency of steps of day

str(merged_daily_records_1)

merged_daily_records_2 <- merged_daily_records_2 %>%
    mutate(weekday = weekdays(date))
merged_daily_records_2$weekday <-ordered(merged_daily_records_2$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                      "Friday", "Saturday", "Sunday"))
str(merged_daily_records_2)  
write.csv(merged_daily_records_2,'merged_daily_records_2.csv')

ggp <- ggplot(data=merged_daily_records_2)+
  geom_col(aes(weekday,totaldistance,fill=weekday))
labs(title="average steps per day" , x= " ", y=" ")


merged_daily_records_3 <- merged_daily_records_2 %>%
  group_by(weekday) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_dist = mean(totaldistance))

head(merged_daily_records_3)


ggplot(data=merged_daily_records_3)+
  geom_col(aes(weekday,mean_daily_steps,fill=weekday))
labs(title="average steps per day" , x= " ", y=" ")

ggplot(data=merged_daily_records_3)+
  geom_col(aes(weekday,mean_daily_dist,fill=weekday))
labs(title="average dist per day" , x= " ", y=" ")


ggp <- ggplot(data=merged_daily_records_3)+
  geom_col(aes(weekday,mean_daily_calories,fill=weekday))
labs(title="average calories per day" , x= " ", y=" ")

ggp + ylim(0,3000)

head(daily_activity)
head(merged_daily_records_2)

# number of hrs users actually wear the bellabeat trackers
merged_daily_records_4 <- daily_activity

merged_daily_records_4$totalusage <- merged_daily_records_4$SedentaryMinutes + merged_daily_records_4$LightlyActiveMinutes + merged_daily_records_4$FairlyActiveMinutes + merged_daily_records_4$VeryActiveMinutes

merged_daily_records_4$totalusageinhrs <- merged_daily_records_4$totalusage/60

merged_daily_records_4$totalusageinhrs <-round(merged_daily_records_4$totalusageinhrs , digit=1)

merged_daily_records_4_sum <- merged_daily_records_4 %>%
  group_by(Id) %>%
  summarise (mean_total_hr = mean(totalusageinhrs))

merged_daily_records_4_sum <- merged_daily_records_4_sum %>% 
  mutate(user_type = case_when( mean_total_hr >=0 & mean_total_hr <16 ~ "work time user",
                                mean_total_hr >=16 & mean_total_hr <=20  ~ "very active user",
                                mean_total_hr > 20 & mean_total_hr <=24 ~ "all day user"))
   
classification_duration <- merged_daily_records_4_sum %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

classification_duration$user_type <- factor(classification_duration$user_type , levels = c("all day user","very active user","work time user"))
head(user_type_percent)

classification_duration %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = scales::percent(round(total_percent,2))), position = position_stack(vjust = 0.5))+
  labs(title='User category based on clocked in duration')

head(merged_daily_records_4_sum)

str(merged_daily_records_4_sum)

#BMI of users

user_weight <- read.csv(file="/cloud/project/weightLogInfo_merged.csv")
clean_names(user_weight)
user_weight<- rename_with(user_weight, tolower)
head(user_weight)

user_weight_percent <- user_weight %>%
  group_by(id) %>%
  summarise (mean_bmi = mean(bmi))
head(user_weight_percent)

user_weight_percent <- user_weight_percent %>% 
  mutate(user_type = case_when( mean_bmi < 18.5 ~ "Underweight",
                                mean_bmi >= 18.5 & mean_bmi <= 24.9 ~ "NormalWeight",
                                mean_bmi >= 25 & mean_bmi <= 29.9 ~ "Overweight",
                                mean_bmi >= 30 ~ "Obesity"))
#scale for bmi is taken from https://www.nhlbi.nih.gov/health/educational/lose_wt/BMI/bmicalc.htm 

head(user_weight_percent)

user_weight_percent_fin <- user_weight_percent %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))
head(user_weight_percent_fin)

user_weight_percent_fin$user_type <- factor(user_weight_percent_fin$user_type , levels = c("Underweight","Overweight","Obesity","NormalWeight"))
head(user_weight_percent_fin)

user_weight_percent_fin %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  scale_fill_brewer(palette="Greens")+
  theme_void()+
  geom_text(aes(label = scales::percent(round(total_percent,2))), position = position_stack(vjust = 0.5))+
  labs(title='User_classification_based_on_bmi')

