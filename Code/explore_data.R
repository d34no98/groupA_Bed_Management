library(readr)
WWL_data <- read_csv("C:/Users/glossop/r_working/SCC460/WWL_data.csv")
View(WWL_data)

summary(WWL_data$spell_episode_los)

library(dplyr)
fil <- filter(WWL_data, WWL_data$spell_episode_los >= 123)
fil

hist(WWL_data$spell_episode_los, 
     breaks=126)

table(WWL_data$spell_episode_los)
fil <- filter(WWL_data, 
              WWL_data$spell_episode_los == 0)

hist(fil$spell_los_hrs)
summary(fil$spell_episode_los)
summary(fil$spell_los_hrs)

fil2 <- filter(fil,
               fil$spell_los_hrs > 24)

summary(fil2$spell_episode_los)
hist(fil2$spell_los_hrs)

table(WWL_data$comorbidity_score)
fil <- filter(WWL_data,
       WWL_data$comorbidity_score == -1)
died <- filter(WWL_data,
               WWL_data$inpatient_death_flag==1)

summary(died$spell_episode_los)

died$discharge_date_days <- died$discharge_date_dt["%d"]
died$discharge_date_dt == died$date_of_death_dt

WWL_data$hrg_group

##notes
# remove people who died?
# check length of stay data?
# define length of stay as categorical? eg tertiles
