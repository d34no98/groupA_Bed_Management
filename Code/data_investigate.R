library(dplyr)
library(readr)
bed_data <- read_csv("./Code/WWL_data.csv")

summary(bed_data)

## Find some NA columns and exclude them from the dataset
na_columns <- names(which(colSums(is.na(bed_data)) > 0))
all_na_columns <- c()

for (col in na_columns) {
  if (all(is.na(bed_data[col]))) {
    all_na_columns <- c(all_na_columns, col)
  }
}

bed_data_drop_na <- bed_data %>% 
  select(-all_na_columns)

######## Analysis on the Full range of data ########
## Store all character and numeric variables separately
library(stringr)
char_columns <- 
  names(bed_data_drop_na)[sapply(bed_data_drop_na, is.character)]
numeric_columns <- 
  names(bed_data_drop_na)[sapply(bed_data_drop_na, is.numeric)]

for (col in char_columns) {
 print(
   bed_data_drop_na %>%
     group_by(!!sym(col)) %>%
     summarize(N=n())
 )
}


for (col in na_columns) {
  print(
    bed_data %>%
      group_by(!!sym(col)) %>%
      summarize(n = n(), .groups = "drop")
  )
}

## Investigate all Comorbidities against patient stay
comorbid_columns <- 
  names(bed_data_drop_na)[sapply(names(bed_data_drop_na), 
                                       function(x) endsWith(x, "_flag") & startsWith(x, "comorbidity_"))]

## Investigate all Chronic conditions against patient stay
chronic_columns <- 
  names(bed_data_drop_na)[sapply(names(bed_data_drop_na), 
                                       function(x) endsWith(x, "_flag") & startsWith(x, "chronic_"))]

## Derive length of stay using admission and discharge dates
library(lubridate)
bed_data_derive_dates <- bed_data %>% 
  mutate(admission_obj = dmy_hms(admission_date_dt, tz = "Europe/London"),
         Date_of_admission = as.Date(admission_obj),
         discharge_obj = dmy_hms(discharge_date_dt, tz = "Europe/London"),
         Date_of_discharge = as.Date(discharge_obj),
         duration_of_stay = (Date_of_discharge - Date_of_admission) + 1) %>% 
  select(-admission_obj, -discharge_obj)

bed_data_derive_dates %>% 
  filter(duration_of_stay >= 123) %>% 
  select(duration_of_stay, spell_episode_los, admission_date_dt, discharge_date_dt)

## Summarize our derivation of length of stay against `spell_episode_los`
summary(bed_data_derive_dates$duration_of_stay)
summary(bed_data$spell_episode_los) # Length of stay calculated in the original data frame

####
# From the results we see 75% of data reports a length of stay up to 1 day.
# There for the the upper 25% is reporting extreme values for length of stay
# lets subset the upper 25% and investigate where they could be experiencing
# longer stays
####




######## Investigate only on the Top 20% of LOS duration rows ########
upper_20 <- bed_data_derive_dates %>% 
  filter(duration_of_stay > quantile(duration_of_stay, 0.8, na.rm=TRUE))


## Quintile summary of duration_of_stay 
quintile_summary <- quantile(bed_data_derive_dates$duration_of_stay, probs = seq(0, 1, 0.01), na.rm = TRUE)
print(quintile_summary)

## Plot box plots for each comorbidity flags
library(ggplot2)
for (col in comorbid_columns) {
  print(
    ggplot(data=upper_20, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}

## Comorbidity HIV showed a significant difference in length of stays


## Plot box plots for each chronic condition flags
for (col in chronic_columns) {
  print(
    ggplot(data=upper_20, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}

## Look in to frailty score
ggplot(data=upper_20, aes(x=frailty_score, y=duration_of_stay)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=45))
### Results: same categories need to be placed with the same result
### use `mutate to bring these categories together`
library(stringr)
upper_20_frailty_grouped <- upper_20 %>% 
  mutate(dev_frailty_score = case_when(str_detect(frailty_score, "1") ~ "1 - Very Fit",
                   str_detect(frailty_score, "2") ~ "2 - Well",
                   str_detect(frailty_score, "3") ~ "3 - Managing Well",
                   str_detect(frailty_score, "4") ~ "4 - Vulnerable",
                   str_detect(frailty_score, "5") ~ "5 - Mildly Frail",
                   str_detect(frailty_score, "6") ~ "6 - Moderately Frail",
                   str_detect(frailty_score, "7") ~ "7 - Severely Frail",
                   str_detect(frailty_score, "8") ~ "8 - Very Severely Frail",
                   str_detect(frailty_score, "9") ~ "9 - Terminally Ill"))

## Boxplot on re-derived Frailty Score
ggplot(data=upper_20_frailty_grouped, aes(x=dev_frailty_score, y=duration_of_stay)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=45))
### RESULTS: No correlation found

## Boxplot for Speciality departments length of stay for the Top 20%
## of observations recording the longest lengths of stays
ggplot(data=upper_20, aes(x=specialty_spec_desc, y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Speciality departments by length of Patient Stay",
       subtitle = "Top 20% data")



######## High associations spotted in High LOS outliers only ########

## Analysis on the outliers of the data only

outliers <- bed_data_derive_dates %>%
  filter(duration_of_stay < quantile(duration_of_stay, 0.25, na.rm = TRUE) - 1.5 * IQR(duration_of_stay, na.rm = TRUE) |
           duration_of_stay > quantile(duration_of_stay, 0.75, na.rm = TRUE) + 1.5 * IQR(duration_of_stay, na.rm = TRUE)) 



## Plot box plots for each comorbidity flags
for (col in comorbid_columns) {
  print(
    ggplot(data=outliers, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}

## Plot box plots for each chronic condition flags
for (col in chronic_columns) {
  print(
    ggplot(data=outliers, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}

table(outliers$sex_description.y, useNA = "always")
# We lose a 3rd of outlier data due to sex not being recorded

ggplot(data=outliers, aes(x=patient_age_on_admission, y=duration_of_stay,
                          col = sex_description.y)) +
  geom_point()

ggplot(data=outliers, aes(x=ward_type_admission, y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

ggplot(data=outliers, aes(x=ward_type_admission, y=duration_of_stay)) +
  geom_boxplot()

ggplot(data=outliers, aes(x=patient_age_on_discharge, y=duration_of_stay,
                          col = sex_description.y)) +
  geom_point()