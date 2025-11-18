library(readr)
bed_data <- read_csv("./Code/WWL_data.csv")

summary(bed_data)
na_columns <- names(which(colSums(is.na(bed_data)) > 0))
all_na_columns <- c()

for (col in na_columns) {
  browser()
  if (all(is.na(bed_data[col]))) {
    all_na_columns <- c(all_na_columns, col)
  }
}

bed_data_drop_na <- bed_data %>% 
  select(-all_na_columns)

## Drop date variables
bed_data_chars_numeric <- bed_data_drop_na %>% 
  select(-date_columns)

## Store all character and numeric variables separately
library(stringr)
char_columns <- 
  names(bed_data_chars_numeric)[sapply(bed_data_chars_numeric, is.character)]
numeric_columns <- 
  names(bed_data_chars_numeric)[sapply(bed_data_chars_numeric, is.numeric)]

for (col in char_columns) {
 print(
   bed_data_chars_numeric %>%
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
  names(bed_data_chars_numeric)[sapply(names(bed_data_chars_numeric), 
                                       function(x) endsWith(x, "_flag") & startsWith(x, "comorbidity_"))]

## Investigate all Chronic conditions against patient stay
chronic_columns <- 
  names(bed_data_chars_numeric)[sapply(names(bed_data_chars_numeric), 
                                       function(x) endsWith(x, "_flag") & startsWith(x, "chronic_"))]

## Derive length of stay using admission and discharge dates
library(lubridate)
bed_data_derive_dates <- bed_data %>% 
  mutate(admission_obj = dmy_hms(admission_date_dt, tz = "Europe/London"),
         Date_of_admission = as.Date(admission_obj),
         discharge_obj = dmy_hms(discharge_date_dt, tz = "Europe/London"),
         Date_of_discharge = as.Date(discharge_obj),
         duration_of_stay = (Date_of_discharge - Date_of_admission)) %>% 
  select(-admission_obj, -discharge_obj)

## Summarize our derivation of length of stay against `spell_episode_los`
summary(bed_data_derive_dates$duration_of_stay)
summary(bed_data$spell_episode_los) # Length of stay calculated in the original data frame

####
# From the results we see 75% of data reports a length of stay up to 1 day.
# There for the the upper 25% is reporting extreme values for length of stay
# lets subset the upper 25% and investigate where they could be experiencing
# longer stays
####

upper_25 <- bed_data_derive_dates %>% 
  filter(duration_of_stay > quantile(duration_of_stay, 0.75, na.rm=TRUE))

## Plot box plots for each comorbidity flags
library(ggplot2)
for (col in comorbid_columns) {
  print(
    ggplot(data=upper_25, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}

## Comorbidity HIV showed a significant difference in length of stays


## Plot box plots for each chronic condition flags
for (col in chronic_columns) {
  print(
    ggplot(data=upper_25, aes(x=as.factor(!!sym(col)), y=duration_of_stay)) +
      geom_boxplot()
  )
}


colnames(bed_data)

