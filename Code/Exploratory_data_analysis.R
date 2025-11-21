### Exploratory Data Analysis

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
bed_data <- read_csv("./Code/WWL_data.csv")

summary(bed_data)

## Find some NA columns and exclude them from the dataset
na_columns <- names(which(colSums(is.na(bed_data)) > 0))
all_na_columns <- c()

## Drop unnecesaary columns with all NAs as the values
bed_data_drop_na <- bed_data %>% 
  select(-all_na_columns)

## Store all character and numeric variables separately
library(stringr)
char_columns <- 
  names(bed_data_drop_na)[sapply(bed_data_drop_na, is.character)]
numeric_columns <- 
  names(bed_data_drop_na)[sapply(bed_data_drop_na, is.numeric)]

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
  ## Extend to derive hours separately also

## Produce a quantile break down to associate the best data to
## recognise for outliers
## Quintile summary of duration_of_stay 
quintile_summary <- quantile(bed_data_derive_dates$duration_of_stay, probs = seq(0, 1, 0.01), na.rm = TRUE)
print(quintile_summary)

## Lets set the outlier criteria to where `duration_of_stay`
## is greater than or equal to (>=) 7 days
outliers <- bed_data_derive_dates %>% 
  filter(duration_of_stay > 7)

## create data separately to have all observations,
## but create a categorical variable for a binary
## variable to label as outlier or not
bed_data_derive_outlier <- bed_data_derive_dates %>% 
  mutate(outlier_cat = if_else(
    duration_of_stay > 7, 1, 0
  ))
table(bed_data_derive_outlier$outlier_cat)

## Look in to frailty score
ggplot(data=outliers, aes(x=frailty_score, y=duration_of_stay)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=45))
### Results: same categories need to be placed with the same result
### use `mutate to bring these categories together`
library(stringr)
bed_data_frailty_grouped <- bed_data_derive_outlier %>% 
  mutate(dev_frailty_score = case_when(str_detect(frailty_score, "10") ~ "NQ",
                                       str_detect(frailty_score, "1") ~ "1 - Very Fit",
                                       str_detect(frailty_score, "2") ~ "2 - Well",
                                       str_detect(frailty_score, "3") ~ "3 - Managing Well",
                                       str_detect(frailty_score, "4") ~ "4 - Vulnerable",
                                       str_detect(frailty_score, "5") ~ "5 - Mildly Frail",
                                       str_detect(frailty_score, "6") ~ "6 - Moderately Frail",
                                       str_detect(frailty_score, "7") ~ "7 - Severely Frail",
                                       str_detect(frailty_score, "8") ~ "8 - Very Severely Frail",
                                       str_detect(frailty_score, "9") ~ "9 - Terminally Ill"))
table(bed_data_frailty_grouped$dev_frailty_score)

## Look for associations closely in the comorbidity scores
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

## A couple of other variables of interest with reference to 
## frequency in our meta-analysis

table(outliers$sex_description.y, useNA = "always")

# We lose a 3rd of outlier data due to sex not being recorded

ggplot(data=outliers, aes(x=patient_age_on_admission, y=duration_of_stay,
                          col = sex_national_code)) +
  geom_point()

## Fix`Sex` by re-deriving with sex_national_code
bed_data_derive_sex <- bed_data_frailty_grouped %>% 
  mutate(dev_sex = factor(
    case_when(sex_national_code == 1 ~ "Male",
              sex_national_code == 2 ~ "Female",
              TRUE ~ NA_character_)
  ))

## Ward Type Admission
ggplot(data=outliers, aes(x=ward_type_admission, y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

## Attendance Type
ggplot(data=outliers, aes(x=attendancetype, y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

## Specialty Spec Desc
ggplot(data=outliers, aes(x=specialty_spec_desc, y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

## Make `General Medicine` the reference for this variable
library(forcats)
bed_data_derive_spec <- bed_data_derive_sex %>% 
  mutate(specialty_spec_desc = as.factor(specialty_spec_desc))

## Set General Medicine as our reference category
bed_data_derive_spec$specialty_spec_desc <-
  fct_relevel(bed_data_derive_spec$specialty_spec_desc,
              "General Medicine")


ggplot(data=outliers, aes(x=as.factor(dementia_diagnosis_flag), y=duration_of_stay)) +
  geom_boxplot()

ggplot(data=outliers, aes(x=inj_or_ail, y=duration_of_stay)) +
  geom_boxplot()

ggplot(data=outliers, aes(x=as.factor(place_of_incident), y=duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

ggplot(data=outliers, aes(x=specialty_division, y=duration_of_stay)) +
  geom_boxplot()

## Ethnicity
ggplot(data=outliers, aes(x=ethnic_origin_description, y = duration_of_stay)) +
  geom_boxplot() +
  coord_flip()

## Readmission
ggplot(data=outliers, aes(x=as.factor(readmission_flag_28_days), y = duration_of_stay)) +
  geom_boxplot()

ggplot(data=outliers, aes(x=as.factor(readmission_flag_28_days_emergancy), y = duration_of_stay)) +
  geom_boxplot()


