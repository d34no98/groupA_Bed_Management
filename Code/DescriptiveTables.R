# load packages and data

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
bed_data <- readr::read_csv("WWL_data.csv")

# Format age, sex and ethnicity

#Sex

## 1=male, 2=female, 9=indeterminate
## Data dictionary source:
## https://archive.datadictionary.nhs.uk/DD%20Release%20May%202024/data_elements/person_phenotypic_sex.html

bed_data$sex_national_code <- as.numeric(bed_data$sex_national_code)

bed_data$sex_national_code <- factor(
  bed_data$sex_national_code,
  levels=c(1, 2, 9),
  labels = c("Male", "Female", "Indeterminate")
)
table(bed_data$sex_national_code)

#check age data
summary(bed_data$patient_age_on_admission)

#recode ethnicity data
bed_data <- bed_data %>%
  mutate(ethnic_group = case_when(
    bed_data$ethnic_origin_description %in% c(
      "British (White)", "Irish (White)", 
      "Any other White Background"
    ) ~ "White",
    
    ethnic_origin_description %in% c(
      "Black African", "Black Caribbean", "Any other Black Background"
    ) ~ "Black",
    
    ethnic_origin_description %in% c(
      "Indian", "Pakistani", "Bangladeshi", 
      "Chinese", "Any other Asian background"
    ) ~ "Asian",
    
    ethnic_origin_description %in% c(
      "White and Asian",
      "White and Black African",
      "White and Black Caribbean",
      "Any other mixed background"
    ) ~ "Mixed",
    
    ethnic_origin_description %in% c(
      "Any other ethnic group"
    ) ~ "Other",
    
    ethnic_origin_description %in% c(
      "Not Stated",
      "NOT KNOWN",
      "DW Generated"
    ) ~ "Not known",
  ))

## Derive length of stay using admission and discharge dates
library(lubridate)
bed_data_derive_dates <- bed_data %>% 
  mutate(admission_obj = dmy_hms(admission_date_dt, tz = "Europe/London"),
         Date_of_admission = as.Date(admission_obj),
         discharge_obj = dmy_hms(discharge_date_dt, tz = "Europe/London"),
         Date_of_discharge = as.Date(discharge_obj),
         duration_of_stay = (Date_of_discharge - Date_of_admission) + 1) %>% 
  select(-admission_obj, -discharge_obj)

## Lets set the outlier criteria to where `duration_of_stay`
## is greater than or equal to (>=) 7 days
outliers <- bed_data_derive_dates %>% 
  filter(duration_of_stay >= 7)

## create data separately to have all observations,
## but create a categorical variable for a binary
## variable to label as outlier or not
bed_data_derive_outlier <- bed_data_derive_dates %>% 
  mutate(outlier_cat = if_else(
    duration_of_stay >= 7, 1, 0
  ))
table(bed_data_derive_outlier$outlier_cat)

##add outlier category to main data frame
bed_data$outlier_cat <- bed_data_derive_outlier$outlier_cat

##Make outlier a factor with meaningful labels
bed_data$outlier_cat <- factor(bed_data$outlier_cat,
                               levels=c(1,0),
                               labels = c("Long stay, equal or greater than 7 days",
                               "Short stay, less than 7 days"))

## Label our sex, age and ethnicity variables
library(Hmisc)
label(bed_data$sex_national_code) <- "Sex"
label(bed_data$patient_age_on_admission) <- "Age"
label(bed_data$ethnic_group) <- "Ethnicity"

##recode frailty score
table(bed_data$frailty_score)
library(stringr)
bed_data_frailty_grouped <- bed_data_derive_outlier %>% 
  mutate(dev_frailty_score = as.factor(case_when(str_detect(frailty_score, "10") ~ "NQ",
                                                 str_detect(frailty_score, "1") ~ "1 - Very Fit",
                                                 str_detect(frailty_score, "2") ~ "2 - Well",
                                                 str_detect(frailty_score, "3") ~ "3 - Managing Well",
                                                 str_detect(frailty_score, "4") ~ "4 - Vulnerable",
                                                 str_detect(frailty_score, "5") ~ "5 - Mildly Frail",
                                                 str_detect(frailty_score, "6") ~ "6 - Moderately Frail",
                                                 str_detect(frailty_score, "7") ~ "7 - Severely Frail",
                                                 str_detect(frailty_score, "8") ~ "8 - Very Severely Frail",
                                                 str_detect(frailty_score, "9") ~ "9 - Terminally Ill")
),
         ## relevel ref category for `Frailty Score`
dev_frailty_score = relevel(dev_frailty_score, "1 - Very Fit")
)
table(bed_data_frailty_grouped$dev_frailty_score)
bed_data$dev_frailty_score <- bed_data_frailty_grouped$dev_frailty_score
    
## check if there's patients with multiple spells in hospital
sum(duplicated(bed_data$ID))
add_count(bed_data, ID, sort = FALSE, name= "Number_spells")
##yes there's over 11,000, so let's make a "spell count" column

###Group the data + Add spell count per patient
bed_data_demos <- group_by(bed_data, ID)
bed_data_spellcount <- mutate(bed_data_demos, spell_count = n())
## Label data for table
label(bed_data_spellcount$spell_count) <- "N Admissions"

#create a dataframe just with the first visits & ungroup
bed_data_visit1 <- slice(bed_data_spellcount, 1)
bed_data_visit1 <- ungroup(bed_data_visit1)
label(bed_data_visit1$specialty_spec_desc) <- "Specialty"

# Table 1 for demographics of all patients
library(table1) 
table1(~ sex_national_code + patient_age_on_admission + ethnic_group #demographics
       + spell_count #variables of interest
       data=bed_data_visit1)
#total n of patients is 30379

#another table to describe variables in analysis, for all *admissions*
#(not per patients)
bed_data <- dplyr::filter(bed_data, sex_national_code != "Indeterminate")
bed_data$sex_national_code <- factor(bed_data$sex_national_code,
                                     levels=c("Male", "Female"))
label(bed_data$sex_national_code) <- "Sex"

bed_data$readmission_flag_28_days<- factor(
  bed_data$readmission_flag_28_days,
  levels=c(1,0),
  labels=c("Yes","No")
)
label(bed_data$dev_frailty_score) <- "Frailty Score"
label(bed_data$readmission_flag_28_days) <- "Readmission from past 28 days"

table1(~ sex_national_code + patient_age_on_admission + ethnic_group
       + dev_frailty_score + readmission_flag_28_days
       | outlier_cat,
       data=bed_data)
