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

# ** Ollie's code to clean NAs **
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

# ** Zoe's code to count n spells and Create table 1
##add outlier category to main data frame
bed_data$outlier_cat <- bed_data_derive_outlier$outlier_cat

##Make outlier a factor with meaningful labels
bed_data$outlier_cat <- factor(bed_data$outlier_cat,
                               levels=c(1,0),
                               labels = c("Short stay <7 days",
                               "Long stay >= 7 days"))

## Label our sex, age and ethnicity variables
label(bed_data$sex_national_code) <- "Sex"
label(bed_data$patient_age_on_admission) <- "Age"
label(bed_data$ethnic_group) <- "Ethnicity"

## check if there's patients with multiple spells in hospital
sum(duplicated(bed_data$ID))
add_count(bed_data, ID, sort = FALSE, name= "Number_spells")
##yes there's over 11,000, so let's make a "spell count" column

###Group the data + Add spell count per patient
bed_data_demos <- group_by(bed_data, ID)
bed_data_spellcount <- mutate(bed_data_demos, spell_count = n())

#create a dataframe just with the first visits & ungroup
bed_data_visit1 <- slice(bed_data_spellcount, 1)
bed_data_visit1 <- ungroup(bed_data_visit1)

# Table 1 for each patient (not each visit!)

library(table1) #<- load handy table1 package

table1(~ sex_national_code + patient_age_on_admission + ethnic_group #demographics
       + spell_count #variables of interest
       | outlier_cat,
       data=bed_data_visit1)

#total n of patients is 30379



