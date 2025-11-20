#Create table 1

WWL_data <- readr::read_csv("WWL_data.csv")

#check sex data
table(WWL_data$sex_national_code)

#1=male, 2=female, 9=indeterminate
#Data dictionary source:
#https://archive.datadictionary.nhs.uk/DD%20Release%20May%202024/data_elements/person_phenotypic_sex.html


WWL_data$sex_national_code <- as.numeric(WWL_data$sex_national_code)

WWL_data$sex_national_code <- factor(
  WWL_data$sex_national_code,
  levels=c(1, 2, 9),
  labels = c("Male", "Female", "Indeterminate")
)

table(WWL_data$sex_national_code)

#check age data
summary(WWL_data$patient_age_on_admission)



#check ethnicity data
table(WWL_data$ethnic_origin_description)
table(WWL_data$ethnic_group)

WWL_data <- WWL_data %>%
  mutate(ethnic_group = case_when(
      WWL_data$ethnic_origin_description %in% c(
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

table(WWL_data$ethnic_group)

#create table 1 - demographics



