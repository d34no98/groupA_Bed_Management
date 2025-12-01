library(glmmTMB)
log_model_1 <- glmmTMB(
  data=bed_data_cleaned ,
  formula = outlier_cat ~  
    patient_age_on_admission + (1 | ID) + (1 | site_description),
  family = binomial(link="logit"))

# log_model_1_a <- glm(
#   data=bed_data_cleaned,
#   formula = outlier_cat ~ patient_age_on_admission,
#   family = binomial(link = "logit"))

summary(log_model_1)

## log_model_2 : Predict Likelihood of an Outlier Length of Stay (LoS)
##               for sex only, accounting for random effects on `ID`

log_model_2 <- glmmTMB(
  data=bed_data_cleaned,
  formula = outlier_cat ~  dev_sex + (1 | ID) + (1 | site_description),
  family = binomial(link="logit"))

summary(log_model_2)

## log_model_3 : Predict Likelihood of an Outlier Length of Stay (LoS)
##               for CFS only, accounting for random effects on `ID`

log_model_3 <- glmmTMB(
  data=bed_data_cleaned,
  formula = outlier_cat ~  dev_frailty_score + (1 | ID) + (1 | site_description),
  family = binomial(link="logit"))

summary(log_model_3)

## log_model_4 : Predict Likelihood of an Outlier Length of Stay (LoS)
##               for Admission within 28 days, accounting for
##               random effects on `ID`

log_model_4 <- glmmTMB(
  data=bed_data_cleaned,
  formula = outlier_cat ~ 
    readmission_flag_28_days  + (1 | ID) + (1 | site_description),
  family = binomial(link="logit"))

summary(log_model_4)

## log_model_5 : Predict Likelihood of an Outlier Length of Stay (LoS)
##               for Speciality Descriptions, accounting for
##               random effects on `ID`

log_model_5 <- glmmTMB(
  data=bed_data_cleaned,
  formula = outlier_cat ~ specialty_spec_desc + (1 | ID) + (1 | site_description),
  family = binomial(link="logit"))

summary(log_model_5)

log_model_6 <- glmmTMB(
  data=bed_data_cleaned,
  formula = outlier_cat ~ dev_ethnic_group + (1 | ID) + (1 | ward_name_admission),
  family = binomial(link="logit"))

summary(log_model_6)

log_model_7 <- glmmTMB(
  data = bed_data_cleaned,
  formula = outlier_cat ~ patient_age_on_admission + dev_sex +
    dev_ethnic_group +  dev_frailty_score + readmission_flag_28_days + 
   (1 | ID) + (1 | ward_name_admission),
  family = binomial(link="logit")
)

summary(log_model_7)

## Write Results
install.packages("gtsummary")
install.packages("broom.helpers")
library(gtsummary)
library(broom.helpers)

summary_table <- tbl_regression(
  log_model_7,
  label = list(patient_age_on_admission ~ "Age (Years) on Admission",
               dev_sex ~ "Gender",
               dev_ethnic_group ~ "Ethnicity",
               dev_frailty_score ~ "Clinical Frailty Score (CFS)",
               readmission_flag_28_days ~ "Patient Readmissioned?"),
  exponentiate = TRUE,
  intercept = TRUE,
  estimate_fun = function(x) style_number(x, digits = 3),
  pvalue_fun = label_style_pvalue(digits=3)) %>% 
  bold_labels()

## Export the final model
library(flextable)
library(officer)

setwd("./Output") ## Send Tables to the specific Folder path

summary_table %>% 
  as_flex_table() %>%  ## Convert to a FlexTable to modify 
                       ## file to save table as
  ### Save Results as a Word File
  save_as_docx(
    path = "final_log_regression_model_results.docx")



### Complete results for the Negative Binomial Regression

nbinom_model_1 <- glmmTMB(
  duration_of_stay ~ patient_age_on_admission + (1 | ID) + (1 | ward_name_admission),
  data = bed_data_cleaned,
  family = nbinom2  # or nbinom1
)

summary(nbinom_model_1)

## nbinom_model_2 : Predict the Duration of Stay (Number of Days) 
##                  for sex only accounting for random effects on `ID`

nbinom_model_2 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~  dev_sex + (1 | ID) + (1 | ward_name_admission),
  family = nbinom2)

summary(nbinom_model_2)

## nbinom_model_3 : Predict the Duration of Stay (Number of Days)
##                  for CFS only, accounting for random effects on `ID`

nbinom_model_3 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~  dev_frailty_score + (1 | ID) + (1 | ward_name_admission),
  family = nbinom2)

summary(nbinom_model_3)

## nbinom_model_4 : Predict Duration of stay (Number of Days)
##                  for Admission within 28 days, accounting for
##                  random effects on `ID`

nbinom_model_4 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~ readmission_flag_28_days  + (1 | ID)
  + (1 | ward_name_admission),
  family = nbinom2)


summary(nbinom_model_4)

## log_model_5 : Predict Duration of stay (Number of Days)
##               for Speciality Descriptions, accounting for
##               random effects on `ID`

nbinom_model_5 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~ specialty_spec_desc + (1 | ID)
  + (1 | ward_name_admission),
  family = nbinom2)


summary(nbinom_model_5)

nbinom_model_6 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~ dev_ethnic_group  + (1 | ID)
  + (1 | ward_name_admission),
  family = nbinom2)

summary(nbinom_model_6)

nbinom_model_7 <- glmmTMB(
  data=bed_data_cleaned,
  formula = duration_of_stay ~ patient_age_on_admission + dev_sex + 
    dev_ethnic_group + dev_frailty_score + readmission_flag_28_days + 
    specialty_spec_desc + (1 | ID) + (1 | ward_name_admission),
  family = nbinom2)

summary(nbinom_model_7)

## Write the model in a FlexTable format
nbinom_model_summary <- tbl_regression(
  nbinom_model_7,
  label = list(patient_age_on_admission ~ "Age (Years) on Admission",
               dev_sex ~ "Gender",
               dev_ethnic_group ~ "Ethnicity",
               dev_frailty_score ~ "Clinical Frailty Score (CFS)",
               readmission_flag_28_days ~ "Patient Readmissioned?",
               specialty_spec_desc ~ "Clinical Specialty"),
  exponentiate = FALSE,
  intercept = TRUE,
  estimate_fun = function(x) style_number(x, digits = 3),
  pvalue_fun = label_style_pvalue(digits=3)) %>% 
  bold_labels()

## Export the final Negative Binomial Model
library(flextable)
library(officer)

nbinom_model_summary %>% 
  as_flex_table() %>%  ## Convert to a FlexTable to modify 
  ## file to save table as
  ### Save Results as a Word File
  save_as_docx(
    path = "final_NB_regression_model_results.docx")