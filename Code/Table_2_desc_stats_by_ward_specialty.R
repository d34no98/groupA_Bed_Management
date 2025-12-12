## 
library(readr)

outlier_group <- read_csv("./Code/bed_data_final.csv") %>% 
  filter(outlier_cat == 1)

## Group Analysis for Outliers:
## Number of admissions, total LoS, and average LoS per admission

per_admission_analysis <- outlier_group %>% 
  group_by(specialty_spec_desc) %>% 
  summarize(N = n(),
            sum_los = sum(duration_of_stay, na.rm=TRUE),
            mean_los = mean(duration_of_stay, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(mean_los))

## Use `gtsummary` R package to create the appendix table
## for Average LoS per Ward Specialty for Outliers

# Create a publishable table
library(gt)
gt_tbl <- per_admission_analysis %>%
  gt(rowname_col = "specialty_spec_desc") %>%
  tab_header(
    title = "Descriptive Statistics of Ward Specialty",
    subtitle = "Length of Stay (LoS) >= 7 Days"
  ) %>%
  cols_label(
    N = "Number of Admissions",
    sum_los = "Total Length of Stay (Days)",
    mean_los = "Average Length of Stay (Days)"
  ) %>%
  fmt_number(
    columns = c(sum_los, mean_los),
    decimals = 1
  )

# Export to Word
library(gt)
library(gto)
library(officer)
doc <- read_docx() %>%
  body_add_gt(gt_tbl) %>%   # insert gt table into the Word document
  print(target = "./Output/ward_specialty_desc_stats.docx")
