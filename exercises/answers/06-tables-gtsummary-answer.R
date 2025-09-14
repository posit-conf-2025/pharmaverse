# Table Exercise: Demographic summary table using {gtsummary}

# Create a Demography table split by treatment

# Setup
## Load necessary packages
library(gtsummary)
library(tidyverse)

## Import data
df_gtsummary_exercise <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") |>
  left_join(
    pharmaverseadam::advs |>
      filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      arrange(ADY) |>
      slice(1, .by = c(USUBJID, PARAMCD)) |>
      pivot_wider(id_cols = USUBJID, names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  ) |>
  select(USUBJID, TRT01A, AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT) |>
  labelled::set_variable_labels(
    BMI = "BMI",
    HEIGHT = "Height, cm",
    WEIGHT = "Weight, kg"
  )

# 1. Use tbl_summary() to summarize AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT by TRT01A
# 2. For all continuous variables, present the following stats: c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}")
# 3. Ensure the AGEGR1 levels are reported in the correct order
# 4. View the ARD saved in the gtsummary table using `gather_ard()` function
# BONUS!
# 5. Add the header "**Active Treatment**" over the 'Xanomeline' treatments using the `modify_spanning_header()` function


tbl <-
  df_gtsummary_exercise |>
  # ensure the age groups print in the correct order
  mutate(AGEGR1 = factor(AGEGR1, levels = c("18-64", ">64"))) |>
  tbl_summary(
    by = TRT01A,
    include = c(AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT), 
    type = all_continuous() ~ "continuous2", # all continuous variables should be summarized as multi-row
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"), # change the statistics for all continuous variables
    label = list(AGEGR1 = "Age Group"), # add a label for AGEGR1
  ) |>
  # add a header above the 'Xanomeline' treatments. We used `show_header_names()` to know the column names
  modify_spanning_header(c(stat_2, stat_3) ~ "**Active Treatment**")

tbl

# extract the ARD from the table
gather_ard(tbl)
