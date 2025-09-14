# Exercise 1
# Update date and time imputation arguments

library(tibble)
library(lubridate)
library(admiral)

posit_mh <- tribble(
  ~USUBJID, ~MHSTDTC,
  1,        "2019-07-18T15:25:40",
  1,        "2019-07-18T15:25",
  1,        "2019-07-18",
  2,        "2024-02",
  2,        "2019",
  2,        "2019---07",
  3,        ""
)

derive_vars_dtm(
  dataset = posit_mh,
  new_vars_prefix = "AST",
  dtc = MHSTDTC,
  highest_imputation = "M",
  date_imputation = "????",
  time_imputation = "????"
)

# Exercise 2
# Update the parameters argument
# Update set_values_to argument for the formula
# MAP Formula: MAP = (SYSBP + 2*DIABP) / 3


ADVS <- tribble(
  ~USUBJID,      ~PARAMCD, ~PARAM,                            ~AVALU,  ~AVAL, ~VISIT,
  "01-701-1015", "DIABP",  "Diastolic Blood Pressure (mmHg)", "mmHg",    51, "BASELINE",
  "01-701-1015", "SYSBP",  "Systolic Blood Pressure (mmHg)",  "mmHg",   121, "BASELINE",
  "01-701-1028", "DIABP",  "Diastolic Blood Pressure (mmHg)", "mmHg",    79, "BASELINE",
  "01-701-1028", "SYSBP",  "Systolic Blood Pressure (mmHg)",  "mmHg",   130, "BASELINE",
) 

derive_param_computed(
  ADVS,
  by_vars = exprs(USUBJID, VISIT),
  parameters = c("????", "????"),
  set_values_to = exprs(
    AVAL = (AVAL.SYSBP + ?? * AVAL.DIABP) / ??,
    PARAMCD = "MAP",
    PARAM = "Mean Arterial Pressure (mmHg)",
    AVALU = "mmHg",
  )
) 




