# Table Exercise: Demographic summary table using {tfrmt}

# For this exercise, we will use the demography ARD from the last section to
# create a {tfrmt} table

# Setup
## Load necessary packages
library(cards)
library(dplyr)
library(tidyr)
library(tfrmt)

## Import & subset data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
adae <- pharmaverseadam::adae |> 
  dplyr::filter(AESOC %in% unique(AESOC)[1:3]) |> 
  dplyr::group_by(AESOC) |> 
  dplyr::filter(AEDECOD %in% unique(AEDECOD)[1:3]) |> 
  dplyr::ungroup()

## Create AE Summary using cards
ard_ae <- ard_stack_hierarchical(
  data = adae,
  variables = c(AESOC, AEDECOD),
  by = TRT01A, 
  id = USUBJID,
  denominator = adsl,
  over_variables = TRUE,
  statistic = ~ c("n", "p")
) 

# Exercise:

# A. Convert `cards` object into a tidy data frame ready for {tfrmt}. 
#    Nothing to do besides run each step & explore the output!

ard_ae_tidy <- ard_ae |> 
  shuffle_card(fill_hierarchical_overall = "ANY EVENT") |> 
  prep_big_n(vars = "TRT01A") |> 
  prep_hierarchical_fill(vars = c("AESOC","AEDECOD"),
                         fill_from = "left")|> 
  dplyr::select(-c(context, stat_label, stat_variable))  

# B. Create a basic tfrmt, filling in the appropriate variable names

ae_tfrmt <- tfrmt(
  group = AESOC,
  label = AEDECOD,
  param = stat_name,
  value = stat,
  column = TRT01A,
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", 
                   frmt_combine(
                     "{n} ({p}%)",
                     n = frmt("xx"),
                     p = frmt("xx", transform = ~ . *100)
                   )
    )
  ),
  big_n = big_n_structure(param_val = "bigN") 
) 

print_to_gt(ae_tfrmt,
            ard_ae_tidy)


# C. Switch the order of the columns so Placebo is last

ae_tfrmt <- ae_tfrmt |> 
  tfrmt(
    col_plan = col_plan(
      contains("High Dose"),
      contains("Low Dose"),
      "Placebo"
    )
  )  

print_to_gt(ae_tfrmt, ard_ae_tidy)


# D. Add a title and source note for the table

ae_tfrmt <- ae_tfrmt |> 
  tfrmt(
    title = "AE Table",
    footnote_plan = footnote_plan(
      footnote_structure("A typical AE Table")
    ) 
  )

print_to_gt(ae_tfrmt, ard_ae_tidy)

