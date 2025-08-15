# Exercise: Adverse Events summaries using {cards}

# Load necessary packages
library(cards) 

# Import data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
adae <- pharmaverseadam::adae 

# Exercise:

# A. Calculate the number and percentage of *unique* subjects with at least one AE:
#  - By each SOC (AESOC)
#  - By each Preferred term (AEDECOD) within SOC (AESOC)
# By every combination of treatment group (TRT01A) 

ard_stack_hierarchical(
  data = ,
  variables = ,
  by = , 
  id = ,
  denominator = 
) 

# B. [*BONUS*] Modify the code from part A to include overall number/percentage of
# subjects with at least one AE, regardless of SOC and PT
