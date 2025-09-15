# ARD Exercise: Adverse Events summaries using {cards}


# Setup: run this first! --------------------------------------------------

# Load necessary packages
library(cards) 

# Import & subset data
adsl <- pharmaverseadam::adsl |> 
  dplyr::filter(SAFFL=="Y")

adae <- pharmaverseadam::adae |> 
  dplyr::filter(SAFFL=="Y") |> 
dplyr::filter(AESOC %in% unique(AESOC)[1:3]) |> 
  dplyr::group_by(AESOC) |> 
  dplyr::filter(AEDECOD %in% unique(AEDECOD)[1:3]) |> 
  dplyr::ungroup()


# Exercise ----------------------------------------------------------------

# A. Calculate the number and percentage of *unique* subjects with at least one AE:
#  - By each SOC (AESOC)
#  - By each Preferred term (AEDECOD) within SOC (AESOC)
# By every combination of treatment group (ARM) 

ard_stack_hierarchical(
  data = ,
  variables = ,
  by = , 
  id = ,
  denominator = 
) 

# B. [*BONUS*] Modify the code from part A to include overall number/percentage of
# subjects with at least one AE, regardless of SOC and PT
