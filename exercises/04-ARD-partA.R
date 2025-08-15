# ARD Exercise: Demographic summaries using {cards}

# For this exercise, compute all the necessary summaries for a demographics
# table that includes age, sex, and race. We want to compute these
# summaries within each treatment group (High Dose, Low Dose, Placebo) and
# overall.

# Setup
## Load necessary packages
library(cards)
library(dplyr)
library(tidyr)

## Import data
adsl <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") 

##  A. First, compute the continuous summaries for AGE and BMI by TRT01A

ard_continuous(
  data = adsl,
  by = ,
  variables =
)


##  B. Next, compute the categorical summaries for SEX and RACE by TRT01A

ard_categorical(
  data = adsl,
  by = ,
  variables =
)

## C. Perform all of the summaries in a single ard_stack() call, including:
#   - summaries by TRT01A as performed above
#      - continuous summaries from part A for AGE and BMI
#      - categorical summaries from part B for SEX and RACE
ard_stack(
  data = adsl,
  .by = ,
  
  # add ard_* calls here
  
)


## D. BONUS!
#   For part C. above, add the following pieces
#   - overall summaries for all of the variables
#   - total N
# (Hint: Modify the `.overall` and `.total_n` arguments, respectively)

