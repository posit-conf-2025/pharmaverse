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

##  A. First, compute the continuous summaries for AGE by TRT01A

ard_continuous(
  data = adsl,
  by = TRT01A,
  variables = AGE
)


##  B. Next, compute the categorical summaries for AGEGR1 and SEX by TRT01A

ard_categorical(
  data = adsl,
  by = TRT01A,
  variables = c(AGEGR1, SEX)
)

## C. Perform all of the summaries in a single ard_stack() call, including:
#   - summaries by TRT01A as performed above
#      - continuous summaries from part A for AGE
#      - categorical summaries from part B for AGEGR1 and SEX
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous(variables = AGE),
  ard_categorical(variables = c(AGEGR1, SEX))
)


## D. BONUS!
#   For part C. above, add the following pieces
#   - overall summaries for all of the variables
#   - total N
# (Hint: Modify the `.overall` and `.total_n` arguments, respectively)
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous(variables = AGE),
  ard_categorical(variables = c(AGEGR1, SEX)),
  .overall = TRUE,
  .total_n = TRUE
)

