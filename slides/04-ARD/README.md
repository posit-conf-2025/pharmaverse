Outline for this section (estimate: 40 mins):

- Intro to ARDs & {cards} (7 mins)
  - CDISC ARS Model concept
  - Where ARDs fit in
    - Principles of ARDs and advantages
  - Intro to {cards} package
    - Talk about pharmaverse collaboration, growth of package

- Basic categorical and continuous summaries (10 mins)
  - `ard_summary()` for continuous + custom summaries
    - Args to highlight:
      - `variables`
      - `by`
      - `statistic`
        - show custom statistic & variable-specific statistics
      - update fmt functions
  - `ard_tabulate()` for n, N, %
    - Args to highlight:
      - `variables`
      - `by`
  - quick overview of other useful functions
  - Stacking function: `ard_stack()`
  
- Exercise #1 - demog (8 mins)
  - Together: 
    - A) continuous summaries by treatment
    - B) categorical summaries by treatment
  - On your own:
    - C) `ard_stack()` for A and B
    - D) Bonus: add overall/total N
  
- Hierarchical summaries (8 mins)
  - Brief touch on `ard_hierarchical()` and `ard_hierarchical_count()`
  - Stacking functions: `ard_stack_hierarchical()` and `ard_stack_hierarchical_count()`
    - Args to highlight:
      - `id`
      - `denominator`
      
- Exercises #2 - ae (7 mins)
  - Together:
    - A) freq & pct of unique subjects w/ at least one AE by SOC and PT, by treatment
  - On your own:
    - B) Bonus: add freq & pct of subjects w/ at least one AE, by treatment