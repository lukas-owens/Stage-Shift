Trial mortality predictions - UKCTOCS
================
Lukas Owens

### Inputs and staging

Values taken from published trial:
<https://pubmed.ncbi.nlm.nih.gov/33991479/>

``` r
# Read total cases and death count by arm extracted from trial
trial_counts <- read_csv('../data/trial-data-ukctocs.csv')

# Subjects were followed for 19.25 years (from April 2001 to June 2020)
trial_length <- 19.25

# Person-years in each arm (from Table 1)
py_ctrl   <- 1577517
py_screen <- 789129

# Define early vs late stage disease
earlylate <- tribble(
  ~stage, ~late,
  'I',    'E',  
  'II',   'A',    
  'III',  'A',    
  'IV',   'A'     
)
```

Group and summarize by early/advanced stage definition

``` r
trial_counts_ea <- trial_counts %>% 
  left_join(earlylate) %>%
  group_by(arm, late) %>%
  summarise(cases = sum(cases), deaths = sum(deaths)) %>%
  mutate(case_dist = cases / sum(cases), fatality_rate = deaths / cases)
trial_counts_ea
```

    ## # A tibble: 4 x 6
    ## # Groups:   arm [2]
    ##   arm     late  cases deaths case_dist fatality_rate
    ##   <chr>   <chr> <dbl>  <dbl>     <dbl>         <dbl>
    ## 1 control A       791    589     0.789        0.745 
    ## 2 control E       212     20     0.211        0.0943
    ## 3 screen  A       362    268     0.700        0.740 
    ## 4 screen  E       155     23     0.300        0.148

### Model calculations

Calculate incidence rate (*λ*<sub>*d*</sub>), rate of late-stage cancer
(*p*<sub>1</sub>) and reduction in late-stage (*α*)

``` r
lambda_d <- sum(trial_counts_ea %>% filter(arm=='control') %>% pull(cases)) / py_ctrl
lambda_d
```

    ## [1] 0.0006358093

``` r
adv_cases_ctrl   <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(cases)
adv_cases_screen <- trial_counts_ea %>% filter(arm=='screen', late=='A') %>% pull(cases)
alpha <- 1 - (adv_cases_screen / py_screen) / (adv_cases_ctrl / py_ctrl)
alpha
```

    ## [1] 0.08513264

``` r
p_1 <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(case_dist)
p_1
```

    ## [1] 0.7886341

Calculate mortality hazards (*λ*<sub>0</sub> and *λ*<sub>1</sub>)
implied by case fatality rates in data.

``` r
calc_mortality_rates <- function(ea) {
  fatality_rate <- trial_counts_ea %>% filter(arm == 'control', late==ea) %>% pull(fatality_rate)
  total_cases <- trial_counts_ea %>% filter(arm == 'control', late==ea) %>% pull(cases)
  case_hazard <- total_cases / py_ctrl 
  
  # Formula for calculating fatality rate from above inputs and a given mortality hazard
  fatality_rate_fn <- function(mort_hazard) {
    (1-(mort_hazard*exp(-case_hazard*trial_length) - case_hazard*exp(-mort_hazard*trial_length)) / (mort_hazard - case_hazard)) / (1-exp(-case_hazard*trial_length))
  }
    
  
  # Determine mortality rate that achieves observed fatality rate 
  f <- function(x) abs(fatality_rate_fn(x) - fatality_rate)
  return(optimize(f, lower=0, upper=5)$minimum)
}
lambda_0 <- calc_mortality_rates('E')
lambda_1 <- calc_mortality_rates('A')
```

### Final calculations

``` r
r_0 <- calc_r(trial_length, lambda_d, lambda_0)
r_1 <- calc_r(trial_length, lambda_d, lambda_1)
rho <- calc_rho(trial_length, r_0, r_1, p_1)
mrel <- rho * alpha

output <- tribble(
  ~r_0, ~r_1, ~rho, ~mrel,
  r_0, r_1, rho, mrel
)
output
```

    ## # A tibble: 1 x 4
    ##       r_0     r_1   rho   mrel
    ##     <dbl>   <dbl> <dbl>  <dbl>
    ## 1 0.00115 0.00906 0.845 0.0719

``` r
write_csv(output, '../output/trial-calculations-ukctocs.csv')
```
