Trial mortality predictions - NLST
================
Lukas Owens

### Inputs and staging

Values taken from published trial:

<https://www.nejm.org/doi/full/10.1056/nejmoa1102873>

<https://pubmed.ncbi.nlm.nih.gov/31995683/>

``` r
# Read total cases and death count by arm extracted from trial
trial_counts <- read_csv('../data/trial-data-nlst.csv')

# Subjects were followed for 7 year, 5 months (midpoint of recruitment to end of followup)
trial_length <- (5+7*12)/12

# Person-years in each arm, based on total cases and cases per PY (in abstract)
py_ctrl   <- 941 / 572 * 100000 
py_screen <- 1060 / 645 * 100000 

# Define early vs late stage disease
earlylate <- tribble(
  ~stage, ~late,
  'IA',    'E',  
  'IB',    'E',  
  'IIA',   'E',
  'IIB',   'E',    
  'IIIA',  'A',
  'IIIB',  'A',    
  'IV',    'A'     
)

# Total deaths in control arm
total_deaths_ctrl <- 309
```

Group and summarize by early/advanced stage definition

``` r
trial_counts_ea <- trial_counts %>% 
  left_join(earlylate) %>%
  group_by(arm, late) %>%
  summarise(cases = sum(cases)) %>%
  mutate(case_dist = cases / sum(cases))
trial_counts_ea
```

    ## # A tibble: 4 x 4
    ## # Groups:   arm [2]
    ##   arm     late  cases case_dist
    ##   <chr>   <chr> <dbl>     <dbl>
    ## 1 control A       566     0.609
    ## 2 control E       363     0.391
    ## 3 screen  A       447     0.430
    ## 4 screen  E       593     0.570

### Model calculations

Calculate incidence rate (*λ*<sub>*d*</sub>), rate of late-stage cancer
(*p*<sub>1</sub>) and reduction in late-stage (*α*)

``` r
# Incidence from paper abstract
lambda_d <- 572 / 100000 
lambda_d
```

    ## [1] 0.00572

``` r
adv_cases_ctrl   <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(cases)
adv_cases_screen <- trial_counts_ea %>% filter(arm=='screen', late=='A') %>% pull(cases)
alpha <- 1 - (adv_cases_screen / py_screen) / (adv_cases_ctrl / py_ctrl)
alpha
```

    ## [1] 0.2094333

``` r
p_1 <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(case_dist)
p_1
```

    ## [1] 0.6092573

Calculate overall mortality hazard implied by case fatality rate

``` r
fatality_rate <- 309 / 572 # (Deaths/100k) / (Cases/100k) in control arm
case_hazard <- lambda_d

# Formula for calculating fatality rate from above inputs and a given mortality hazard
fatality_rate_fn <- function(mort_hazard) {
  (1-(mort_hazard*exp(-case_hazard*trial_length) - case_hazard*exp(-mort_hazard*trial_length)) / 
     (mort_hazard - case_hazard)) / (1-exp(-case_hazard*trial_length))
}

# Determine mortality rate that achieves observed fatality rate 
f <- function(x) abs(fatality_rate_fn(x) - fatality_rate)
mort_hazard <- optimize(f, lower=0, upper=5)$minimum
mort_hazard
```

    ## [1] 0.2436176

### Calculate calibrated SEER mortality hazards

Import SEER data

``` r
# Import and clean raw data
seer <- read_csv('../data/seer-mortality-nlst.csv', 
                 skip=1,
                 col_names = c('page', 'stage', 'interval', 'n', 'median', 'surv', 'se', 'lb', 'ub')) %>%
  mutate(surv = surv/100) %>%
  left_join(earlylate) %>%
  select(stage, late, n, surv)
seer
```

    ## # A tibble: 11 x 4
    ##    stage     late      n   surv
    ##    <chr>     <chr> <dbl>  <dbl>
    ##  1 IA        E     12043 0.674 
    ##  2 IB        E     10727 0.503 
    ##  3 IIA       E      1445 0.418 
    ##  4 IIB       E      4696 0.327 
    ##  5 IIIA      A     12793 0.198 
    ##  6 IIIB      A     20866 0.124 
    ##  7 IV        A     65302 0.0282
    ##  8 <NA>      <NA>   1704 0.808 
    ##  9 OCCULT    <NA>   1431 0.211 
    ## 10 UNK Stage <NA>   9536 0.108 
    ## 11 Blank(s)  <NA>  95919 0.157

``` r
# Group by early / late
seer_grouped <- seer %>%
  filter(!is.na(late)) %>%
  group_by(late) %>%
  summarise(surv = weighted.mean(surv, n), n=sum(n))
  
# Calculate mortality hazards
seer_hazards <- seer_grouped %>% mutate(hazard_raw = -log(surv) / trial_length )
seer_hazards
```

    ## # A tibble: 2 x 4
    ##   late    surv     n hazard_raw
    ##   <chr>  <dbl> <dbl>      <dbl>
    ## 1 A     0.0705 98961     0.358 
    ## 2 E     0.541  28911     0.0828

Calculate stage-specific mortality hazards (*λ*<sub>0</sub> and
*λ*<sub>1</sub>) calibrated to SEER data

``` r
# Function for calculating mortality hazard given an calibration factor
hazard_adj_fn <- function(adj_factor) {
  adj_surv <- seer_hazards %>% 
    mutate(hazard_adj = hazard_raw * adj_factor,
                          surv_adj = exp(-hazard_adj*trial_length)) %>%
    summarise(surv_adj = weighted.mean(surv_adj, n)) %>%
    pull()
  adj_hazard <- -log(adj_surv) / trial_length
  return(adj_hazard)
}

f <- function(x) abs(hazard_adj_fn(x) - mort_hazard)
adj_factor <- optimize(f, lower=0, upper=5)$minimum
adj_factor
```

    ## [1] 1.061465

``` r
lambda_0 <- adj_factor * seer_hazards %>% filter(late=='E') %>% pull(hazard_raw)
lambda_0
```

    ## [1] 0.08786413

``` r
lambda_1 <- adj_factor * seer_hazards %>% filter(late=='A') %>% pull(hazard_raw)
lambda_1
```

    ## [1] 0.3796054

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
    ##      r_0    r_1   rho  mrel
    ##    <dbl>  <dbl> <dbl> <dbl>
    ## 1 0.0111 0.0278 0.479 0.100

``` r
write_csv(output, '../output/trial-calculations-nlst.csv')
```
