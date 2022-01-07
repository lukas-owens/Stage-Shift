Trial mortality predictions - ERSPC
================
Lukas Owens

### Inputs and staging

Values taken from published trial:

<https://www.nejm.org/doi/full/10.1056/nejmoa1113135>

<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6027585/>

``` r
# Read total cases count by arm extracted from trial
trial_counts <- read_csv('../data/trial-data-erspc.csv')

# Subjects were followed for median of 11 years
trial_length <- 11

# Person-years in each arm, from Table 3
py_ctrl   <- 933052
py_screen <- 764233 

# Define early vs late stage disease
earlylate <- tribble(
  ~stage,          ~late,
  'low',          'E',
  'intermediate', 'E',
  'high',         'E',
  'metastatic',   'A'
)

# Total deaths in control arm (Figure 1)
total_deaths_ctrl <- 462

# Actual observed mortality reduction (and upper/lower CI bounds)
actual <- 0.21
lb <- 0.09
ub <- 0.32
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

    ## # A tibble: 6 x 4
    ## # Groups:   arm [2]
    ##   arm     late  cases case_dist
    ##   <chr>   <chr> <dbl>     <dbl>
    ## 1 control A       424    0.0786
    ## 2 control E      4364    0.809 
    ## 3 control <NA>    608    0.113 
    ## 4 screen  A       180    0.0259
    ## 5 screen  E      6208    0.892 
    ## 6 screen  <NA>    575    0.0826

### Model calculations

Calculate incidence rate (*λ*<sub>*d*</sub>), rate of late-stage cancer
(*p*<sub>1</sub>) and reduction in late-stage (*α*)

``` r
# Incidence from paper abstract
total_cases_ctrl <- sum(filter(trial_counts_ea, arm=='control')$cases)
lambda_d <- total_cases_ctrl / py_ctrl
lambda_d
```

    ## [1] 0.005783172

``` r
adv_cases_ctrl   <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(cases)
adv_cases_screen <- trial_counts_ea %>% filter(arm=='screen', late=='A') %>% pull(cases)
alpha <- 1 - (adv_cases_screen / py_screen) / (adv_cases_ctrl / py_ctrl)
alpha
```

    ## [1] 0.4816934

``` r
p_1 <- trial_counts_ea %>% filter(arm=='control', late=='A') %>% pull(case_dist)
p_1
```

    ## [1] 0.07857672

Calculate overall mortality hazard implied by case fatality rate

``` r
fatality_rate <- total_deaths_ctrl / total_cases_ctrl
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

    ## [1] 0.01633334

### Calculate calibrated SEER mortality hazards

Import SEER data

``` r
# Import and clean raw data

seer_stage_map <- tribble(
  ~stage,  ~late,
  'M0',    'E',
  'M1',    'A',
  'M1a',   'A',
  'M1b',   'A',
  'M1c',   'A',
  'M1NOS', 'A'
)

seer <- read_csv('../data/seer-mortality-erspc.csv', 
                 skip=1,
                 col_names = c('page', 'stage', 'interval', 'n', 'median', 'surv', 'se', 'lb', 'ub')) %>%
  mutate(surv = surv/100) %>%
  left_join(seer_stage_map) %>%
  select(stage, late, n, surv)
seer
```

    ## # A tibble: 8 x 4
    ##   stage    late       n  surv
    ##   <chr>    <chr>  <dbl> <dbl>
    ## 1 M0       E     152830 0.952
    ## 2 M1a      A        261 0.328
    ## 3 M1b      A       2925 0.191
    ## 4 M1c      A       1188 0.167
    ## 5 M1NOS    A        138 0.147
    ## 6 <NA>     <NA>      27 0.299
    ## 7 MX       <NA>    5741 0.849
    ## 8 Blank(s) <NA>   94698 0.920

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
    ##   late   surv      n hazard_raw
    ##   <chr> <dbl>  <dbl>      <dbl>
    ## 1 A     0.191   4512    0.150  
    ## 2 E     0.952 152830    0.00447

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

    ## [1] 3.064077

``` r
lambda_0 <- adj_factor * seer_hazards %>% filter(late=='E') %>% pull(hazard_raw)
lambda_0
```

    ## [1] 0.01370791

``` r
lambda_1 <- adj_factor * seer_hazards %>% filter(late=='A') %>% pull(hazard_raw)
lambda_1
```

    ## [1] 0.4606999

### Final calculations

``` r
r_0 <- calc_r(trial_length, lambda_d, lambda_0)
r_1 <- calc_r(trial_length, lambda_d, lambda_1)
rho <- calc_rho(trial_length, r_0, r_1, p_1)
mrel <- rho * alpha

output <- tribble(
  ~trial, ~lambda_d, ~lambda_0, ~lambda_1, ~p_1, ~rho, ~alpha, ~mrel, ~actual, ~lb, ~ub,
  'erspc', lambda_d, lambda_0, lambda_1, p_1, rho, alpha, mrel, actual, lb, ub
)
output
```

    ## # A tibble: 1 x 11
    ##   trial lambda_d lambda_0 lambda_1    p_1   rho alpha  mrel actual    lb    ub
    ##   <chr>    <dbl>    <dbl>    <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1 erspc  0.00578   0.0137    0.461 0.0786 0.444 0.482 0.214   0.21  0.09  0.32

``` r
write_csv(output, '../output/trial-calculations-erspc.csv')
```
