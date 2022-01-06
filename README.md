# Stage-Shift

## Overview
This code supports the paper "Stage Shift as an Endpoint in Cancer Screening Trials: Implications for Multi-Cancer Early Detection". It contains two parallel analyses using the framework described in that paper:
- Hypothetical trials (covering 7 common cancer types)
- Real-world trials (examining the NLST, the ERSPC, and the UKCTOCS

Additionally, a R shiny interface implements the framework, described further below.

## Hypothetical Trials
### Data
This part of the analysis relies on incidence and survival data from SEER. The queries and raw data are saved at the locations below.

SEER queries
- [seer-queries/incidence-rates-hypothetical.si](seer-queries/incidence-rates-hypothetical.si)
- [seer-queries/mortality-rates-hypothetical.ss](seer-queries/mortality-rates-hypothetical.ss)

SEER Data
- [data/seer-incidence-hypthetical.csv](data/seer-incidence-hypthetical.csv)
- [data/seer-mortality-hypthetical.csv](data/seer-mortality-hypthetical.csv)

### Analysis
The SEER data is run through the model in [analysis/seer.R](analysis/seer.R), which calculates the expected mortality improvement for each hypothetical trial. The main output is saved Table 1 and Figure 1 (saved in the [output](output) folder).

## Real-world Trials
This part of the analysis uses data from three published trials (NLST for lung cancer, ERSPC for prostate cancer, and UKCTOCS for ovarian cancer) to predict the mortality improvement from screening. The data and analysis for each trial are linked below. The results from the three trials are combined and visualized by [analysis/trials.R](analysis/trials.R).


- NLST
  - Data
     - Trial data: [trial-data-nlst.csv](trial-data-nlst.csv)
     - Supplemental SEER data: [seer-mortality-nlst.csv](seer-mortality-nlst.csv)
  - Analysis
    - Code: [analysis/trial-nlst.Rmd](analysis/trial-nlst.Rmd)
    - Summary: [analysis/trial-nlst.md](analysis/trial-nlst.md)
 - ERSPC
   - Data
     - Trial data: [trial-data-erspc.csv](trial-data-erspc.csv)
     - Supplemental SEER data: [seer-mortality-erspc.csv](seer-mortality-erspc.csv)
   - Analysis
     - Code: [analysis/trial-erspc.Rmd](analysis/trial-erspc.Rmd)
     - Summary: [analysis/trial-erspc.md](analysis/trial-erspc.md)
- UKCTOCS
  - Data
    - Trial data: [trial-data-ukctocs.csv](trial-data-ukctocs.csv)
    - Supplemental SEER data: not applicable
  - Analysis
    - Code: [analysis/trial-ukctocs.Rmd](analysis/trial-ukctocs.Rmd)
    - Summary: [analysis/trial-ukctocs.md](analysis/trial-ukctocs.md)

## Shiny App
We also created a Shiny app that implements the same framework used above in an interactive UI. This is saved in the [analysis/shiny-app](analysis/shiny-app) folder and is currently published at https://lukasowens.shinyapps.io/stage-shift/

