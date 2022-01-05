# Core functions for stage shift model

# Notation: 
# Var    | Description
#-------------------------------------------------
# t      | Time period length
# ld     | Disease incidence hazard
# lk     | Stage-specifc mortality hazard 
# rho    | Stage-shift to mortality 
#        |    improvement multiplier
# r      | Stage-specific cumulative mortality
#        |    under all-stage incidence
# Mrel   | Relative mortality improvement


# Calculate r-value for a specific stage
calc_r <- function(t, ld, lk) {
  return(1 - (lk * exp(-t*ld) - ld * exp(-t*lk)) / (lk - ld))
}


# Calculate stage-shift to mortality improvement multiplier 
# under early/advanced stage assumption
calc_rho <- function(t, r0, r1, P) {
  return(P * (r1 - r0) / (P*r1 + (1-P)*r0))
}

