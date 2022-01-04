# Core functions for stage shift model

# Notation: 
# Var    | Description
#-------------------------------------------------
# t      | Time period length
# ld     | Disease incidence hazard
# lk     | Stage-specifc mortality hazard 
# K      | Stage-shift to mortality 
#        |    improvement multiplier
# r      | Stage-specific cumulative mortality
#        |    under all-stage incidence
# Mimp   | Relative mortality improvement


# Calculate r-value for a specific stage
calc_r <- function(t, ld, lk) {
  return(1 - (lk * exp(-t*ld) - ld * exp(-t*lk)) / (lk - ld))
}


# Calculate stage-shift to mortality improvement multiplier 
# under early/advanced stage assumption
calc_K <- function(t, ld, le, la, P) {
  re <- calc_r(t, ld, le)
  ra <- calc_r(t, ld, la)
  return(P * (ra - re) / (P*ra + (1-P)*re))
}

