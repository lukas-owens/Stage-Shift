library(tidyverse)
library(ggplot2)
library(gridExtra)

# Read data from split trials calculations
trial_data <- rbind(
  read_csv('../output/trial-calculations-nlst.csv'),
  read_csv('../output/trial-calculations-erspc.csv'),
  read_csv('../output/trial-calculations-ukctocs.csv'))

# Function for plotting single trial results
# plot_mort_imprv <- function(slope, alpha, actual, lb, ub, title) {
plot_mort_imprv <- function(trialname, title) {
  row <- trial_data %>% filter(trial == trialname)
  slope <- row %>% pull(rho)
  alpha <- row %>% pull(alpha)
  actual <- row %>% pull(actual)
  lb <- max(row %>% pull(lb),0) # Floor at 0 for display
  ub <- row %>% pull(ub)
  
  end_points <- tibble(alpha = c(0, 1), mortality_reduction = c(0, slope))
  point <- tibble(alpha = c(alpha), mortality_reduction = c(slope * alpha))
  
  lim <- 0.5
  output <- ggplot() + 
    geom_segment(aes(x=0, y=0, xend=lim, yend=lim*slope)) +
    geom_segment(aes(x=0, y=0, xend=lim, yend=lim), linetype='dashed') +
    geom_point(aes(x=alpha, y=(alpha*slope)), size=3, shape=1) +
    geom_point(aes(x=alpha, y=actual), size=3, shape=3) +
    geom_errorbar(aes(x=alpha, ymin=lb, ymax=ub), width=0.02)+
    scale_y_continuous(limits = c(0,lim), labels = scales::percent_format(accuracy = 5L)) +
    scale_x_continuous(limits = c(0,lim), labels = scales::percent_format(accuracy = 5L)) + 
    xlab("Reduction in late-stage disease") + 
    ylab("Reduction in mortality") +
    labs(title=title) + 
    theme_classic() + 
    theme(plot.title = element_text(size=12),
          text=element_text(size=10))
  
  return(output)
}

# Plot each trial
g1 <- plot_mort_imprv('nlst', title="Lung Cancer - NLST") 
g2 <- plot_mort_imprv('erspc', title="Prostate Cancer - ERSPC") 
g3 <- plot_mort_imprv('ukctocs', title="Ovarian Cancer - UKCTOCS") 

# Save output
pdf(file='../output/figure2.pdf', width=6, height=5)
grid.arrange(g1, g2, g3, nrow=2)
dev.off()
