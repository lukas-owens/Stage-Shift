library(tidyverse)
library(ggplot2)
library(gridExtra)


plot_mort_imprv <- function(slope, alpha, actual, lb, ub, title) {
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


g1 <- plot_mort_imprv(slope=0.4754, alpha=0.21, actual=0.20, lb=0.07, ub=0.27, title="Lung Cancer - NLST") 
g2 <- plot_mort_imprv(slope=0.4172, alpha=0.48, actual=0.21, lb=0.09, ub=0.32, title="Prostate Cancer - ERSPC") 
g3 <- plot_mort_imprv(slope=0.7100, alpha=0.11, actual=0.04, lb=0.00, ub=0.17, title="Ovarian Cancer - UKCTOCS") 


pdf(file='output\\figure2.pdf', width=6, height=5)
grid.arrange(g1, g2, g3, nrow=2)
dev.off()
