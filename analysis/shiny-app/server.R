# Server 
library(tidyverse)
library(scales)
library(shiny)
library(xtable)

# Helper functions

# Calculate r-value for a specific stage
calc_r <- function(t, ld, lk) {
  return(1 - (lk * exp(-t*ld) - ld * exp(-t*lk)) / (lk - ld))
}

# Calculate stage-shift to mortality improvement multiplier 
# under early/advanced stage assumption
calc_rho <- function(t, ld, l0, l1, P) {
  r0 <- calc_r(t, ld, l0)
  r1 <- calc_r(t, ld, l1)
  return(P * (r1 - r0) / (P*r1 + (1-P)*r0))
}

# Main plotting function
plot_mort_imprv <- function(rho, alpha, incl_actual=FALSE, actual=0, lb=0, ub=0) {
  
  end_points <- tibble(alpha = c(0, 1), mortality_reduction = c(0, rho))
  point <- tibble(alpha = c(alpha), mortality_reduction = c(rho * alpha))
  
  lim <- 1.0
  output <- ggplot() + 
    geom_segment(aes(x=0, y=0, xend=lim, yend=lim*rho)) +
    geom_segment(aes(x=0, y=0, xend=lim, yend=lim), linetype='dashed') +
    geom_point(aes(x=alpha, y=(alpha*rho)), size=5, shape=1) +
    scale_y_continuous(limits = c(0,lim), labels = scales::percent_format(accuracy = 5L)) +
    scale_x_continuous(limits = c(0,lim), labels = scales::percent_format(accuracy = 5L)) + 
    xlab("Reduction in late-stage disease") + 
    ylab("Reduction in mortality") +
    theme_classic()  +
    theme(text=element_text(size=14))
  
  if(incl_actual){
    output <- output + 
      geom_point(aes(x=alpha, y=actual), size=3, shape=3) +
      geom_errorbar(aes(x=alpha, ymin=lb, ymax=ub), width=0.02)
    
  }
  return(output)
}


# Run server
function(input, output, session) {
  
  output$plot <- renderPlot({
    rho <- calc_rho(input$t, input$ld, input$l0, input$l1, input$P)
    plot_mort_imprv(rho, input$alpha, FALSE)
  })
  
  output$table <- renderUI({
    r0 <- calc_r(input$t, input$ld, input$l0)
    r1 <- calc_r(input$t, input$ld, input$l1)
    rho <- calc_rho(input$t, input$ld, input$l0, input$l1, input$P)
    mrel <- rho * input$alpha
    
    table <- data.frame(value = c(format(r0, digits=2),
                                  format(r1, digits=2),
                                  format(rho, digits=3),
                                  percent(mrel)
    ))
    rownames(table) <- c('r_0',
                         'r_1',
                         '\\rho',
                         'M^{\\mathrm{rel}}')
    LaTeXtab <- print(xtable(table, align=rep("c", ncol(table)+1)), 
                      floating=FALSE,
                      include.colnames=FALSE,
                      tabular.environment="array",
                      comment=FALSE, 
                      print.results=FALSE, 
                      sanitize.rownames.function = function(x) x,
                      hline.after=NULL)
    tagList(
      withMathJax(),
      HTML(paste0("$$", LaTeXtab, "$$"))
    )
  })
}
