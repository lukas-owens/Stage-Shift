# Shiny app for calculating mortality improvements
library(tidyverse)
library(shiny)

# User interface

fixedPage(

  # Title
  titlePanel('Stage Shifting and Mortality Improvement'),
  
  # Body
  
  fixedRow(
  
    # Sidebar
    column(4,
      withMathJax(),
      numericInput('t', 
                   'Years of follow-up \\((T)\\)',
                   min   = 0,
                   value = 10,
                   step  = 1),
      numericInput('ld', 
                   'Incidence hazard rate \\((\\lambda_d)\\)',
                   min   = 0.0,
                   value = 0.0001,
                   step  = 0.0001),
      numericInput('l0', 
                   'Early-stage mortality hazard \\((\\lambda_0)\\)',
                   min   = 0.00,
                   value = 0.10,
                   step  = 0.01),
      numericInput('l1', 
                   'Early-stage mortality hazard \\((\\lambda_0)\\)',
                   min   = 0.00,
                   value = 0.40,
                   step  = 0.01),
      numericInput('P', 
                   'Percent late-stage cancer \\((p_1)\\)',
                   min   = 0.00,
                   max   = 1.00,
                   value = 0.20,
                   step  = 0.01),
      sliderInput('alpha', 
                  'Reduction in late-stage cancer under screening \\((\\alpha)\\)',
                  min   = 0.00,
                  max   = 1.00,
                  value = 0.20,
                  step  = 0.01),
    ),
    
    
    # Main panel and graph

    column(4, uiOutput('table')),
    column(4, plotOutput('plot'))
    )
  )