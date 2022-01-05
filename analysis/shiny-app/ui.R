# Shiny app for calculating mortality improvements
library(tidyverse)
library(shiny)

intro_text <- read_file('intro.html')

# User interface

fixedPage(

  # Title
  titlePanel('Stage-shift model of cause-specific mortality reduction in cancer screening trials'),
  
  # Body
  
  sidebarLayout(
  
    # Sidebar
    sidebarPanel(
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
                   value = 0.05,
                   step  = 0.01),
      numericInput('l1', 
                   'Late-stage mortality hazard \\((\\lambda_1)\\)',
                   min   = 0.00,
                   value = 0.30,
                   step  = 0.01),
      sliderInput('P', 
                   'Percent late-stage cancer \\((p_1)\\)',
                   min   = 0.00,
                   max   = 1.00,
                   value = 0.40,
                   step  = 0.01),
      sliderInput('alpha', 
                  'Reduction in late-stage cancer \\((\\alpha)\\)',
                  min   = 0.00,
                  max   = 1.00,
                  value = 0.25,
                  step  = 0.01)
    ),
    
    
    # Main panel and graph
    mainPanel(tabsetPanel(
      tabPanel('Intro', HTML(intro_text)),
      tabPanel('Output', fluidRow(
               column(6,plotOutput('plot', width=500, height=400)), 
               column(6,uiOutput('table')))
      )
    )
  )
)
)