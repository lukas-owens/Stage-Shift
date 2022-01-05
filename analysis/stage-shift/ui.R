# Shiny app for calculating mortality improvements
library(tidyverse)
library(shiny)

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
                   value = 0.10,
                   step  = 0.01),
      numericInput('l1', 
                   'Late-stage mortality hazard \\((\\lambda_1)\\)',
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
                  'Reduction in late-stage cancer \\((\\alpha)\\)',
                  min   = 0.00,
                  max   = 1.00,
                  value = 0.20,
                  step  = 0.01)
    ),
    
    
    # Main panel and graph

    mainPanel(tabsetPanel(
      tabPanel('Intro', 
               h3('Introduction'),
               HTML(paste0(
                 'This app implements the framework for evaluating the impact of estimating the impact of stage-shift ',
                 'on disease-specific mortality in cancer screening trials described in <em>Owens et al</em>. The user should ',
                 'read that paper in its entirety before using this tool.'
                 )),
               h3('Summary and Instructions'),
               p(paste0('This tool allows the user to approximate the reduction in disease-specific mortality associated with the implementation ',
                        'associated with a reduction in late-stage disease following the implementation of a screening protocol in a given population. ',
                        'The inputs needed are specified in the sidebar to the left. The hazards of diagnosis and post-diagnosis mortality are assumed to be constant ',
                        'over time and equal in screened and unscreened populations. A table of outputs of the model are displayed in the Output tab, along with a graphical representation. See the publication cited below for ',
                        'further definition and discussion of the inputs and outputs of this tool.'
                        )),
               h3('Citation'),
               p('Lukas Owens, Roman Gulati, Ruth Etzioni (2022), ', em('Stage Shift as an Endpoint in Cancer Screening Trials: Implications for Multi-Cancer Early Detection')),
               h3('Contact'),
               p('For questions and comments, please contact:'),
               HTML(paste0('Lukas Owens<br/>',
                           'Fred Hutchinson Cancer Research Center<br/>',
                           '<a href="mailto:lowens2@fredhutch.org">lowens2@fredhutch.org</a>')
               )),
      tabPanel('Plot', plotOutput('plot'), uiOutput('table'))
    )
  )
)
)