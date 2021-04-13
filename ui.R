library(shiny)

# shinyUI(pageWithSidebar(
fluidPage(
  
  tabsetPanel(
    tabPanel("Plot",
             headerPanel("Plot the distribution"),
             # Sidebar with controls to select a dataset and specify the number
             # of observations to view
             sidebarPanel(
               
               numericInput("obs", "Number of observations (n):", 1000),
               selectInput(
                 "dist", "Distribution",
                 c(Uniform = "uniform",
                   Gaussian = "gaussian",
                   Gamma = "gamma",
                   Cauchy = "cauchy",
                   "Half Cauchy" = "hcauchy",
                   "Bernoulli" = "bernoulli",
                   Binomial = "binomial",
                   "Geometric" = "geometric",
                   "Negative Binomial" = "negbinom",
                   "Poisson" = "poisson")
               ),
               
               conditionalPanel(
                 condition = "input.dist == 'uniform'",
                 numericInput("unif_min", "Min (a):", 1, step = 1),
                 numericInput("unif_max", "Max (b):", 10, step = 1),
                 withMathJax(),
                 uiOutput('unif_pdf')
               ),
               
               conditionalPanel(
                 condition = "input.dist == 'gaussian'",
                 numericInput("gaus_mean", "Mean:", 0, step = 0.1),
                 numericInput("gaus_var", "Variance:", 1, step = 0.1),
                 withMathJax(),
                 uiOutput('gaus_pdf')
               ),
               
               conditionalPanel(
                 condition = "input.dist == 'gamma'",
                 numericInput("gamma_shape", "Shape:", 4, step = 0.1),
                 numericInput("gamma_scale", "Scale (1/Shape):", 1, step = 0.1),
                 withMathJax(),
                 uiOutput('gamma_pdf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'cauchy'",
                 numericInput("cauc_loc", "Location:", 0, step = 0.1),
                 numericInput("cauc_scale", "Scale:", 1, step = 0.1),
                 withMathJax(),
                 uiOutput('cauchy_pdf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'hcauchy'",
                 numericInput("hcauc_scale", "Scale:", 1, step = 0.1)
               ),
               # discrete
               conditionalPanel(
                 condition = "input.dist == 'bernoulli'",
                 numericInput("bern_prob", "Probability:", 0.5, step = 0.1, min = 0, max = 1),
                 withMathJax(),
                 uiOutput('bern_pmf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'binomial'",
                 numericInput("binom_n", "N:", 10),
                 numericInput("binom_prob", "Probability:", 0.5, step = 0.1, min = 0, max = 1),
                 withMathJax(),
                 uiOutput('binom_pmf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'geometric'",
                 numericInput("geom_prob", "Probability:", 0.5, step = 0.1, min = 0, max = 1),
                 withMathJax(),
                 uiOutput('geom_pmf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'poisson'",
                 numericInput("pois_lambda", "Lambda:", 1, step = 0.1),
                 withMathJax(),
                 uiOutput('pois_pmf')
               ),
               conditionalPanel(
                 condition = "input.dist == 'negbinom'",
                 numericInput("negbinom_mean", "Mean:", 0.5, step = 0.1, min = 0, max = 1),
                 numericInput("negbinom_shape", "Shape:", 0.5, step = 0.1, min = 0, max = 1),
                 withMathJax(),
                 uiOutput('negbinom_pmf')
               )
             ),
             
             # Show a summary of the dataset and an HTML table with the requested
             # number of observations
             mainPanel(
               verbatimTextOutput("summary"),
               plotOutput("distPlot"), 
               plotOutput("ggPlot"), 
               tableOutput("view"),
             )
             
    ),
    tabPanel("Bayes",
             headerPanel("Simple Bayes with Gaussian prior and count data"),
             # 
             # # Sidebar with controls to select a dataset and specify the number
             # # of observations to view
             sidebarPanel(
               
               numericInput("total_n", "Total size:", 10, step = 1),
               numericInput("obs_ct", "Observed count:", 0, step = 1),
               sliderInput("pr_gaus_mean", "Prior: Gaussian, Mean:", min = 0, max = 1, value = 0, step = 0.1),
               sliderInput("pr_gaus_sd", "Prior: Gaussian, SD:", min = 0, max = 1, value = 0, step = 0.01)
             ),
             
             #            conditionalPanel(
             #              condition = "input.dist == 'gaussian'",
             #              sliderInput("gaus_mean", "Mean:", min = -1, max = 1, value = 0, step = 0.2),
             #              sliderInput("gaus_sd", "SD:", min = 0, max = 0.1, value = 0, step = 0.01)
             #            )
             #          ),
             #          
             #          # Show a summary of the dataset and an HTML table with the requested
             #          # number of observations
             mainPanel(
               plotOutput("post")
             )
             #          #   )
             # )
    )
  )
)
