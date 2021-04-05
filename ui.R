library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Text"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    
    numericInput("n", "Number of observations (n):", 1000),
    # selectInput("dist", "Choose a distribution:", 
    #             choices = c("uniform", "gaussian", "cauchy")),
    selectInput(
      "dist", "Distribution",
      c(Uniform = "uniform",
        Gaussian = "gaussian",
        Gamma = "gamma")
    ),
    
    conditionalPanel(
      condition = "input.dist == 'uniform'",
      numericInput("unif_min", "Min (a):", 1),
      numericInput("unif_max", "Max (b):", 10),
      withMathJax(),
      uiOutput('unif_pdf')
    ),
    
    conditionalPanel(
      condition = "input.dist == 'gaussian'",
      numericInput("gaus_mean", "Mean ($$\\beta$$):", 0),
      numericInput("gaus_var", "Variance ($$\\sigma^2$$):", 1),
      withMathJax(),
      uiOutput('gaus_pdf')
    ),
    
    conditionalPanel(
      condition = "input.dist == 'gamma'",
      numericInput("gamma_shape", "Shape:", 4),
      numericInput("gamma_scale", "Scale:", 1),
      withMathJax(),
      uiOutput('gamma_pdf')
    )
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    verbatimTextOutput("summary"),
    plotOutput("distPlot"), 
    tableOutput("view")
    
  )
))