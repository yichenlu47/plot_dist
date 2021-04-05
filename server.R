library(shiny)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  output$unif_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{b-a}$$'))
  })
  output$gaus_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{\\sqrt{2 \\pi}\\sigma} \\exp{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$'))
  })
  output$gamma_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{\\gamma(\\alpha)\\beta^{\\alpha}} x^{\\alpha-1} \\exp{-\\frac{x}{\\beta}}$$'))
  })
  
  # Return the requested dataset
  dataInput <- reactive({
    switch(input$dist,
           "uniform" = runif(input$n, min = input$unif_min, max = input$unif_max),
           "gaussian" = rnorm(input$n, mean = input$gaus_mean, sd = sqrt(input$gaus_var)),
           "gamma" = rgamma(input$n, shape = input$gamma_shape, scale = input$gamma_scale)
    )
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    data <- dataInput()
    summary(data)
  })
  
  output$distPlot <- renderPlot({
    data <- dataInput()
    # generate an rnorm distribution and plot it
    hist(data, breaks = 50)
  })
  
  # Show the first "n" observations
  # output$view <- renderTable({
  #   head(dataInput(), n = 4)
  # })
  
  
})