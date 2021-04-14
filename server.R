library(shiny)
library(ggplot2)
library(MASS)
library(extraDistr)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  output$unif_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{b-a}$$', '$E(x) = \\frac{b + a}{2} = $', (input$unif_max + input$unif_min)/2))
  })
  output$gaus_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{\\sqrt{2 \\pi}\\sigma} \\exp{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$',
                         '$E(x) = \\mu = $', input$gaus_mean,
                         '$Var(x) = \\sigma^2 = $',  input$gaus_var))
  })
  output$gamma_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{\\gamma(\\alpha)\\beta^{\\alpha}} x^{\\alpha-1} \\exp{-\\frac{x}{\\beta}}$$',
                         'example: X = amount of rainfall accumulated in a reservoir or the size of aggregate insurance claims, always positive, skewed'))
  })
  output$cauchy_pdf <- renderUI({
    withMathJax(helpText('$$f(x) = \\frac{1}{\\gamma(\\alpha)\\beta^{\\alpha}} x^{\\alpha-1} \\exp{-\\frac{x}{\\beta}}$$',
                         'Median =', input$cauc_loc))
  })
  
  output$bern_pmf <- renderUI({
    withMathJax(helpText('$$p(x) = p^x(1-p)^{1-x}$$','$E(x) = p = $', input$bern_prob))
  })
  output$binom_pmf <- renderUI({
    withMathJax(helpText('$$p(x) =C^{n}_{x} p^x(1-p)^{1-x}$$','$E(x) = np = $', input$binom_n * input$binom_prob))
  })
  
  output$geom_pmf <- renderUI({
    withMathJax(helpText('$$p(x) =(1-p)^{x-1} p$$','$E(x) = \\frac{1}{p} =$', 1/input$geom_prob,
                         'example: X = the number of independent Bernoulli trials, each with probability p of success, needed to observe the first success'))
  })
  
  output$pois_pmf <- renderUI({
    withMathJax(helpText('$$p(X) = \\frac{1}{x!} exp^{-\\lambda} \\lambda^x$$',
                         '$E(X) = \\lambda = $', input$pois_lambda,
                         'example: \u03bb = 1, average infection rate at 1 patient per day;
                         X = number of patients infected in a given day;
                         P(X = 5), probability of having 5 patients infected in a given day'))
    
  })
  
  output$negbinom_pmf <- renderUI({
    withMathJax(helpText('$$p(X) = C^{x+r-1}_{r-1} p^r (1-p)^{x}$$',
                         '$E(X) = \\frac{r(1-p)}{p} = $', (1-input$negbinom_mean)* input$negbinom_shape/input$negbinom_mean,
                         '$Var(X) = \\frac{r(1-p)}{p^2}= $', (1-input$negbinom_mean)* input$negbinom_shape/(input$negbinom_mean^2),
                         'example: X = failure events ocurring prior to reaching r successful events in a sequence of Bernouli trias of success probability p'))
  })
  
  # Return the requested dataset
  dataInput <- reactive({
    switch(input$dist,
           "uniform" = runif(input$obs, min = input$unif_min, max = input$unif_max),
           "gaussian" = rnorm(input$obs, mean = input$gaus_mean, sd = sqrt(input$gaus_var)),
           "gamma" = rgamma(input$obs, shape = input$gamma_shape, scale = input$gamma_scale),
           
           "cauchy" = rcauchy(input$obs, location = input$cauc_loc, scale = input$cauc_scale),
           "hcauchy" = rhcauchy(input$obs, sigma = input$hcauc_scale),
           
           "bernoulli" = rbinom(input$obs, size = 1, prob = input$bern_prob),
           "binomial" = rbinom(input$obs, size = input$binom_n, prob = input$binom_prob),
           "geometric" = rgeom(input$obs, prob = input$geom_prob),
           "poisson" = rpois(input$obs, lambda = input$pois_lambda),
           "negbinom" = rnegbin(input$obs, mu = input$negbinom_mean, theta = input$negbinom_shape)
    )
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    data <- dataInput()
    summary(data)
  })
  # 
  output$distPlot <- renderPlot({
    data <- dataInput()
    # generate an rnorm distribution and plot it
    hist(data, breaks = 50)
  })
  
  output$ggPlot <- renderPlot({
    data <- as.data.frame(dataInput())
    colnames(data) = "x"
    ggplot(data, aes(x=x)) + geom_density(alpha=0.4, fill="lightskyblue")+
      geom_vline(aes(xintercept = mean(x)), color="orange", linetype="dashed", size = 1) +
      geom_text(mapping = aes(x = mean(x), y = 0.05, label = paste0("Mean ", round(mean(x), 2), "\nVar ", round(var(x), 2)), hjust = - 0.5), color  = "orange") + 
      
      
      geom_vline(aes(xintercept = median(x)), color="deepskyblue", linetype="dashed", size = 1) +  
      geom_text(mapping = aes(x = median(x), y = 0.05, label = paste0("Median:", round(median(x), 2)), hjust = 1.5), color  = "deepskyblue") + 
      labs(title="Density curve", y = "Density")+
      theme_classic()
  })
  
  # posterior
  output$post <- renderPlot({
    rangeP <- seq(0, 1, length.out = 101)
    lik <- dbinom(x = input$obs_ct, prob = rangeP, size = input$total_n)
    
    prior <- dnorm(x = rangeP, mean = input$pr_gaus_mean, sd = input$pr_gaus_sd)/15
    post <- lik * prior / sum(lik * prior)
    
    plot(rangeP, lik, type = "l", xlab = "P(Black)", ylab = "Density") # likelihood
    lines(rangeP, prior, col = "red") # prior
    lines(rangeP, post, col = "blue")
    legend("topleft", legend = c("Lik", "Prior", "Post"),
           text.col = c("black", "red", "blue"), bty = "n")
  })
  # Show the first "n" observations
  # output$view <- renderTable({
  #   head(dataInput(), n = 4)
  # })
  
  
})