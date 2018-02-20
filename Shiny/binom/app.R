library(shiny)
library(binom)

ui <- fluidPage(
  p("This app shows the coverage probability for four types of 
    confidence intervals for the population proportion"),
  tags$hr(""),
  sidebarLayout(
    sidebarPanel(
      h3("Inputs:"),
      numericInput(inputId = "n", label = "Select number of bernoulli trials (n):", value = 20, min = 2, max = 1000000),
      sliderInput(inputId = "conf", label = "Select confidence level:", value = 0.95, min = 0.800, max = 0.999),
      selectInput("METH", "Choose a method:", 
                  choices = c("Asymptotic", "Wilson", "Agresti-Coull", "Exact"))
    ),
    mainPanel(
      plotOutput(outputId = "binoplot")
    ),
  )
)

server <- function(input, output) {
  
  # Return the requested Method
  methInput <- reactive({
    switch(input$METH,
           "Asymptotic" = binom.asymp,
           "Wilson" = binom.wilson,
           "Agresti-Coull" = binom.agresti.coull,
           "Exact" = binom.exact)
  })
  
  output$binoplot <- renderPlot({
    binom.plot(input$n, method = methInput(), input$conf, np = 2000)
  })
}


shinyApp(ui = ui, server = server)