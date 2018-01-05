ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "MU", label = "Mean:", value = 0, min = -Inf, max = Inf),
      numericInput(inputId = "SIGMA", label = "SD:", value = 1, min = 0.0001, max = Inf),
      numericInput(inputId = "SIMS", label = "Simulate Values:", value = 1000, min = 1, max = 100000),
      textInput(inputId = "COLOR", label = "Color:", value = "hotpink")
    ),
    mainPanel(
      plotOutput(outputId = "histo")
    ),
  )
)

server <- function(input, output) {
  output$histo <- renderPlot({
    hist(rnorm(input$SIMS, input$MU, input$SIGMA), main = "Simulated Histogram", 
         xlab = "", col = input$COLOR, breaks = "Scott")
  })
}


shinyApp(ui = ui, server = server)