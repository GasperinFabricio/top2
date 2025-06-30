library(shiny)

# carregando modelo
modelo <- readRDS("modelo_predict_defects.rds")

# interface
ui <- fluidPage(
  titlePanel("Previsão de Defeitos por linhas de código - LOC"),
  sidebarLayout(
    sidebarPanel(
      numericInput("loc", "Linhas de Código (LOC):", value = 100),
      actionButton("prever", "Prever")
    ),
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)

# server
server <- function(input, output) {
  observeEvent(input$prever, {
    pred <- predict(modelo, newdata = data.frame(sumLOC_TOTAL = input$loc))
    output$resultado <- renderPrint({
      paste("Número estimado de defeitos:", round(pred, 2))
    })
  })
}

shinyApp(ui = ui, server = server)
