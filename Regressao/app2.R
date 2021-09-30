library(shiny)

dados = read.csv("slr12.csv",sep = ";")
modelo = lm(CusInic ~ FrqAnual, data=dados)


# Define UI for application that draws a histogram
ui <- fluidPage(

    
    titlePanel("Previsao de Custo Inicial para Montar uma Franquia " ),
    
    fluidRow(
      column(4,
      h2("Dados"),
      tableOutput("Dados")
      ),
      column(8,
      plotOutput("Graf")
      )
    ),
    fluidRow(
        column(6,
        h3("Valor Anual da Franquia"),
        numericInput("Novo Valor","Insira Novo Valor", 1500, min=1, max=999999),
        actionButton("Processar", "Processar")
        ),
        column(6,
        h1(textOutput("Resultado"))
        )
    )
)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Graf = renderPlot({
        plot(CusInic ~ FrqAnual, data=dados)
        abline(modelo)
    })
    output$Dados = renderTable({head(dados, 10)})
    
    observeEvent(input$Processar, {
        valr = input$NovoValor
        prev = predict(modelo, data.frame(FrqAnual = eval(parse(text = val))))
        prev = paste0( "Previsao de Custo Inicial R4: ", round(prev, 2))
        output$Resultado = renderText({prev})
    }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
