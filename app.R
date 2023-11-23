library(shiny)
library(rugarch)

dados <- read.table("2004-2021.tsv", header = TRUE, sep = "\t")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Parte 1", titlePanel("Parte 1")),

    tabPanel("Modelo Preditivo",
      titlePanel("ARMA(p, q) - GARCH(1, 1)"),

      sidebarLayout(
        sidebarPanel(width = 2,
          uiOutput("escolhas_regiao"),
          uiOutput("escolhas_estado"),
          uiOutput("escolhas_produto"),
          sliderInput("p", "p:", value = 1, min = 1, max = 15),
          sliderInput("q", "q:", value = 1, min = 1, max = 15),
          radioButtons("distribuicao", "Distribuição:", c("Normal", "t de Student")),
          sliderInput("n_dias", "Número de dias:", value = 1, min = 1, max = 30),
          downloadButton("download", "Download")
        ),
        mainPanel(width = 10,
          tabsetPanel(
            tabPanel("Estimação",
              fluidRow(
                column(width = 4, plotOutput("grafico_precos")),
                column(width = 4, plotOutput("grafico_ACF_retornos_c_quadrado")),
                column(width = 4, plotOutput("grafico_ACF_retornos"))
              ),
              fluidRow(
                column(width = 4, plotOutput("grafico_retornos")),
                column(width = 4, verbatimTextOutput("teste_ljung_box")),
                column(width = 4, plotOutput("grafico_PACF_retornos"))
              )
            ),
            tabPanel("Diagnóstico",
              fluidRow(
                column(width = 4, verbatimTextOutput("modelo_arma_garch")),
                tags$head(tags$style("#modelo_arma_garch{font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 800px;}")),
                column(width = 8,
                  fluidRow(plotOutput("grafico_qq")),
                  fluidRow(
                    column(width = 6, plotOutput("grafico_ACF_residuos")),
                    column(width = 6, plotOutput("grafico_ACF_residuos_quadrado"))
                  ))
              )
            ),
            tabPanel("Previsão",
              tableOutput("previsao")
            )
          )
        )
      )
    )

  )
)

server <- function(input, output, session) {

  # Entrada
  output$escolhas_regiao <- renderUI({
    selectInput("regiao", "Região:", unique(dados$REGIÃO))
  })
  output$escolhas_estado <- renderUI({
    dados <- dados[dados$REGIÃO == input$regiao, ]
    selectInput("estado", "Estado:", choices = unique(dados$ESTADO))
  })
  output$escolhas_produto <- renderUI({
    dados <- dados[dados$ESTADO == input$estado, ]
    selectInput("produto", "Produto:", choices = unique(dados$PRODUTO))
  })

  # Saída - Estimação
  dados_modelagem <- reactive({
    dados[dados$ESTADO == input$estado & dados$PRODUTO == input$produto, ]
  })
  precos <- reactive({
    as.ts(dados_modelagem()$PREÇO.MÉDIO.REVENDA, Daily, as.Date(dados_modelagem()$DATA.FINAL))
  })
  retornos <- reactive({
    as.ts(diff(dados_modelagem()$PREÇO.MÉDIO.REVENDA)/dados_modelagem()$PREÇO.MÉDIO.REVENDA[-1])
  })
  retornos_c <- reactive({
    scale(retornos(), center = TRUE, scale = TRUE)
  })
  output$grafico_precos <- renderPlot({
    ts.plot(precos())
  })
  output$grafico_retornos <- renderPlot({
    ts.plot(retornos())
  })
  output$grafico_ACF_retornos_c_quadrado <- renderPlot({
    acf(retornos_c()^2)
  })
  output$teste_ljung_box <- renderPrint({
    Box.test(retornos()^2, type = "Ljung-Box", lag = 10)
  })
  output$grafico_ACF_retornos <- renderPlot({
    acf(retornos())
  })
  output$grafico_PACF_retornos <- renderPlot({
    pacf(retornos())
  })

  # Sáida - Diagnóstico
  fit <- reactive({
    if (input$distribuicao == "Normal") {
      distribution = "norm"
    } else {
      distribution = "std"
    }
    spec <- ugarchspec(mean.model = list(armaOrder = c(input$p, input$q), include.mean = FALSE),
               variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
               distribution = distribution)
    ugarchfit(spec, retornos(), solver = "hybrid")
  })
  e_hat <- reactive({
    fit()@fit$residuals/fit()@fit$sigma
  })
  output$modelo_arma_garch <- renderPrint({
    fit()
  })
  output$grafico_qq <- renderPlot({
    plot(fit(), which = 9)
  })
  output$grafico_ACF_residuos <- renderPlot({
    acf(e_hat())
  })
  output$grafico_ACF_residuos_quadrado <- renderPlot({
    acf(e_hat()^2)
  })

  # Saída - Previsão
  previsoes <- reactive({
    datas <- format(rep(Sys.Date(), input$n_dias) + 0:(input$n_dias-1), "%Y-%m-%d")
    previsao <- ugarchforecast(fit(), n.ahead = input$n_dias)
    series <- formatC(as.vector(previsao@forecast[["seriesFor"]]), digits = 9)
    sigmas <- formatC(as.vector(previsao@forecast[["sigmaFor"]]), digits = 9)
    data.frame(data = datas, retorno = series, sigma = sigmas)
  })
  output$previsao <- renderTable({
    previsoes()
  })
  output$download <- downloadHandler(
    filename = function(){"previsoes.csv"},
    content = function(fname){
      write.csv(previsoes(), fname, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)