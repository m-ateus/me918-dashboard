library(shiny)
library(rugarch)
library(tidyverse)
library(geobr)
library(sf)

dados <- read.table(file.path("data", "2004-2021.tsv"), header = TRUE, sep = "\t")
caminho_arquivo <- file.path("data", "2004-2021.tsv")
dados_combustiveis <- read.delim(caminho_arquivo)
data <- read_tsv(file.path("data", "2004-2021.tsv"))

data$ESTADO = tolower(data$ESTADO)
data = data %>%
  mutate(ANO = year(`DATA FINAL`),
         MES = month(`DATA FINAL`)) %>%
  select(-`DATA INICIAL`,-`DATA FINAL`)
data$PRODUTO = iconv(data$PRODUTO, from = 'UTF-8', to = 'ASCII//TRANSLIT')

media_anual = tibble()
for(i in 2004:2021) {
  for (j in levels(as.factor(data$PRODUTO))) {
    for (k in levels(as.factor(data$ESTADO))) {
      media_anual= rbind(media_anual, c(i, j, k, NA))
    }
  }
}
rm(i)
rm(j)
rm(k)
colnames(media_anual)<- c("ANO", "PRODUTO", "ESTADO", "VALOR")
media_anual$ANO = as.integer(media_anual$ANO)

media_anual = left_join(media_anual, data %>%
                          group_by(ESTADO, ANO, PRODUTO) %>%
                          select(`PREÇO MÉDIO REVENDA`, MES) %>%
                          summarise(MEDIA = mean(`PREÇO MÉDIO REVENDA`)) %>%
                          mutate(PRODUTO = as.factor(PRODUTO)),
                        by = join_by(ANO, ESTADO, PRODUTO)) %>%
  select(-VALOR)

#Data base geobr
states = read_state(year = 2010, showProgress = FALSE)
states$name_state = tolower(iconv(states$name_state, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

#Juntando as bases
media_anual = left_join(states, media_anual, by = join_by(name_state == ESTADO)) %>%
  select(-abbrev_state, -code_state, -code_region, -name_region)


#Função geradora do gráfico
graf = function(ano, prod) {
  media_anual %>% 
    filter(ANO == ano, PRODUTO == prod) %>% ggplot() +
    geom_sf(aes(fill = MEDIA)) +
    labs(subtitle = paste0("Preço médio d", if_else(prod == "GASOLINA COMUM", "a ", "o "),
                           tolower(prod), ", ", ano)) +
    scale_fill_distiller(palette = "Blues", name = "Preço Médio Gasolina") +
    theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) +
    theme_minimal()
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Dados de Combustíveis", 
      titlePanel("Filtrar Dados de Combustíveis"),
      sidebarLayout(
        sidebarPanel(
          selectizeInput("regioes", label = "Selecione uma ou mais regiões:",
                         choices = unique(dados_combustiveis$REGIÃO), multiple = TRUE),
          conditionalPanel(
            condition = "input.regioes.length > 0",
            selectizeInput("estados", label = "Selecione um ou mais estados:",
                           choices = NULL, multiple = TRUE),
            conditionalPanel(
              condition = "input.estados.length > 0",
              selectizeInput("produtos", label = "Selecione um ou mais produtos:",
                             choices = NULL, multiple = TRUE),
              actionButton("filtrar", "Filtrar")
            )
          )
        ),
        mainPanel(
          tableOutput("tabela_filtrada")
        )
      )
    ),
    
    tabPanel("Infográfico",
             #Title
             titlePanel("Infográfico"),
             
             #Sidebar
             sidebarLayout(
               sidebarPanel(width = 2,
                            uiOutput("escolha_combustivel"),
                            uiOutput("escolha_ano")
               ),
               #Main panel
               mainPanel(
                 plotOutput("grafico")
               )
             )
           ),

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
  # Tab 1
  observeEvent(input$regioes, {
    dados_estados <- dados_combustiveis %>%
      filter(REGIÃO %in% input$regioes) %>%
      pull(ESTADO) %>%
      unique()
    
    updateSelectizeInput(session, "estados", choices = dados_estados)
  })
  
  observeEvent(input$estados, {
    dados_produtos <- dados_combustiveis %>%
      filter(ESTADO %in% input$estados) %>%
      pull(PRODUTO) %>%
      unique()
    
    updateSelectizeInput(session, "produtos", choices = dados_produtos)
  })
  
  observeEvent(input$filtrar, {
    regioes_selecionadas <- input$regioes
    estados_selecionados <- input$estados
    produtos_selecionados <- input$produtos
    
    dados_filtrados <- dados_combustiveis %>%
      filter(REGIÃO %in% regioes_selecionadas) %>%
      filter(ESTADO %in% estados_selecionados) %>%
      filter(PRODUTO %in% produtos_selecionados)
    
    output$tabela_filtrada <- renderTable({
      return(dados_filtrados)
    })
  })
  
  # Tab 2
  output$escolha_ano = renderUI({
    selectInput("ano", "Ano: ", unique(media_anual$ANO))
  })
  output$escolha_combustivel = renderUI({
    media_anual = media_anual %>%
      filter(ANO == input$ano)
    selectInput("combustivel", "Combustível:", unique(media_anual$PRODUTO))
    })
  
  
  output$grafico = renderPlot(graf(input$ano, input$combustivel))
    
  # Tab 3
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
