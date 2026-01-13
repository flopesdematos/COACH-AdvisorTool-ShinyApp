############################################################
# COACH - Advisor Tool to support the sustainable management of 
# cockle fisheries in the Ria de Aveiro
# Shiny application
#
# This application visualizes:
#  - Current habitat suitability (HSI)
#  - Projected suitability for the following month
#  - Estimated changes between climatic scenarios
#
# Author: Fábio L. Matos | fmatos@ua.pt
############################################################

# -------------------------
# Load required libraries
# -------------------------
library(shiny)
library(shiny.i18n)
library(leaflet)
library(viridis)
library(raster)
library(leafem)
library(RColorBrewer)


# -------------------------
# language setup
# -------------------------
# Translation files must be located in ./www
# Configuration file is defined in config.yaml
i18n <- Translator$new(
  translation_csvs_path = "./www",
  translation_csv_config = "../../config.yaml"
)
i18n$set_translation_language("pt") # default language


# -------------------------
# User Interface definition
# -------------------------
ui <- fluidPage(
  # Enable translations
  shiny.i18n::usei18n(i18n),
  # Language selector (top-right corner)
  div(style = "float: right;",
      selectInput(
        'selected_language',
        i18n$t("Alterar idioma"),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      )
  ),
  
  # -------------------------
  # Sidebar controls
  # -------------------------
  sidebarLayout(
    sidebarPanel(
      # Current month selection
      selectInput(
        "select_month_current",
        h3(i18n$t("Selecionar mês actual")),
        choices = list(
          "1" = "jan",
          "2" = "feb",
          "3" = "mar",
          "4" = "apr",
          "5" = "may",
          "6" = "jun",
          "7" = "jul",
          "8" = "aug",
          "9" = "sep",
          "10" = "oct",
          "11" = "nov",
          "12" = "dec"
        ),
        selected = "jan"
      ),
      # Scenario for current conditions
      selectInput(
        "select_scenario_current",
        h3(i18n$t("Selecionar condições actuais")),
        choices = list(
          "A" = "low",
          "B" = "avg",
          "C" = "high"
        ),
        selected = "avg"
      ),
      # Scenario for future conditions
      selectInput(
        "select_scenario_future",
        h3(i18n$t("Selecionar cenário futuro")),
        choices = list(
          "A" = "low",
          "B" = "avg",
          "C" = "high"
        ),
        selected = "avg"
      ),
      # Scenario legend
      tags$div(
        i18n$t("A: baixo caudal"),
        tags$br(),
        i18n$t("B: caudal médio"),
        tags$br(),
        i18n$t("C: caudal elevado"),
        tags$br(),
        tags$br(),
        tags$br()
      ),
      
      # Raster transparency control
      sliderInput(
        "transp",
        i18n$t("Transparência:"),
        min = 0,
        max = 1,
        value = .7
      )
    ),
    
    # -------------------------
    # Main panel with maps and text
    # -------------------------
    mainPanel(
      # Three synchronized maps
      fluidRow(
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          tagList(tags$h4(i18n$t("Mês actual")), leafletOutput("mymap_current")),
          tagList(tags$h4(i18n$t("Próximo mês")), leafletOutput("mymap_future")),
          tagList(tags$h4(i18n$t("Mudanças estimadas")), leafletOutput("mymap_difference"))
        )
      ),
      h3(""),
      # Descriptive text sections
      p(i18n$t("Esta ferramenta visa ajudar pescadores e entidades reguladoras na gestão da pesca do berbigão na Ria de Aveiro de forma a tornar a exploração deste recurso sustentável. Esta ferramenta permitirá aplicar medidas dinâmicas de gestão da captura de berbigão. A gestão dinâmica dos recursos marinhos consiste na adaptação das respostas de gestão às mudanças, por exemplo, nas condições ecológicas/ambientais. A gestão dinâmica de recursos marinhos é uma ferramenta complementar às medidas de gestão mais tradicionais, tipicamente fixas no espaço e no tempo. A ferramenta foi desenvolvida por investigadores da Universidade de Aveiro (Departamento de Biologia e Física) em colaboração com investigadores do IPMA (Instituto Português do Mar e da Atmosfera).")),
      h3(i18n$t("Como utilizar a ferramenta")),
      p(i18n$t("A área da Ria está colorida de acordo com a adequação (qualidade) das condições ambientais para a ocorrência de berbigão (HSI). O mapa da esquerda e do centro representam, respetivamente, as condições ambientais “atuais” e no mês seguinte de acordo com os cenários selecionados no painel interativo, à esquerda do ecrã. Os cenários propostos descrevem diferentes volume de água doce descarregado na Ria e que se traduzem, por exemplo, na alteração da salinidade da água. As zonas classificadas com valores perto de 1 (áreas coloridas a amarelo) são as que oferecem melhores condições ambientais para a ocorrência de berbigão enquanto as zonas codificadas com valores perto de 0 (roxo) identificam áreas com pouca qualidade ambiental para a espécie se desenvolver. O mapa da direita, indica as alterações previstas que se irão verificar entre os dois meses consecutivos: zonas a vermelho identificam áreas onde as condições ambientais vão piorar enquanto as áreas a verde mostram zonas em que as condições irão melhorar de acordo com o cenário ambiental previsto. O gradiente entre as duas cores reflete a intensidade da mudança. De acordo com a ferramenta deverá evitar-se pescar em áreas coloridas com tons de vermelho, especialmente as áreas mais escuras, de forma que os bancos de pesca de berbigão existentes nessas zonas tenham mais probabilidade de resistir às condições ambientais mais desfavoráveis que se prevêem ocorrer com a diminuição da pressão da pesca.")),
      h3(i18n$t("Isenção de responsabilidade")),
      p(i18n$t("As previsões da ferramenta de apoio COACH resultam de métodos de modelação estando por isso sujeitas a um determinado grau de incerteza pelo que a informação apresentada é meramente indicativa. Futuros estudos permitirão validar estes resultados e melhorar a capacidade de previsão da ferramenta. Detalhes sobre o modelo no artigo:")),
      p("Matos, F. L., N. Vaz, A. Picado, J. M. Dias, F. Maia, M. B. Gaspar, and L. Magalhães. 2022. Assessment of Habitat Suitability for Common Cockles in the Ria the Aveiro Lagoon Under Average and Projected Environmental Conditions. Estuaries and Coasts:1–14."),
      HTML("<a href='https://link.springer.com/article/10.1007/s12237-022-01136-z'>https://link.springer.com/article/10.1007/s12237-022-01136-z</a>"),
      h3(i18n$t("Que dados ambientais são usados?")),
      p(i18n$t("A ferramenta integra dados de ocorrência de berbigão na Ria de Aveiro com dados ambientais de salinidade, temperatura, clorofila a, concentração de nitratos, velocidade de correntes, e tempo de submersão. ")),
      h3(i18n$t("Financiamento do projeto")),
      p(i18n$t("O financiamento do projeto foi assegurado pelo Fundo de Conservação dos Oceanos, uma iniciativa conjunta da Fundação Oceano Azul e do Oceanário de Lisboa."))
    )
  )
)

# -------------------------
# Server logic
# -------------------------

server <- function(input, output, session) {
  
  # -------------------------
  # Language switching
  # -------------------------
  
  observeEvent(input$selected_language, {
    req(input$selected_language)
    message("Language change! ", input$selected_language)
    
    shiny.i18n::update_lang(session, input$selected_language)
  }, ignoreInit = TRUE)
  
  
  # -------------------------
  # Current month map
  # -------------------------
  output$mymap_current <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend(
        pal = colorNumeric(viridis(256), c(0:1), reverse = TRUE),
        values = c(0:1),
        title = "HSI"
      ) %>%
      setView(-8.7, 40.7, 10) %>%
      addLogo(
        "COACH_v_umacor.png",
        src = "remote",
        width = 110,
        height = 100,
        position = "topleft",
        alpha = .4
      )
  })
  
  observe({
    
    nc1 <- raster(paste0("./data/scenarios_projection/", input$select_month_current, "_", input$select_scenario_current,".tif")
    )
    
    crs(nc1) <-  "EPSG:3763"
    nc1 <- projectRaster(nc1, 
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
    
    pal <- colorNumeric(
      pal = viridis(256), c(0:1), #values(nc),
      na.color = "transparent")
    
    leafletProxy("mymap_current") %>%
      clearImages() %>%  
      addRasterImage(nc1, colors = pal, opacity = input$transp)  
    
  })
  
  
  
  # -------------------------
  # Future month map
  # -------------------------
  
  output$mymap_future <- renderLeaflet({
    
    pal <- colorNumeric(
      pal = viridis(256), c(0:1), #values(nc),
      na.color = "transparent")
    
    # Create a leaflet map
    leaflet() %>%
      addTiles() %>%
      # addLayersControl(baseGroups = c("CartoDB", "Esri.WorldImagery", "OSM")) %>% 
      # addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLegend(pal = colorNumeric(
        pal = viridis(256), c(0:1), na.color = "transparent", reverse = T),
        values = c(0:1),
        title = "HSI",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
      setView(-8.7, # lon
              40.7, # lat
              10 # zoom
      )  
  })
  
  observe({
    
    if(input$select_month_current != "dec"){ 
      nc2 <- raster(paste0("./data/scenarios_projection/", tolower(month.abb[(match(paste(toupper(substr(input$select_month_current, 1, 1)), substr(input$select_month_current, 2, nchar(input$select_month_current)), sep=""),month.abb)+1)]) , "_", input$select_scenario_future,".tif"))
    } else {
      nc2 <- raster(paste0("./data/scenarios_projection/", "jan" , "_", input$select_scenario_future,".tif"))
      
    } 
    
    crs(nc2) <-  "EPSG:3763"
    nc2 <- projectRaster(nc2, 
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
    
    pal <- colorNumeric(
      pal = viridis(256), c(0:1), #values(nc),
      na.color = "transparent")
    
    leafletProxy("mymap_future") %>%
      clearImages() %>%  
      addRasterImage(nc2, colors = pal, opacity = input$transp)  
    
  })
  
  # -------------------------
  # Difference map (ΔHSI)
  # -------------------------
  
  output$mymap_difference <- renderLeaflet({
    
    pal <- colorNumeric(
      pal = c(rev(brewer.pal(6,"OrRd")), "white", brewer.pal(6,"BuGn")), c(-1:1), #values(nc),
      na.color = "transparent")
    
    # Create a leaflet map
    leaflet() %>%
      addTiles() %>%
      # addLayersControl(baseGroups = c("CartoDB", "Esri.WorldImagery", "OSM")) %>% 
      # addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLegend(pal = colorNumeric(
        pal =  c(rev(brewer.pal(6,"OrRd")), "white", brewer.pal(6,"BuGn")), c(-1:1), na.color = "transparent", reverse = T),
        values = c(-1:1),
        title = "Δ HSI",
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
      setView(-8.7, # lon
              40.7, # lat
              10 # zoom
      ) 
  })
  
  
  observe({
    
    nc1 <- raster(paste0("./data/scenarios_projection/", input$select_month_current, "_", input$select_scenario_current,".tif")
    )
    
    crs(nc1) <-  "EPSG:3763"
    nc1 <- projectRaster(nc1, 
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
    
    if(input$select_month_current != "dec"){ 
      nc2 <- raster(paste0("./data/scenarios_projection/", tolower(month.abb[(match(paste(toupper(substr(input$select_month_current, 1, 1)), substr(input$select_month_current, 2, nchar(input$select_month_current)), sep=""),month.abb)+1)]) , "_", input$select_scenario_future,".tif"))
    } else {
      nc2 <- raster(paste0("./data/scenarios_projection/", "jan" , "_", input$select_scenario_future,".tif"))
      
    } 
    
    crs(nc2) <-  "EPSG:3763"
    nc2 <- projectRaster(nc2, 
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
    
    nc_dif <- nc2-nc1
    
    
    pal <- colorNumeric(
      pal = c(brewer.pal(6,"OrRd"), "white", rev(brewer.pal(6,"BuGn"))), c(-1:1), #values(nc),
      na.color = "transparent")
    
    leafletProxy("mymap_difference") %>%
      clearImages() %>%  
      addRasterImage(nc_dif, colors = pal, opacity = input$transp) 
    
  })
  
  # -------------------------
  # Map synchronization
  # based on: https://resultfor.dev/105934-how-to-sync-maps-in-r-shiny
  # -------------------------
  observe({
    coords <- input$mymap_current_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_future") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({
    coords <- input$mymap_current_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_difference") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({
    coords <- input$mymap_future_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_current") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({
    coords <- input$mymap_future_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_difference") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({
    coords <- input$mymap_difference_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_current") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({
    coords <- input$mymap_difference_bounds
    if (!is.null(coords)) {
      leafletProxy("mymap_future") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
