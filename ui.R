library(shiny)
library(shinyjs)
library(shinydashboard)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(){dashboardPage(
  #### ----
  #### scripts ----
  # tags$script(
  #   # este script toma el tamaño de la vantana interna
  #   'var dimension = [0, 0];
  #             $(document).on("shiny:connected", function(e) {
  #                 dimension[0] = window.innerWidth;
  #                 dimension[1] = window.innerHeight;
  #                 Shiny.onInputChange("dimension", dimension);
  #             });
  #             $(window).resize(function(e) {
  #                 dimension[0] = window.innerWidth;
  #                 dimension[1] = window.innerHeight;
  #                 Shiny.onInputChange("dimension", dimension);
  #             });'
  # ),
  ####----
  header = dashboardHeader(
    title = "Informe Integral"
  ),
  sidebar = dashboardSidebar(
    #### scripts ----
    useShinyjs(),
    #### estilos css ----
    includeCSS("www/style.css"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
      # put these files in the www subfolder
      # tags$script(src = "scripts/xlsx.core.min.js"),
      # tags$script(src = "scripts/FileSaver.min.js"),
      # tags$script(src = "scripts/tableexport.min.js")
    ),
    ###----
    fluidRow(
      ui_grupo_id("grupo_id"),
      disabled(actionButton("do", "Consulta")),
      disabled(downloadButton("download", "Descarga", class = "btn-default"))
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "menu",
      menuItem(
        "opciones", icon = icon("fas fa-cogs"),
        # menuSubItem("Sub-item 1", tabName = "subitem1"),
        # menuSubItem("Sub-item 2", tabName = "subitem2")
        checkboxInput(
          inputId = "sin_covid19",
          label = "sin COVID-19",
          value = opciones$sin_covid19_default
        ),
        selectInput(
          "metodo_IBNER",
          "Método de IBNER",
          choices = metodo_IBNER,
          selected = opciones$metodo_IBNER_default
        ),
        selectInput(
          "modo_moneda",
          "Moneda",
          choices = modos_moneda,
          selected = opciones$modo_moneda_default
        ),
        numericInput(
          "inflacion_futura_anual",
          "Inflación futura anual (%)",
          min = 0, max = 60, step = 0.1,
          value = opciones$inflacion_futura_anual_default
        ),
        numericInput(
          "mes_corte_datos",
          "Mes corte datos",
          min = mes_corte_datos_min,
          max = mes_corte_datos_max,
          opciones$mes_corte_datos_default
        ),
        checkboxInput(
          "rolling",
          "Rolling 12 meses",
          TRUE
        ),
        numericInput(
          "mes_rolling",
          "Mes de Rolling",
          min = 1L, max = 12L, step = 1L,
          value = opciones$mes_nro_cierre_default
        ),
        numericInput(
          "periodos",
          "Número de períodos",
          min = 5L, max = 15L, step = 1L,
          value = 5L
        )
      ),
      bsTooltip(
        id = "rolling",
        title = "Toma los años móviles, cerrando en el mes elegido",
        placement = "right", trigger = "hover"
      ),
      bsTooltip(
        id = "metodo_IBNER",
        title = "Método con el que se proyectan los importes acontecidos pero no informados",
        placement = "right", trigger = "hover"
      )
    )
  ),

  body = dashboardBody(
    useSweetAlert(),
    uiOutput("ui_cabecera"),
    tabsetPanel(
      tabPanel( #### pestaña principal----
        "Principal",
        icon = icon("file-invoice"),
        fluidRow( # fila para la tabla
          gt_output(outputId = "gt.tabla1")
        ),
        fluidRow(
          class = "row_graficos",
          # 5 - gráfico
          column(
            class = "col_graficos",
            width = 6,
            box(
              title = "Prima y Siniestralidad",
              width = 12,
              plotOutput("plot.abajo1")
            )
          ),
          # 7 - gráfico
          column(
            class = "col_graficos",
            width = 6,
            box(
              title = "Siniestralidad, Reservas e IBNER",
              width = 12,
              plotOutput("plot.abajo2")
            )
          )
        )
      ),         #### ----
      tabPanel(
        "frecuencias",
        icon = icon("chart-bar"),
        fluidRow( # fila para la tabla
          # tabla 2
        ),
        fluidRow(
          class = "row_graficos",
          # 5 - gráfico
          # 6 - gráfico
          column(
            class = "col_graficos",
            width = 4,
            box(
              title = "Indice de Incidencia",
              solidHeader = TRUE,
              width = 12,
              plotOutput("plot.abajo3")
            )
          ),
          column(
            class = "col_graficos",
            width = 4,
            box(
              title = "Indice de Judicialidad",
              width = 12,
              plotOutput("plot.abajo4")
            )
          ),
          # 6 - gráfico
          column(
            class = "col_graficos",
            width = 4,
            box(
              title = "Indices de Gravedad",
              width = 12,
              plotOutput("plot.abajo5")
            )
          )
        )
      ),
      tabPanel(
        "siniestros",
        DTOutput("lista_siniestros"),
        downloadButton("exporta_excel_stros", "Exportar Excel"), #boton excel
        icon = icon("table")
      ),
      tabPanel(
        "siniestralidad anual",
        DTOutput("lista_siniestralidad"),
        downloadButton("exporta_excel_sdad", "Exportar Excel"), #boton excel
        icon = icon("table")
      )
    )
  )
)

}
