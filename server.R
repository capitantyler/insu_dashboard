shinyServer(
  function(input, output, session){

    # código para detener aplicación, o par aprogramar algo ante cierre de navegador
    # session$onSessionEnded(stopApp)

    ## Para no tener que regenerar todos los plots y tablas del reporte
    ## una solución es guardarlos en una variable global que se actualice
    ## cada vez que cambian
    ## reactiveValues viene a salvar estos objetos como variables dinámicas.
    ## vals contiene todos los plots y tablas (grobs).

    vals <- reactiveValues(
      grupo = NULL,
      rpt1_excelDT = NULL,
      rpt_sdad_excelDT = NULL,
      nota_de_calculo = NULL,
      periodos = NULL
    )

    success <- reactiveValues(
      boton_do = FALSE
    )

    seleccion <- reactiveValues(
      grupo_id = NULL,
      grupo_tipo = NULL
    )

    # reportes de salida
    sdad_rpt1 <- reactiveVal()
    sdad_per_cto <- reactiveVal()

    srv_grupo_id("grupo_id", seleccion = seleccion)

    mes_rolling <- reactive({
      if_else(
        input$rolling == TRUE,
        input$mes_rolling,
        12L
      )
    })

    perCierre <- reactive({
      AAAAMM_ejercicio(input$mes_corte_datos, mes_rolling())
    })

    periodos <- reactive({
      (perCierre() - input$periodos_n + 1L):perCierre()
    })

    mes_min <- reactive({
      ifelse(
        input$rolling == TRUE,
        AAAAMM_diferido(
          max(
            perCierre() * 100L + mes_rolling(),
            input$mes_corte_datos
          ) - input$periodos_n * 100,
          min(12L, mes_rolling() + 1L)
        ),
        (input$mes_corte_datos %/% 100L - input$periodos_n + 1L) * 100L + 1L
      )
    })

    indice_salarial <- reactive({

      ripte.data[
        MES >= AAAAMM_diferido(mes_min(), -1L) & MES <= input$mes_corte_datos
      ][
        order(desc(MES))
      ][,
        `:=`(
          INDICE_medio = IRIPTE_medio/IRIPTE_medio[1],
          INDICE = IRIPTE/IRIPTE[1]
        )
      ][
        MES >= mes_min()
      ][,
        .(MES, INDICE, INDICE_medio)
      ]

    })

    # parámetros de BF y BF_modificado
    args_metodo <- reactive({

      args <- list(
        cols_fda = c("ILT_LIQ", "ESP_LIQ", "ILP_INC", "JUI_LIQ"),
        cols_names_fda = c("ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT"),
        fda = as.list(
          fda.data[,
            c("FDA_liq_ILT", "FDA_liq_ESP", "FDA_inc_ILP", "FDA_liq_JUI")
          ][,
            FDA_liq_JUI_2017 := FDA_liq_JUI
          ]
        ),

        moneda_homogenea = case_when(
          input$modo_moneda == "historica"     ~ c(FALSE, FALSE, FALSE, FALSE),
          input$modo_moneda == "prima actual"  ~ c(TRUE, TRUE, TRUE, TRUE),
          input$modo_moneda == "prima emitida" ~ c(TRUE, TRUE, TRUE, TRUE),
          TRUE ~ c(FALSE, FALSE, FALSE, FALSE)
        ),

        # por ahora, usamos un sólo índice, no va en forma de lista
        fct_inf = indice_salarial()[["INDICE_medio"]],
        #fct_inf = list(indice_salarial()[["INDICE_medio"]]),

        fct_inf_fut = (1 + input$inflacion_futura_anual/100)^(1/12)

      )

      if(input$metodo_IBNER == "BF_modificado"){
        args[["col_ajuste"]] <- "SALARIO_EMI"
      }

      return(args)

    })

    rpt <- boton_do_server(
      id = "boton_do",
      vals = vals,
      seleccion = seleccion,
      success = success,
      # así no! porque `$` convierte refs a values en reactivevalues.
      # Hay que llamar a la variable reactiva completa
      # grupo_tipo = seleccion$grupo_tipo,
      # grupo_id = seleccion$grupo_id,
      sin_covid19 = reactive(input$sin_covid19),
      mes_rolling = mes_rolling,
      mes_min = mes_min,
      mes_corte_datos = reactive(input$mes_corte_datos),
      periodos_n = reactive(input$periodos_n),
      metodo_IBNER = reactive(input$metodo_IBNER),
      modo_moneda = reactive(input$modo_moneda),
      indice_salarial = indice_salarial,
      args_metodo = args_metodo
    )

    #### activar botones consultar y download ----

    observeEvent(
      seleccion$grupo_id,
      {
        if(is.null(seleccion$grupo_id) || seleccion$grupo_id == ""){
          shinyjs::hide("div_boton_do")
        } else {
          shinyjs::show("div_boton_do")
        }
      }
    )

    observeEvent(success$boton_do,
      {
        if(success$boton_do == FALSE){
        #if(is.null(rpt()$success) || rpt()$success == FALSE){
          shinyjs::hide("div_download_pdf")
        } else {
          shinyjs::show("div_download_pdf")
        }
      },
      ignoreInit = TRUE
    )

    #### mes de corte de datos ----

    observeEvent(input$mes_corte_datos, {
      x <- input$mes_corte_datos
      if (x %% 100 == 0){
        updateNumericInput(session, "mes_corte_datos", value = x - 88L)
      }
      if (x %% 100 == 13){
        updateNumericInput(session, "mes_corte_datos", value = x + 88L)
      }
    })

    #### opción de rolling ----

    observeEvent(input$rolling, {
      if(input$rolling == FALSE){
        shinyjs::disable("mes_rolling")
      } else {
        shinyjs::enable("mes_rolling")
      }
    })


    #### elementos gráficos ----

    cabecera <- cabecera_Server(
      id = "cuadro.cabecera",
      vals = vals,
      seleccion = seleccion
    )

    gt_tabla <- cuadro_Server(
      id = "cuadro.1",
      vals = vals,
      # no se puede llamar como reactivo a un elemento solo de lista
      # vals = vals$rpt_periodo,
      modo_moneda = input$modo_moneda,
      siniestralidad_target = opciones$siniestralidad_target,
      nota_al_pie_especial = vals$nota_de_calculo
    )

    graf_sdad_prima <- graf_sdad_prima_Server(
      id = "plot.sdad.prima",
      vals = vals,
      periodo = vals$periodo
    )

    graf_sdad_rvas <- graf_sdad_rvas_Server(
      id = "plot.sdad.rvas",
      vals = vals,
      periodo = vals$periodo
    )

    graf_frec_total <- graf_frec_total_Server(
      id = "plot.indice.incidencia",
      vals = vals,
      periodo = vals$periodo
    )

    graf_frec_jud <- graf_frec_jud_Server(
      id = "plot.indice.judicial",
      vals = vals,
      periodo = vals$periodo
    )

    graf_frec_grmu_porinc <- graf_frec_grmu_porinc_Server(
      id = "plot.indice.gravedad",
      vals = vals,
      periodo = vals$periodo
    )

    #### listados de datos ----

    # listado_descargable_Server(
    #   id = "lista_siniestros",
    #   listado = rpt
    # )
    output$lista_siniestros <- renderDataTable(
      vals$rpt1_excelDT,
      server = FALSE
    )

    output$lista_siniestralidad <- renderDataTable(
      vals$rpt_sdad_excelDT,
      server = FALSE
    )

    #### botón descargar ----

    # debe estar instalado PahntomJS  webshot::is_phantomjs_installed()
    # instalar con webshot::install_phantomjs()

    output$download_pdf <- downloadHandler(
      filename = function(){
        glue(
          "tablero ",
          seleccion$grupo_tipo,
          " ",
          substr(paste(seleccion$grupo_id, collapse = " "), 1, 50),
          ".pdf"
        )
      },
      content = function(file) {

        disable("div_boton_do")

        #### Progressing indicator
        withProgress(
          message = 'Preparando reporte',
          detail = 'Esto puede tardar un momento...',
          value = 0,
          {
            for (i in 1:10) {
              incProgress(1/15)
              Sys.sleep(0.01)
            }
            ## End of progression
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).

            src <- normalizePath("reporte.Rmd")

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))

            # hago un HTML temporal de la cabecera
            htmltools::save_html(
              HTML(
                as.character(
                  map(
                    cabecera(),
                    #vals$cabecera,
                    ~ div(
                      .x$children[[1]],
                      strong("TRABAJADORES: "), .x$children[[2]],
                      strong("ALICUOTA: "), .x$children[[3]],
                    ) %>% as.character
                  )
                )
              ),
              "cabecera.html"
            )
            pandoc_convert(
              "cabecera.html",
              to = "markdown",
              out = "cabecera.md"
            )
            # hago una imagen temporal de la tabla (no la pude llevar a latex)
            gtsave(
              gt_tabla(),
              # valor elevado de pixels del ancho del viewport (pantalla)
              vwidth = 2500,
              file = file.path(tempdir(), "gt_tabla.png")
            )

            incProgress(2/15)

            file.copy(src, "reporte_PDF.Rmd", overwrite = TRUE)
            out <- render(
              "reporte_PDF.Rmd",
              pdf_document(
                latex_engine = "xelatex"
              ),
              params = list(vals = vals)
            )

            for (i in 13:15) {
              incProgress(1/15)
              Sys.sleep(0.01)
            }

            enable("div_boton_do")

            file.rename(out, file)

          })
      }
    )

    output$exporta_excel_stros <- downloadHandler(

      filename = function(){
        glue(
          "siniestros ",
          seleccion$grupo_tipo,
          " ",
          substr(paste(seleccion$grupo_id, collapse = " "), 1, 50),
          ".xlsx"
        )
      },

      content = function(file) {

        disable("div_boton_do")

        wb <- createWorkbook()
        addWorksheet(wb, "siniestros")
        writeData(wb, "siniestros", vals$grupo$siniestralidad$rpt1)
        wb <- formatear_columnas(
          wb, metodo = "openxlsx",
          hoja = "siniestros",
          columnas_wb = names(vals$grupo$siniestralidad$rpt1),
          filas_wb = nrow(vals$grupo$siniestralidad$rpt1),
          hoja_estilos = stl
        )

        enable("div_boton_do")

        saveWorkbook(wb, file = file, overwrite = TRUE)

      }
    )

    output$exporta_excel_sdad <- downloadHandler(

      filename = function(){
        glue(
          "sdad ",
          seleccion$grupo_tipo,
          " ",
          substr(paste(seleccion$grupo_id, collapse = " "), 1, 50),
          ".xlsx"
        )
      },

      content = function(file) {

        disable("div_boton_do")

        wb <- createWorkbook()
        addWorksheet(wb, "siniestralidad")
        writeData(wb, "siniestralidad", vals$grupo$siniestralidad_per_cto)
        wb <- formatear_columnas(
          wb, metodo = "openxlsx",
          hoja = "siniestralidad",
          columnas_wb = names(vals$grupo$siniestralidad_per_cto),
          filas_wb = nrow(vals$grupo$siniestralidad_per_cto),
          hoja_estilos = stl
        )

        enable("div_boton_do")

        saveWorkbook(wb, file = file, overwrite = TRUE)

      }
    )

    #### profiler ----

    # callModule(profvis_server, "profvis")
    # prof <- reactiveVal()
    # output$profvis <- renderUI({prof()})

  }
)
