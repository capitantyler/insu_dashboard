shinyServer(
  function(input, output, session){

    rpt_sdad_boton_do <- reactiveValues(data = NULL)

    data <- reactiveValues()
    results <- reactiveValues(
      cabecera = NULL
    )

    success <- reactiveValues(
      boton_do = FALSE
    )

    seleccion <- reactiveValues(
      grupo_id = NULL,
      grupo_tipo = NULL
    )

    srv_grupo_id("grupo_id", seleccion = seleccion)

    mes_rolling <- reactive({
      req(input$mes_rolling)
      if_else(
        input$rolling == TRUE,
        input$mes_rolling,
        12L
      )
    })

    observeEvent(
      eventExpr = input$mes_corte_datos,
      feedbackWarning("mes_corte_datos", "", "falta dato AAAAMM")
    )

    perCierre <- reactive({
      req(input$mes_corte_datos)

      AAAAMM_ejercicio(input$mes_corte_datos, mes_rolling())
    })

    periodos <- reactive({
      req(input$periodos_n)
      (perCierre() - input$periodos_n + 1L):perCierre()
    })

    mes_min <- reactive({
      req(
        input$mes_rolling,
        input$periodos_n,
        input$mes_corte_datos
      )
      ifelse(
        input$rolling == TRUE,
        AAAAMM_diferido(
          x = (perCierre() - input$periodos_n + 1L) * 100L + mes_rolling(),
          1L
        ),
        (input$mes_corte_datos %/% 100L - input$periodos_n + 1L) * 100L + 1L
      )
    })

    indice_salarial <- reactive({
      req(input$mes_corte_datos)

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
      req(input$inflacion_futura_anual)

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

        fct_inf_fut = (1 + input$inflacion_futura_anual/100)^(1/12)

      )

      if(input$metodo_IBNER == "BF_modificado"){
        args[["col_ajuste"]] <- "SALARIO_EMI"
      }

      return(args)

    })

    boton_do_server(
      id = "boton_do",
      emision = emision,
      siniestralidad = siniestralidad,
      seleccion = seleccion,
      rpt_sdad_boton_do = rpt_sdad_boton_do,
      success = success,
      mes_corte_datos = reactive(input$mes_corte_datos),
      modo_moneda = reactive(input$modo_moneda),
      indice_salarial = indice_salarial
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

    nota_de_calculo <- eventReactive(
      seleccion$grupo_tipo,
      {
        if(
          seleccion$grupo_tipo != "Asociart"
        ){
          ""
        } else {
          paste0(
            "No incluye contratos de régimen doméstico ni ",
            "contratos con prima emitida $ 0."
          )
        }
      }
    )

    observeEvent(success$boton_do,
      {
        if(success$boton_do == FALSE){
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

    #### data ----

    data <- reactive({

      filtra_datos_tablero(
        grupo_tipo = seleccion$grupo_tipo,
        grupo_id = seleccion$grupo_id,
        mes_min = mes_min(),
        mes_max = input$mes_corte_datos,
        sin_covid19 = input$sin_covid19
      )

    })

    emision <- reactive({

      reportes_emision(
        data,
        contratos_x_cluster = contratos_x_cluster,
        mes_min = mes_min,
        mes_corte_datos = input$mes_corte_datos,
        mes_rolling = mes_rolling
      )

    })

    #### siniestralidad ----

    siniestralidad <- reactive({
      #timestamp(suffix = glue(": pasa por siniestralidad"))
      req(emision)

      siniestralidad <- reportes_siniestralidad(
        data = data(),
        contratos_x_cluster = contratos_x_cluster,
        args_siniestralidad = args_siniestralidad(),
        emision = emision()
      )
      return(siniestralidad)
    })

    args_siniestralidad <- reactive({

      acomoda_argumentos(
        list(
          mesMin = mes_min(),
          mesMax = input$mes_corte_datos,
          mes_cierre = input$mes_corte_datos,
          cierreDeMes = mes_rolling(),
          # argumentos asociados a funciones externas de ibner e inflación
          metodo_IBNER = input$metodo_IBNER,
          ibnr_puro = data()$ibnr_puro,
          args_metodo()
        )
      )

    })

    rpt_periodo <- eventReactive(
      rpt_sdad_boton_do$data,
      {
        arma_gt_periodo(
          contratos = data()$contratos,
          emision_per = emision()[["rpt2"]],
          montos_per =  rpt_sdad_boton_do$data$siniestralidad_per,
          frecuencias_per = siniestralidad()[["rpt6"]],
          mes_rolling = mes_rolling(),
          periodos_n = input$periodos_n
        )
      }
    )

    #### elementos gráficos ----

    cabecera_Server(
      id = "cuadro.cabecera",
      cabecera = cabecera
    )

    cabecera <- reactive({
      req(data(), seleccion$grupo_tipo)

      escribe_cabecera_tablero_HTML(
        grupo = data(),
        grupo_tipo = seleccion$grupo_tipo,
        limite_contratos = 20L,
        len_cadenas = 50L
      )

    })

    gt_tabla <- cuadro_Server(
      id = "cuadro.1",
      rpt_periodo = rpt_periodo,
      modo_moneda = input$modo_moneda,
      siniestralidad_target = opciones$siniestralidad_target,
      nota_al_pie_especial = nota_de_calculo()
    )

    results$graf_sdad_prima <- graf_sdad_prima_Server(
      id = "plot.sdad.prima",
      vals = rpt_sdad_boton_do,
      periodo = periodos()
    )

    results$graf_sdad_rvas <- graf_sdad_rvas_Server(
      id = "plot.sdad.rvas",
      vals = rpt_sdad_boton_do,
      periodo = periodos()
    )

    results$graf_frec_total <- graf_frec_total_Server(
      id = "plot.indice.incidencia",
      vals = rpt_periodo(),
      periodo = periodos() #vals$periodo
    )

    results$graf_frec_jud <- graf_frec_jud_Server(
      id = "plot.indice.judicial",
      vals = rpt_periodo(),
      periodo = periodos() #vals$periodo
    )

    results$graf_frec_grmu_porinc <- graf_frec_grmu_porinc_Server(
      id = "plot.indice.gravedad",
      vals = rpt_periodo(),
      periodo = periodos() #vals$periodo
    )

    #### listados de datos ----

    listado_descargable_Server(
      id = "lista_siniestros",
      listado = reactive({
        siniestralidad()[["rpt1"]]
      }),
      seleccion = seleccion
    )

    listado_descargable_Server(
      id = "lista_siniestralidad",
      listado = reactive(
        rpt_sdad_boton_do[["data"]][["siniestralidad_per_cto"]]
      ),
      seleccion = seleccion
    )


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
              params = list(vals = results)
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

    #### profiler ----

    # callModule(profvis_server, "profvis")
    # prof <- reactiveVal()
    # output$profvis <- renderUI({prof()})

  }
)
