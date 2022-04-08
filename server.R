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
      cabecera = NULL,
      gt_tabla = NULL,
      plotsdadprima = NULL,
      plotsdadibner = NULL,
      plotfrectotal = NULL,
      plotfrecjud = NULL,
      plotfrecporinc = NULL,
      rpt1_excelDT = NULL,
      rpt_sdad_excelDT = NULL,
      grupo = NULL
    )

    seleccion <- srv_grupo_id("grupo_id")

    #### activar botón ----

    observeEvent(input$grupo_id, {
      if(is.null(input$grupo_id) ||input$grupo_id == ""){
        shinyjs::disable("do")
      } else {
        shinyjs::enable("do")
      }
    })

    #### botón mes de corte de datos

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

    #### botón consultar ----

    observeEvent(input$do, {

      boton_do(
        n = isolate(input$do),
        sin_covid19 = isolate(input$sin_covid19),
        rolling = isolate(input$rolling),
        mes_rolling = isolate(input$mes_rolling),
        mes_corte_datos <- isolate(input$mes_corte_datos),
        grupo_tipo = isolate(input$`grupo_id-grupo_tipo`),
        grupo_id = isolate(input$grupo_id),
        periodos_n = isolate(input$periodos),
        metodo_IBNER = isolate(input$metodo_IBNER),
        modo_moneda = isolate(input$modo_moneda),
        fct_inf_fut = isolate((1 + input$inflacion_futura_anual/100)^(1/12)),
        vals = vals
      )
      
      vals$rpt1_excelDT <- formatear_columnas(
        metodo = "DT",
        objeto = datatable(
          vals$grupo$siniestralidad$rpt1,
          selection = 'none',
          editable = FALSE,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            pageLength = 25,
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            bInfo = FALSE,
            dom = 'Bfrtip',
            buttons = c('copy'),
            class = "display",
            scrollX = TRUE
          )
        )
      )
      
      vals$rpt_sdad_excelDT <- formatear_columnas(
        metodo = "DT",
        objeto = datatable(
          vals$grupo$siniestralidad_per_cto,
          selection = 'none',
          editable = FALSE,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            pageLength = 25,
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            bInfo = FALSE,
            dom = 'Bfrtip',
            buttons = c('copy'),
            class = "display",
            scrollX = TRUE
          )
        )
      )
      
    })

    output$ui_cabecera <- renderUI({
      vals$cabecera
    })
    
    output$gt.tabla1 <- render_gt(
      vals$gt_tabla
    )
    
    output$plot.abajo1 <- renderPlot(
      vals$plotsdadprima
    )
    
    output$plot.abajo2 <- renderPlot(
      vals$plotsdadibner
    )
    
    output$plot.abajo3 <- renderPlot(
      vals$plotfrectotal
    )
    
    output$plot.abajo4 <- renderPlot(
      vals$plotfrecjud
    )
    
    output$plot.abajo5 <- renderPlot(
      vals$plotfrecporinc
    )
    

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

    output$download <- downloadHandler(
      filename = function(){
        glue(
          "tablero ",
          isolate(input$`grupo_id-grupo_tipo`),
          " ",
          substr(isolate(paste(input$grupo_id, collapse = " ")), 1, 50),
          ".pdf"
        )
      },
      content = function(file) {
          #### Progressing indicator
        disable("do")
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
                     vals$cabecera,
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
               vals$gt_tabla,
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

             enable("do")

             file.rename(out, file)

          })
      }
    )

    output$exporta_excel_stros <- downloadHandler(

      filename = function(){
        glue(
          "siniestros ",
          isolate(input$`grupo_id-grupo_tipo`),
          " ",
          substr(isolate(input$grupo_id), 1, 50),
          ".xlsx"
        )
      },

      content = function(file) {

        disable("do")

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

        enable("do")

        saveWorkbook(wb, file = file, overwrite = TRUE)

      }
    )

      output$exporta_excel_sdad <- downloadHandler(

        filename = function(){
          glue(
            "sdad ",
            isolate(input$`grupo_id-grupo_tipo`),
            " ",
            substr(isolate(input$grupo_id), 1, 50),
            ".xlsx"
          )
        },

        content = function(file) {

          disable("do")

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

          enable("do")

          saveWorkbook(wb, file = file, overwrite = TRUE)

        }
    )

    #### profiler ----

    # callModule(profvis_server, "profvis")
    # prof <- reactiveVal()
    # output$profvis <- renderUI({prof()})

  }
)
