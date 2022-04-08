library(shiny)
library(shinydashboard)

# basicConfig()

shinyServer(
  function(input, output, session){
    showLog()
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
      grupo = NULL
    )


    output$grupo_id <- renderUI (expr = {
      # Si se devuelve vacío, return para evitar error de función
      if (is.null (input$grupo_tipo))
        return ()
      switch (
        input$grupo_tipo,
        "Asociart" = {
        },
        "grupo_ec" = {
          selectInput (
            inputId = "grupo_id",
            label = "Grupo económico",
            choices = lista_grupo_id$grupo_ec$choices,
            selected = lista_grupo_id$grupo_ec$default,
            multiple = TRUE
          )
        },
        "sucursales" = {
          selectInput(
            "grupo_id",
            label = "Sucursal",
            choices = setNames(
              lista_grupo_id$sucursales$choices$SUC_ID,
              lista_grupo_id$sucursales$choices$sucursal
            ),
            selected = lista_grupo_id$sucursales$default
          )
        },
        "regiones" = {
          selectInput(
            "grupo_id",
            label = "Región",
            choices = setNames(
              lista_grupo_id$regiones$choices$REG_ID,
              lista_grupo_id$regiones$choices$region
            ),
            selected = lista_grupo_id$regiones$default
          )
        },
        "provincias" = {
          selectInput(
            "grupo_id",
            label = "Provincia",
            choices = setNames(
              lista_grupo_id$provincias$choices$ID,
              lista_grupo_id$provincias$choices$PROVINCIA
            ),
            selected = lista_grupo_id$provincias$default
          )
        },
        "uc" = {
          textInput (
            "grupo_id",
            label = "Nro de Unidad Comercial",
            value = lista_grupo_id$uc$default
          )
        },
        "pas" = {
          textInput (
            "grupo_id",
            "CUIT del PAS/socia/organizador/productor/asesor/broker",
            value = lista_grupo_id$pas$default
          )
        },
        "clase_3" = {
          selectInput (
            "grupo_id",
            label = "clase",
            choices = setNames(
              lista_grupo_id$clase_3$choices$value,
              lista_grupo_id$clase_3$choices$label
            )
          )
        },
        "clase_1" = {
          selectInput (
            "grupo_id",
            label = "Actividad x20",
            choices = setNames(
              lista_grupo_id$clase_1$choices$value,
              lista_grupo_id$clase_1$choices$label
            )
          )
        },
        "CIIUR2" = {
          selectInput (
            "grupo_id",
            label = "Actividad",
            choices = setNames(
              lista_grupo_id$CIIUR2$choices$value,
              lista_grupo_id$CIIUR2$choices$label
            ),
            selected = lista_grupo_id$CIIUR2$default
          )
        },
        "CIIUR2_1d" = {
          selectInput (
            "grupo_id",
            label = "Actividad x10",
            choices = setNames(
              lista_grupo_id$CIIUR2_1d$choices$value,
              lista_grupo_id$CIIUR2_1d$choices$label
            )
          )
        },
        "contratos" = {
          textInput (
            "grupo_id",
            label = "Contratos",
            value = lista_grupo_id$contratos$default
          )
        },
        "cuit" = {
          textInput (
            "grupo_id",
            label = "CUITS Contratos",
            value = lista_grupo_id$cuit$default
          )
        },
        disabled(
          textInput (
            "grupo_id",
            "Valores",
            ""
          )
        )
      )

    })


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

      n <- isolate(input$do)
      sin_covid19 <- isolate(input$sin_covid19)
      rolling <- isolate(input$rolling)
      grupo_tipo <- isolate(input$grupo_tipo)
      grupo_id <- isolate(input$grupo_id)
      periodos_n <- isolate(input$periodos)
      metodo_IBNER <- isolate(input$metodo_IBNER)
      modo_moneda <- isolate(input$modo_moneda)
      fct_inf_fut <- isolate((1 + input$inflacion_futura_anual/100)^(1/12))

      if (n == 0) {
        disable("download")
        # return()
      } else {
        enable("download")
      }

      mes_rolling <- if_else(
        rolling == TRUE,
        isolate(input$mes_rolling),
        12L
      )

      mes_corte_datos <- isolate(input$mes_corte_datos)
      perCierre <- AAAAMM_ejercicio(mes_corte_datos, mes_rolling)
      mesMin <- ifelse(
        rolling == TRUE,
        AAAAMM_diferido(
          max(perCierre * 100L + mes_rolling, mes_corte_datos) - periodos_n * 100,
          min(12L, mes_rolling + 1L)
        ),
        (mes_corte_datos %/% 100L - periodos_n + 1L) * 100L + 1L
      )
      periodos <- (perCierre - periodos_n + 1L):perCierre

      if(periodos_n > 5L){
        credibilidad_IBNR <- c(rep(1, periodos_n - 5L), credibilidad_IBNR_5)
      } else {
        credibilidad_IBNR <- credibilidad_IBNR_5[(5L-periodos_n+1L):5L]
      }

      indice_salarial <- ripte.data[
        MES >= AAAAMM_diferido(mesMin, -1L) & MES <= mes_corte_datos
      ][
        order(desc(MES))
      ][,
        `:=`(
          INDICE_medio = IRIPTE_medio/IRIPTE_medio[1],
          INDICE = IRIPTE/IRIPTE[1]
        )
      ][
        MES >= mesMin
      ][,
        .(MES, INDICE, INDICE_medio)
      ]

      filtroContratos <- if(
        grupo_tipo != "Asociart"
      ){
        filtroContratos <- filtrar_nros_contrato(
          grupo_tipo, grupo_id
        )
      } else {

        # debido a la voluminosidad,
        # calculamos Asociart enRégimen General
        # sólo contratos con emisión
        # filtroContratos <- NA

        nota_al_pie_especial <- paste0(
          "No incluye contratos de régimen doméstico ni ",
          "contratos con prima emitida $ 0."
        )

        filtroContratos <- emiorig.data[
          MES >= mesMin & TIPO != "D"
        ][,
          .(PRIMA = sum(PRIMA)),
          by = CONTRATO
        ][
          PRIMA > 0
          #& !CONTRATO %in% contratos_emblema
          #& CONTRATO %in% contratos_in_force
        ][["CONTRATO"]]

      }

      vals$grupo <- filtrar_tablas(
        filtroContratos
      )

      # filtro de emision
      vals$grupo$emiorig <- vals$grupo$emiorig[
        MES >= mesMin & MES <= mes_corte_datos
      ]

      if(
        length(filtroContratos) == 0 |
        nrow(vals$grupo$emiorig) == 0
      ){
        sendSweetAlert(
          session = session,
          title   = HTML("Ups..."),
          text    = HTML("No hay datos en el periodo para los contratos seleccionado..."),
          type    = "warning",
          html    = TRUE
        )
        req(FALSE)
      }


      # filtro temporal
      # vals$grupo$siniestros <- vals$grupo$siniestros[DENUNCIA == 1127658]
      # filtrado de columnas inservibles (pasar a .Rprofile)
      vals$grupo$siniestros <- vals$grupo$siniestros[,
        .(
          CONTRATO, DENUNCIA, SINIESTRO, CUIL, SEXO, TRABAJADOR, EDAD, F_INGRESO,
          MESACC, F_ACC, F_BAJA, F_ALTA, EDOCONS, ESTADO_STRO, IBM,
          SALARIO, PORINC, CIE10_id, DIAS, JUI, ORIGEN, TERMINACION, FLAG_COVID
        )
      ]

      # agrego columnas

      vals$grupo$siniestros <- estado_consecuencia.data[,
        .(EDOCONS, ESTADO_CONSECUENCIA)
      ][
        vals$grupo$siniestros,
        on = .(EDOCONS),
        allow.cartesian = TRUE
      ][,
        EDOCONS := NULL
      ]

      vals$grupo$siniestros <- estado_siniestro.data[,
        .(ESTADO_STRO, ESTADO_SINIESTRO)
      ][
        vals$grupo$siniestros,
        on = .(ESTADO_STRO),
        allow.cartesian = TRUE
      ]

      vals$grupo$siniestros <- cie10.data[
        vals$grupo$siniestros,
        on = .(ID = CIE10_id),
        allow.cartesian = TRUE
      ][,
        ID := NULL
      ]

      vals$grupo$siniestros <- vals$grupo$siniestros[,
         .(
           CONTRATO, DENUNCIA, SINIESTRO, CUIL, SEXO, TRABAJADOR, EDAD, F_INGRESO,
           MESACC, F_ACC, F_BAJA, F_ALTA, ESTADO_CONSECUENCIA,
           # se necesita ESTADO_STRO para rpt6 siniestralidad
           ESTADO_STRO, ESTADO_SINIESTRO,
           IBM, SALARIO, PORINC, CIE10_DESCR, DIAS, JUI, ORIGEN, TERMINACION,
           FLAG_COVID
         )
      ]

      setkey(vals$grupo$siniestros, DENUNCIA)

      # filtro de covid
      if(sin_covid19 == TRUE) {
        vals$grupo$siniestros <- vals$grupo$siniestros[
          FLAG_COVID == FALSE
        ]
        vals$grupo$juicios <- vals$grupo$juicios[
          vals$grupo$siniestros, .SD,
          on = .(DENUNCIA),
          nomatch = 0
        ]
        vals$grupo$liquidaciones <- vals$grupo$liquidaciones[
          vals$grupo$siniestros, .SD, nomatch = 0
        ]
        vals$grupo$reservas <- vals$grupo$reservas[
          vals$grupo$siniestros, .SD, nomatch = 0
        ]
      }


      ## preparación de datos para IBNER ----

      # lista vacía de parámetros para las opciones no programadas de IBNER
      args_metodo <- list()

      #### moneda homogenea

      moneda_homogenea <- case_when(
        modo_moneda == "historica" ~  c(FALSE, FALSE, FALSE, FALSE),
        modo_moneda == "prima actual" ~   c(TRUE, TRUE, TRUE, TRUE),
        modo_moneda == "prima emitida" ~   c(TRUE, TRUE, TRUE, TRUE),
        TRUE ~ c(FALSE, FALSE, FALSE, FALSE)
      )

      if(metodo_IBNER %in% metodos_BF){

        # 1 - calculo el ibnr puro en nro y pesos ----

        # las frecuencias y primas puras que se pasan son MENSUALES.
        vals$grupo$ibnr_puro <- ultimates_GSC.data[,.(
          MES_INI, MES_FIN,
          N_FREC_ULT = N_FREC_ULT/12,
          ILT_IBNR_PP, ESP_IBNR_PP, ILP_IBNR_PP, JUI_IBNR_PP
        )
        ][
          # unequal join para obtener ultimates entre fechas
          # duplico col MES para que no desaparezca con el join
          vals$grupo$emiorig[, MES_EMI := MES],
          on = .(MES_INI <= MES_EMI, MES_FIN >= MES_EMI)
        ]

        # selección
        vals$grupo$ibnr_puro[,.(
          CONTRATO, MES, TRABAJADORES,
          N_FREC_ULT, ILT_IBNR_PP, ESP_IBNR_PP, ILP_IBNR_PP, JUI_IBNR_PP
        )]
        # delay
        vals$grupo$ibnr_puro[,
          DELAY := pmax(
            (mes_corte_datos_max %/% 100- MES %/% 100)  * 12L +
              (mes_corte_datos_max %% 100- MES %% 100) + 1L,
            1L
          )
        ]
        # frecuencia IBNR de casos
        vals$grupo$ibnr_puro[,
          N_FREC_IBNR := fifelse(
            DELAY > length(patron_fda),
            0L,
            N_FREC_ULT * (1- 1/patron_fda[DELAY])
          )
        ]
        # IBNR puro y absoluto de casos y prima pura,
        # agrupado por CONTRATO y MES
        vals$grupo$ibnr_puro[,
          c('N_IBNR', 'ILT_IBNR', 'ESP_IBNR', 'ILP_IBNR', 'JUI_IBNR') := lapply(
          .SD, "*", TRABAJADORES
         ),
         .SDcols = c('N_FREC_IBNR', 'ILT_IBNR_PP', 'ESP_IBNR_PP', 'ILP_IBNR_PP', 'JUI_IBNR_PP')
        ][,
          lapply(.SD, sum), by = list(CONTRATO, MES),
          .SDcols = c('N_IBNR', 'ILT_IBNR', 'ESP_IBNR', 'ILP_IBNR', 'JUI_IBNR')
        ]

        ## 2 - ultimates a priori para BF Modificado ----

        vals$grupo$siniestros[,
         # unequal join para obtener ultimates entre fechas
         # duplico col MESACC para que no desaparezca con el join
         c("MES_ACC_INI", "MES_ACC_FIN") := list(MESACC, MESACC)
        ]

        if(metodo_IBNER %in% c("BF", "BF_modificado")){

          if(metodo_IBNER == "BF"){
            # agrego los costos medios ultimate por siniestro
            vals$grupo$siniestros <- vals$grupo$siniestros[ultimates_GSC.data[,
              .(
                MES_INI, MES_FIN,
                ILT_ULT_CM0, ESP_ULT_CM0, ILP_ULT_CM0, JUI_ULT_CM0
              )
            ],
            nomatch = NULL,
            on = .(MES_ACC_INI >= MES_INI, MES_ACC_FIN <= MES_FIN)
            ]
          }

          if(metodo_IBNER == "BF_modificado"){
            # agrego los costos medios ultimate por siniestro
            vals$grupo$siniestros <- vals$grupo$siniestros[ultimates_GSC.data[,
              .(
                MES_INI, MES_FIN, SALARIO_EMI_0 = SALARIO_EMI,
                ILT_ULT_CM0, ESP_ULT_CM0, ILP_ULT_CM0, JUI_ULT_CM0
              )
            ],
            nomatch = NULL,
            on = .(MES_ACC_INI >= MES_INI, MES_ACC_FIN <= MES_FIN)
            ]
          }

          vals$grupo$siniestros[,
            c("MES_ACC_INI", "MES_ACC_FIN") := NULL
          ]

          setkey(vals$grupo$siniestros, DENUNCIA)

          setnames(
            vals$grupo$siniestros,
            c('ILT_ULT_CM0', 'ESP_ULT_CM0', 'ILP_ULT_CM0', 'JUI_ULT_CM0'),
            c('ILT_ULT_H', 'ESP_ULT_H', 'ILP_ULT_H', 'JUI_ULT_H')
          )

          # agrego CM para juicios pre 2017
          vals$grupo$siniestros[,
           JUI_ULT_2017_H := JUI_ULT_H
          ]

        }

        # parámetros de BF y BF_modificado
        args_metodo <- list(

          cols_fda = c("ILT_LIQ", "ESP_LIQ", "ILP_INC", "JUI_LIQ"),
          cols_names_fda = c("ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT"),
          fda = as.list(fda.data[,
           c("FDA_liq_ILT", "FDA_liq_ESP", "FDA_inc_ILP", "FDA_liq_JUI")
          ][,
            FDA_liq_JUI_2017 := FDA_liq_JUI
          ]),

          moneda_homogenea = moneda_homogenea,

          # por ahora, usamos un sólo índice, no va en forma de lista
          fct_inf = indice_salarial[["INDICE_medio"]],
          #fct_inf = list(indice_salarial[["INDICE_medio"]]),

          fct_inf_fut = fct_inf_fut
        )

        if(metodo_IBNER == "BF_modificado"){
          args_metodo[["col_ajuste"]] <- "SALARIO_EMI"
        }

      }

      ## reportes siniestralidad y emision ----

      if(
        length(vals$grupo$contratos$CONTRATO) < contratos_x_cluster
      ){
        # llamo con un do call ya que preciso pasar los argumentos como lista
        # porque cambian los parámetros según método de IBNER
        ## reportes emisión
        vals$grupo$emision <- EmisionConMovimientos(
          reportes = c("rpt1", "rpt2"),
          # reportes = c("rpt1", "rpt2", "rpt5"),
          emiorig = vals$grupo$emiorig,
          rectificativa = vals$grupo$rectificativa,
          domesticas = vals$grupo$domesticas,
          comisiones = vals$grupo$comisiones,
          cobranza = vals$grupo$cobranza,
          contratos = vals$grupo$contratos,
          sucursal = vals$grupo$sucursal,
          factorContrato = "CONTRATO",
          mesMin = mesMin,
          mesMax = mes_corte_datos,
          modoRectificativa = "A",
          cierreDeMes = mes_rolling
        )

        # convierto en data.table (falta programar hacerlo antes)
        vals$grupo$emision <- lapply(
          vals$grupo$emision, as.data.table
        )

        # achico reportes
        vals$grupo$emision[["rpt1"]] <- vals$grupo$emision[["rpt1"]][,
          .(
            CONTRATO, MES, PER, SALPROM, PRIMA_r, COMI_r
          )
        ]

        # creo que se pueden sacar aún más columnas
        vals$grupo$emision[["rpt2"]] <- vals$grupo$emision[["rpt2"]][,
           .(
             CONTRATO, MES, PER,
             MESES,
             TRABAJADORES, TRABAJADORESMESsinDOM, TRABAJADORESMES,
             MASA_desest,
             TRABAJADORESMES_r, #SALARIO,
             MASA_r, PREMIO_r, PRIMA_r, COMI_r
           )
        ]

        ## reportes  siniestralidad

        if(metodo_IBNER == "BF_modificado"){
          # agrego salario promedio emitido
          vals$grupo$siniestros <- vals$grupo$siniestros[
            vals$grupo$emision[["rpt1"]][,.(CONTRATO, MES, SALPROM)],
            SALARIO_EMI := i.SALPROM,
            on = c(CONTRATO = "CONTRATO", MESACC = "MES")
          ]
        }

        vals$grupo$siniestralidad <- do.call(
          genera_reportes_siniestros,
          args = c(
            list(
              reportes = c("rpt1", "rpt6"),
              siniestros = vals$grupo$siniestros,
              liquidaciones = vals$grupo$liquidaciones,
              reservas = vals$grupo$reservas,
              juicios = vals$grupo$juicios,
              contratos = vals$grupo$contratos,
              mesMin = mesMin,
              mesMax = mes_corte_datos,
              mes_cierre = mes_corte_datos_max,
              cierreDeMes = mes_rolling,
              # argumentos asociados a funciones externas de ibner e inflación
              metodo_IBNER = metodo_IBNER,
              ibnr_puro = vals$grupo$ibnr_puro
            ),
            args_metodo
          )
        )

      } else {

        clusters <- split(
          vals$grupo$contratos$CONTRATO,
          ceiling(seq_along(vals$grupo$contratos$CONTRATO)/contratos_x_cluster)
        )
        n_cluster <- length(clusters)

        # progress bar
        progressSweetAlert(
          session = session,
          id = "ui_progress_bar_clusters",
          title = "Procesando...",
          status = "info",
          display_pct = TRUE, value = 0
        )

        emi_rpt1_list <- vector(mode = "list", length = n_cluster)
        emi_rpt2_list <- vector(mode = "list", length = n_cluster)
        # emi_rpt5_list <- vector(mode = "list", length = n_cluster)
        sdad_rpt1_list <- vector(mode = "list", length = n_cluster)
        sdad_rpt6_list <- vector(mode = "list", length = n_cluster)


        for(cluster in seq_along(clusters)){

          denuncias_en_cluster <- vals$grupo$siniestros[
            CONTRATO %in% clusters[[cluster]]
          ][["DENUNCIA"]]

          ## reportes emisión
          tmp_emision <- EmisionConMovimientos(
            reportes = c("rpt1", "rpt2"),
            # reportes = c("rpt1", "rpt2", "rpt5"),
            emiorig = vals$grupo$emiorig[CONTRATO %in% clusters[[cluster]]],
            rectificativa = vals$grupo$rectificativa[CONTRATO %in% clusters[[cluster]]],
            domesticas = vals$grupo$domesticas[CONTRATO %in% clusters[[cluster]]],
            comisiones = vals$grupo$comisiones[CONTRATO %in% clusters[[cluster]]],
            cobranza = vals$grupo$cobranza[CONTRATO %in% clusters[[cluster]]],
            contratos = vals$grupo$contratos[CONTRATO %in% clusters[[cluster]]],
            sucursal = vals$grupo$sucursal,
            factorContrato = "CONTRATO",
            mesMin = mesMin,
            mesMax = mes_corte_datos,
            modoRectificativa = "A",
            cierreDeMes = mes_rolling
          )
          # convierto en data.table (falta programar hacerlo antes)
          tmp_emision <- lapply(
            tmp_emision, as.data.table
          )

          # achico reportes
          tmp_emision[["rpt1"]] <- tmp_emision[["rpt1"]][,
            .(
              CONTRATO, MES, PER, SALPROM, PRIMA_r, COMI_r
            )
          ]

          # creo que se pueden sacar aún más columnas
          tmp_emision[["rpt2"]] <- tmp_emision[["rpt2"]][,
           .(
             CONTRATO, MES, PER,
             MESES,
             TRABAJADORES, TRABAJADORESMESsinDOM, TRABAJADORESMES,
             MASA_desest,
             TRABAJADORESMES_r, #SALARIO,
             MASA_r, PREMIO_r, PRIMA_r, COMI_r
           )
          ]

          ## reportes siniestralidad

          tmp_siniestros <- vals$grupo$siniestros[DENUNCIA %in% denuncias_en_cluster]

          if(metodo_IBNER == "BF_modificado"){
            # agrego salario promedio emitido
            tmp_siniestros <- tmp_siniestros[
              tmp_emision[["rpt1"]][,.(CONTRATO, MES, SALPROM)],
              SALARIO_EMI := i.SALPROM,
              on = c(CONTRATO = "CONTRATO", MESACC = "MES")
            ]
          }

          tmp_siniestralidad <- do.call(
            genera_reportes_siniestros,
            args = c(
              list(
                reportes = c("rpt1", "rpt6"),
                siniestros = tmp_siniestros,
                liquidaciones = vals$grupo$liquidaciones[DENUNCIA %in% denuncias_en_cluster],
                reservas = vals$grupo$reservas[DENUNCIA %in% denuncias_en_cluster],
                juicios = vals$grupo$juicios[DENUNCIA %in% denuncias_en_cluster],
                contratos = vals$grupo$contratos[CONTRATO %in% clusters[[cluster]]],
                mesMin = mesMin,
                mesMax = mes_corte_datos,
                mes_cierre = mes_corte_datos_max,
                cierreDeMes = mes_rolling,
                # argumentos asociados a funciones externas de ibner e inflación
                metodo_IBNER = metodo_IBNER,
                ibnr_puro = vals$grupo$ibnr_puro
              ),
              args_metodo
            )
          )

          emi_rpt1_list[[cluster]] <- tmp_emision[["rpt1"]]
          emi_rpt2_list[[cluster]] <- tmp_emision[["rpt2"]]
          # emi_rpt5_list[[cluster]] <- tmp_emision[["rpt5"]]
          sdad_rpt1_list[[cluster]] <- tmp_siniestralidad[["rpt1"]]
          sdad_rpt6_list[[cluster]] <- tmp_siniestralidad[["rpt6"]]

          message(paste0(
            "Calculando por etapas: ",
            " clúster ", cluster, " de ", n_cluster, " realizado."
          ))

          updateProgressBar(
            session = session,
            id = "ui_progress_bar_clusters",
            value = cluster / n_cluster * 100,
          )

        }

        rm(
          tmp_siniestros, tmp_siniestralidad, tmp_emision,
          clusters, denuncias_en_cluster
        )

        gc()

        vals$grupo$emision <- vector(mode = "list", length = 3L)
        vals$grupo$emision[[1]] <- rbindlist(emi_rpt1_list, use.names = TRUE)
          rm(emi_rpt1_list)
        vals$grupo$emision[[2]] <- rbindlist(emi_rpt2_list, use.names = TRUE)
          rm(emi_rpt2_list)
        # vals$grupo$emision[[3]] <- rbindlist(emi_rpt5_list, use.names = TRUE)
        #   rm(emi_rpt5_list)
        names(vals$grupo$emision) <- c("rpt1", "rpt2")
        # names(vals$grupo$emision) <- c("rpt1", "rpt2", "rpt5")

        vals$grupo$siniestralidad <- vector(mode = "list", length = 2L)
        vals$grupo$siniestralidad[[1]] <- rbindlist(sdad_rpt1_list, use.names = TRUE)
          rm(sdad_rpt1_list)
        vals$grupo$siniestralidad[[2]] <- rbindlist(sdad_rpt6_list, use.names = TRUE)
          rm(sdad_rpt6_list)
        names(vals$grupo$siniestralidad) <- c("rpt1", "rpt6")

        closeSweetAlert(session = session)

      }

      # Calcular siniestralidad

      # elimino columnas innecesarias del rpt1.
      # Sería más preciso utilizar el rpt2 por CONTRATO y MES, pero
      # al agrupar no tenemos manera de agregar la columna SALARIO_EMI
      # en el parámetro columnas_agrupar ya que solo toma columnas de contrato
      # y como está por siniestro, no es posible diferenciarla de otras columnas.
      # La propuesta es agregar a ... el parámetro columnas_agrupar
      if(metodo_IBNER == "BF_modificado"){
        cols_agrupar_rpt2 <- c(
          "CONTRATO", "CLIENTE", "INIVIG",
          "SALARIO_EMI", "SALARIO_EMI_0",
          "MESACC"
        )
      } else {
        cols_agrupar_rpt2 <- c(
          "CONTRATO", "CLIENTE", "INIVIG",
          "MESACC"
        )
      }

      vals$grupo$siniestralidad[["rpt2"]] <- vals$grupo$siniestralidad[["rpt1"]][,
         lapply(.SD, sum),
         keyby = cols_agrupar_rpt2,
         .SDcols = c(
           "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
           "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
           "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
           "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
           "ILT_LIQ_0", "ESP_LIQ_0", "ILP_LIQ_0", "JUI_LIQ_0", "TOT_LIQ_0",
           "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
         )
      ]

      # aplico la moneda
      vals$grupo$siniestralidad_per <- calcula_siniestralidad(
        reporte_emision_mes = vals$grupo$emision[["rpt1"]],
        reporte_siniestralidad_mes = vals$grupo$siniestralidad[["rpt2"]],
        modo_moneda = modo_moneda,
        columna_prima = "PRIMA_r",
        columnas_agrupar = "PER",
        indice = indice_salarial[,.(MES, INDICE)],
        mes_datos = mes_corte_datos,
        # mes_datos = mes_corte_datos_max,
        sufijo = "_H"
      )

      vals$grupo$siniestralidad_per_cto <- calcula_siniestralidad(
        reporte_emision_mes = vals$grupo$emision[["rpt1"]],
        reporte_siniestralidad_mes = vals$grupo$siniestralidad[["rpt2"]],
        modo_moneda = modo_moneda,
        columna_prima = "PRIMA_r",
        columnas_agrupar = c("CONTRATO", "CLIENTE", "INIVIG", "PER"),
        indice = indice_salarial[,.(MES, INDICE)],
        mes_datos = mes_corte_datos,
        # mes_datos = mes_corte_datos_max,
        sufijo = "_H"
      )


      # elimino algunas columnas innecesarias de rpt1
      columnas_eliminar_rpt1 <- which(
        colnames(vals$grupo$siniestralidad[["rpt1"]]) %in% c(
          "ILT_ULT_H", "ESP_ULT_H",	"ILP_ULT_H",	"JUI_ULT_H",	"JUI_ULT_2017_H",
          "SALARIO_EMI_0", "SALARIO_EMI"
        )
      )
      vals$grupo$siniestralidad[["rpt1"]] <- vals$grupo$siniestralidad[["rpt1"]][,
        !..columnas_eliminar_rpt1
      ]
# browser()
      rpt <- arma_gt_periodo(
        contratos = vals$grupo$contratos,
        emision_per = vals$grupo$emision[["rpt2"]],
        montos_per = vals$grupo$siniestralidad_per,
        frecuencias_per = vals$grupo$siniestralidad[["rpt6"]],
        mes_rolling = mes_rolling,
        periodos_n = periodos_n,
        credibilidad_IBNR = credibilidad_IBNR
      )


      #### cabecera ----

      vals$cabecera <- escribe_cabecera_tablero_HTML(
        vals$grupo,
        grupo_tipo
      )

      output$ui_cabecera <- renderUI({
        vals$cabecera
      })

      ## renders de tablas y gráficos

      vals$gt_tabla <- gt_periodo(
        rpt,
        modo_moneda,
        siniestralidad_target = opciones$siniestralidad_target,
        nota_al_pie_especial = nota_al_pie_especial
      )

      vals$plotsdadibner <- plot_sdad_ibner(
        rpt$PER,
        rpt$PRIMA_0_H,
        rpt$TOT_LIQ_0_H,
        rpt$TOT_RVA,
        rpt$TOT_IBNER_H,
        PER_x = periodos
      )

      vals$plotsdadprima <- plot_sdad_prima(
        rpt$PER,
        rpt$PRIMA_0_H,
        rpt$COMI_r_H,
        rpt$ESP_ULT_H,
        rpt$ILT_ULT_H,
        rpt$ILP_ULT_H,
        rpt$JUI_ULT_H,
        PER_x = periodos
      )

      vals$plotfrectotal <- plot_frec_total(
        rpt$PER,
        rpt$N,
        rpt$N_FINAL,
        rpt$FRSA_N_FINAL,
        PER_x = periodos
      )

      vals$plotfrecjud <- plot_frec_jud(
        rpt$PER,
        rpt$JU,
        rpt$JU_FINAL,
        rpt$FRSA_JU_FINAL,
        PER_x = periodos
      )

      vals$plotfrecporinc <- plot_frec_porinc_grmu(
        rpt$PER,
        rpt$GR,
        rpt$GR_FINAL,
        rpt$MU,
        rpt$MU_FINAL,
        rpt$PORINC_66,
        PER_x = periodos
      )

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

      rpt1_excelDT <- formatear_columnas(
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

      output$lista_siniestros <- renderDataTable(
        rpt1_excelDT,
        server = FALSE
      )

      rpt_sdad_excelDT <- formatear_columnas(
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

      output$lista_siniestralidad <- renderDataTable(
        rpt_sdad_excelDT,
        server = FALSE
      )


    })


    #### botón descargar ----

    # debe estar instalado PahntomJS  webshot::is_phantomjs_installed()
    # instalar con webshot::install_phantomjs()

    output$download <- downloadHandler(
      filename = function(){
        glue(
          "tablero ",
          isolate(input$grupo_tipo),
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
          isolate(input$grupo_tipo),
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
            isolate(input$grupo_tipo),
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
