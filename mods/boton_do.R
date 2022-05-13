boton_do <- function(id, label = "CONSULTA") {
  ns <- NS(id)
  tagList(
    actionButton(ns("boton_do"), label = label)
  )
}

boton_do_server <- function(
  id,
  # valores reactivos compartidos
  vals,
  seleccion,
  success,
  # variables-funciones seleccionadas como inputs, todas reactivas ()
  sin_covid19,
  mes_rolling,
  mes_min,
  mes_corte_datos,
  periodos_n,
  metodo_IBNER,
  modo_moneda,
  indice_salarial,
  args_metodo
) {

  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$boton_do, {
        req(seleccion$grupo_id)

        #timestamp(suffix = glue(": click boton do {session$ns(NULL)}"))
        # if (n == 0) {
        #   disable("download")
        #   # return()
        # } else {
        #   enable("download")
        # }

        # completa periodos de credibilidad IBNR
        if(periodos_n() > 5L){
          credibilidad_IBNR <- c(rep(1, periodos_n() - 5L), credibilidad_IBNR_5)
        } else {
          credibilidad_IBNR <- credibilidad_IBNR_5[(5L-periodos_n()+1L):5L]
        }

        filtroContratos <- if(
          seleccion$grupo_tipo != "Asociart"
        ){

          vals$nota_de_calculo <- ""

          filtroContratos <- filtrar_nros_contrato(
            seleccion$grupo_tipo, seleccion$grupo_id
          )

        } else {

          # debido a la voluminosidad,
          # calculamos Asociart enRégimen General
          # sólo contratos con emisión
          # filtroContratos <- NA

          vals$nota_de_calculo <- paste0(
            "No incluye contratos de régimen doméstico ni ",
            "contratos con prima emitida $ 0."
          )

          # filtroContratos <- unique(
          #   contratos.data[
          #     TIPO != "D" ,
          #     .(CONTRATO, GRUPO_EC)
          #   ][
          #     emiorig.data[MES >= mes_min(),
          #       .(
          #         PRIMA = sum(PRIMA),
          #         TRABAJADORES = sum(TRABAJADORES[MES == mes_corte_datos()])
          #       ),
          #       by = CONTRATO
          #     ],
          #     on = .(CONTRATO),
          #     nomatch = NULL
          #   ][
          #     PRIMA > 0 &
          #       (TRABAJADORES >= 50
          #        | (TRABAJADORES < 50 & GRUPO_EC != "SD")
          #       )
          #   ][["CONTRATO"]]
          # )
          #
          filtroContratos <- unique(
            contratos.data[
              TIPO != "D" ,
              .(CONTRATO, GRUPO_EC)
            ][
              emiorig.data[MES >= mes_min(),
               .(
                 PRIMA = sum(PRIMA),
                 TRABAJADORES = sum(TRABAJADORES[MES == mes_corte_datos()])
               ),
               by = CONTRATO
              ],
              on = .(CONTRATO),
              nomatch = NULL
            ][
              PRIMA > 0 &
                TRABAJADORES < 50 & GRUPO_EC == "SD"
            ][["CONTRATO"]]
          )

        }

        vals$grupo <- filtrar_tablas(
          filtroContratos
        )

        # filtros por fecha
        vals$grupo$emiorig <- vals$grupo$emiorig[
          MES >= mes_min() & MES <= mes_corte_datos()
        ]

        if(
          length(filtroContratos) == 0 |
          nrow(vals$grupo$emiorig) == 0
        ){

          sendSweetAlert(
            #session = session,
            title   = HTML("Ups..."),
            text    = HTML("No hay datos en el periodo para los contratos seleccionado..."),
            type    = "warning",
            html    = TRUE
          )
          req(FALSE)
        }

        vals$grupo$liquidaciones <- vals$grupo$liquidaciones[
          MESLIQ <= mes_corte_datos()
        ]

        vals$grupo$reservas <- vals$grupo$reservas[
          MES <= mes_corte_datos()
        ]

        vals$grupo$siniestros <- vals$grupo$siniestros[
          MESACC <= mes_corte_datos()
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
        if(sin_covid19() == TRUE) {
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

        if(metodo_IBNER() %in% metodos_BF){

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
               (mes_corte_datos() %/% 100- MES %/% 100)  * 12L +
                 (mes_corte_datos() %% 100- MES %% 100) + 1L,
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

          if(metodo_IBNER() %in% c("BF", "BF_modificado")){

            if(metodo_IBNER() == "BF"){
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

            if(metodo_IBNER() == "BF_modificado"){
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
            emiorig = vals$grupo$emiorig,
            rectificativa = vals$grupo$rectificativa,
            domesticas = vals$grupo$domesticas,
            contratos = vals$grupo$contratos,
            sucursal = vals$grupo$sucursal,
            factorContrato = "CONTRATO",
            mesMin = mes_min(),
            mesMax = mes_corte_datos(),
            modoRectificativa = "A",
            cierreDeMes = mes_rolling()
          )

          # convierto en data.table (falta programar hacerlo antes)
          vals$grupo$emision <- lapply(
            vals$grupo$emision, as.data.table
          )

          # achico reportes
          vals$grupo$emision[["rpt1"]] <- vals$grupo$emision[["rpt1"]][,
             .(
               CONTRATO, CLIENTE, INIVIG, MES, PER, SALPROM, PRIMA_r, COMI_r
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

          if(metodo_IBNER() == "BF_modificado"){
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
                mesMin = mes_min(),
                mesMax = mes_corte_datos(),
                mes_cierre = mes_corte_datos(),
                cierreDeMes = mes_rolling(),
                # argumentos asociados a funciones externas de ibner e inflación
                metodo_IBNER = metodo_IBNER(),
                ibnr_puro = vals$grupo$ibnr_puro
              ),
              args_metodo()
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
          sdad_rpt1_list <- vector(mode = "list", length = n_cluster)
          sdad_rpt6_list <- vector(mode = "list", length = n_cluster)


          for(cluster in seq_along(clusters)){

            denuncias_en_cluster <- vals$grupo$siniestros[
              CONTRATO %in% clusters[[cluster]]
            ][["DENUNCIA"]]

            ## reportes emisión
            tmp_emision <- EmisionConMovimientos(
              reportes = c("rpt1", "rpt2"),
              emiorig = vals$grupo$emiorig[CONTRATO %in% clusters[[cluster]]],
              rectificativa = vals$grupo$rectificativa[CONTRATO %in% clusters[[cluster]]],
              domesticas = vals$grupo$domesticas[CONTRATO %in% clusters[[cluster]]],
              contratos = vals$grupo$contratos[CONTRATO %in% clusters[[cluster]]],
              sucursal = vals$grupo$sucursal,
              factorContrato = "CONTRATO",
              mesMin = mes_min(),
              mesMax = mes_corte_datos(),
              modoRectificativa = "A",
              cierreDeMes = mes_rolling()
            )
            # convierto en data.table (falta programar hacerlo antes)
            tmp_emision <- lapply(
              tmp_emision, as.data.table
            )

            # achico reportes
            tmp_emision[["rpt1"]] <- tmp_emision[["rpt1"]][,
             .(
               CONTRATO, CLIENTE, INIVIG, MES, PER, SALPROM, PRIMA_r, COMI_r
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

            if(metodo_IBNER() == "BF_modificado"){
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
                  mesMin = mes_min(),
                  mesMax = mes_corte_datos(),
                  mes_cierre = mes_corte_datos(),
                  cierreDeMes = mes_rolling(),
                  # argumentos asociados a funciones externas de ibner e inflación
                  metodo_IBNER = metodo_IBNER(),
                  ibnr_puro = vals$grupo$ibnr_puro
                ),
                args_metodo()
              )
            )

            emi_rpt1_list[[cluster]] <- tmp_emision[["rpt1"]]
            emi_rpt2_list[[cluster]] <- tmp_emision[["rpt2"]]
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
          names(vals$grupo$emision) <- c("rpt1", "rpt2")

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
        if(metodo_IBNER() == "BF_modificado"){
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
          modo_moneda = modo_moneda(),
          columna_prima = "PRIMA_r",
          columnas_agrupar = "PER",
          indice = indice_salarial(),
          mes_datos = mes_corte_datos(),
          sufijo = "_H"
        )

        vals$grupo$siniestralidad_per_cto <- calcula_siniestralidad(
          reporte_emision_mes = vals$grupo$emision[["rpt1"]],
          reporte_siniestralidad_mes = vals$grupo$siniestralidad[["rpt2"]],
          modo_moneda = modo_moneda(),
          columna_prima = "PRIMA_r",
          columnas_agrupar = c("CONTRATO", "CLIENTE", "INIVIG", "PER"),
          indice = indice_salarial(),
          mes_datos = mes_corte_datos(),
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

        vals$rpt_periodo <- arma_gt_periodo(
          contratos = vals$grupo$contratos,
          emision_per = vals$grupo$emision[["rpt2"]],
          montos_per = vals$grupo$siniestralidad_per,
          frecuencias_per = vals$grupo$siniestralidad[["rpt6"]],
          mes_rolling = mes_rolling(),
          periodos_n = periodos_n(),
          credibilidad_IBNR = credibilidad_IBNR
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
              class = "display",
              scrollX = TRUE
            )
          )
        )

        #timestamp(suffix = glue(": final boton do {session$ns(NULL)}"))

        success$boton_do <- TRUE

        return(
          list(
            success = TRUE,
            sdad_rpt1 = reactive(vals$grupo$siniestralidad[["rpt1"]]),
            rpt_periodo = reactive(vals$rpt_periodo)
          )
        )

      }, ignoreInit = TRUE)

    }
  )
}
