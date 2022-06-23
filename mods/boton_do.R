boton_do <- function(id, label = "CONSULTA") {
  ns <- NS(id)
  tagList(
    actionButton(ns("boton_do"), label = label)
  )
}

boton_do_server <- function(
  id,
  # valores reactivos compartidos
  emision,
  siniestralidad,
  rpt_sdad_boton_do,
  seleccion,
  success,
  # variables-funciones seleccionadas como inputs, todas reactivas ()
  mes_corte_datos,
  modo_moneda,
  indice_salarial
) {

  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$boton_do, {

        data <- list()
        success$boton_do <- FALSE

        data$siniestralidad_per <- calcula_siniestralidad(
          reporte_emision_mes = emision()[["rpt1"]],
          reporte_siniestralidad_mes = siniestralidad()[["rpt2"]],#siniestralidad()[["rpt2"]],
          modo_moneda = modo_moneda(),
          columna_prima = "PRIMA_r",
          columnas_agrupar = "PER",
          indice = indice_salarial(),
          mes_datos = mes_corte_datos(),
          sufijo = "_H"
        )

        data$siniestralidad_per_cto <- calcula_siniestralidad(
          reporte_emision_mes = emision()[["rpt1"]],
          reporte_siniestralidad_mes = siniestralidad()[["rpt2"]],
          modo_moneda = modo_moneda(),
          columna_prima = "PRIMA_r",
          columnas_agrupar = c("CONTRATO", "CLIENTE", "INIVIG", "PER"),
          indice = indice_salarial(),
          mes_datos = mes_corte_datos(),
          sufijo = "_H"
        )

        success$boton_do <- TRUE

        rpt_sdad_boton_do$data <- data

      }, ignoreInit = TRUE)

    }
  )
}
