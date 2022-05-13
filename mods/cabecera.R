cabecera_UI <- function(id) {
  ns <- NS(id)
  tagList(
    #htmlOutput("cabecera")
    uiOutput(ns("cabecera"))
  )
}

cabecera_Server <- function(
  id,
  vals,
  seleccion
){

  moduleServer(id, function(input, output, session) {

    cabecera <- eventReactive(vals$rpt_periodo, {
      escribe_cabecera_tablero_HTML(
        grupo = vals$grupo,
        grupo_tipo = seleccion$grupo_tipo,
        limite_contratos = 20L,
        len_cadenas = 50L
      )
    })

    output$cabecera <- renderUI({
      cabecera()
    })

    return(cabecera)

  })

}
