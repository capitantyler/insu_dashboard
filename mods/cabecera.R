cabecera_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cabecera"))
  )
}

cabecera_Server <- function(
  id,
  cabecera
){

  moduleServer(id, function(input, output, session) {

    output$cabecera <- renderUI({
      req(cabecera())
      cabecera()
    })

  })

}
