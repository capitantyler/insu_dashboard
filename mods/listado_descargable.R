listado_descargable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(outputId = ns("cuadro"))
  )
}

listado_descargable_Server <- function(
  id,
  listado
){

  library(DT)

  moduleServer(id, function(input, output, session) {

    observeEvent(listado, {
      browser()
    }, ignoreInit = TRUE)

    listado_local <- eventReactive(listado, {
      formatear_columnas(
        metodo = "DT",
        objeto = datatable(
          #data = listado$grupo$siniestralidad$rpt1,
          data = listado,
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

    output$cuadro <- renderDataTable(
      expr = listado_local(),
      server = FALSE
    )

  })

}
