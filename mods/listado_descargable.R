listado_descargable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("exporta_excel"), label = "Exportar Excel"),
    DTOutput(outputId = ns("listadoDT")),
  )
}

listado_descargable_Server <- function(
  id,
  listado,
  seleccion
){

  library(DT)

  moduleServer(id, function(input, output, session) {

    output$listadoDT <- renderDataTable(
      expr = {
        req(listado())
        formatear_columnas(
          metodo = "DT",
          objeto = datatable(
            data = listado(),
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
      },
      server = FALSE
    )

    output$exporta_excel <- downloadHandler(

      filename = function(){
        glue(
          "{id} ",
          seleccion$grupo_tipo,
          " ",
          substr(paste(seleccion$grupo_id, collapse = " "), 1, 50),
          ".xlsx"
        )
      },

      content = function(file) {

        disable("div_boton_do")

        wb <- createWorkbook()
        addWorksheet(wb = wb, sheetName = id)
        writeData(wb = wb,sheet = id, x = listado())
        wb <- formatear_columnas(
          wb, metodo = "openxlsx",
          hoja = id,
          columnas_wb = names(listado()),
          filas_wb = nrow(listado()),
          hoja_estilos = stl
        )

        enable("div_boton_do")

        saveWorkbook(wb, file = file, overwrite = TRUE)

      }
    )

  })

}
