cuadro_UI <- function(id) {
  ns <- NS(id)
  tagList(
    gt_output(outputId = ns("cuadro"))
  )
}

cuadro_Server <- function(
  id,
  vals,
  modo_moneda,
  siniestralidad_target,
  nota_al_pie_especial = ""
){

  library(gt)

  moduleServer(id, function(input, output, session) {

    #req(vals)
    gt_tabla <- eventReactive(vals$rpt_periodo, {
      gt_periodo(
        vals$rpt_periodo,
        modo_moneda,
        siniestralidad_target = siniestralidad_target,
        nota_al_pie_especial = nota_al_pie_especial
      )
    })

    output$cuadro <- render_gt({
      gt_tabla()
    })

  })

}
