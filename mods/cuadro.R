cuadro_UI <- function(id) {
  ns <- NS(id)
  tagList(
    gt_output(outputId = ns("cuadro"))
  )
}

cuadro_Server <- function(
  id,
  rpt_periodo,
  modo_moneda,
  siniestralidad_target,
  nota_al_pie_especial = ""
){

  library(gt)

  moduleServer(id, function(input, output, session) {

    gt_tabla <- eventReactive(rpt_periodo(), {
      gt_periodo(
        rpt_periodo(),
        modo_moneda,
        siniestralidad_target = siniestralidad_target,
        nota_al_pie_especial = nota_al_pie_especial
      )
    })

    output$cuadro <- render_gt({
      gt_tabla()
      # req(rpt_periodo())
      # gt_periodo(
      #   rpt_periodo(),
      #   modo_moneda,
      #   siniestralidad_target = siniestralidad_target,
      #   nota_al_pie_especial = nota_al_pie_especial
      # )
    })

    return(gt_tabla)

  })

}
