graf_sdad_prima_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Prima y Siniestralidad",
      width = 12,
      plotOutput(outputId = ns("grafico"))
    )
  )
}

graf_sdad_prima_Server <- function(
  id,
  vals,
  periodo
){

  moduleServer(id, function(input, output, session) {

    grafico <- eventReactive(vals$rpt_periodo, {
      plot_sdad_prima(
        vals$rpt_periodo$PER,
        vals$rpt_periodo$PRIMA_0_H,
        vals$rpt_periodo$COMI_r_H,
        vals$rpt_periodo$ESP_ULT_H,
        vals$rpt_periodo$ILT_ULT_H,
        vals$rpt_periodo$ILP_ULT_H,
        vals$rpt_periodo$JUI_ULT_H,
        PER_x = periodo
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

  })

}
