graf_sdad_rvas_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Siniestralidad, Reservas e IBNER",
      width = 12,
      plotOutput(outputId = ns("grafico"))
    )
  )
}

graf_sdad_rvas_Server <- function(
  id,
  vals,
  periodo
){

  moduleServer(id, function(input, output, session) {

    #req(vals)
    grafico <- eventReactive(vals$rpt_periodo, {
      plot_sdad_rvas(
        vals$rpt_periodo$PER,
        vals$rpt_periodo$PRIMA_0_H,
        vals$rpt_periodo$TOT_LIQ_0_H,
        vals$rpt_periodo$TOT_RVA,
        vals$rpt_periodo$TOT_IBNER_H,
        PER_x = periodo
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

  })

}
