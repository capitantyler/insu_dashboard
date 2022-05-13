graf_frec_grmu_porinc_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Indice de Gravedad",
      width = 12,
      plotOutput(outputId = ns("grafico"))
    )
  )
}

graf_frec_grmu_porinc_Server <- function(
  id,
  vals,
  periodo
){

  moduleServer(id, function(input, output, session) {

    grafico <- eventReactive(vals$rpt_periodo, {
      plot_frec_grmu_porinc(
        vals$rpt_periodo$PER,
        vals$rpt_periodo$GR,
        vals$rpt_periodo$GR_FINAL,
        vals$rpt_periodo$MU,
        vals$rpt_periodo$MU_FINAL,
        vals$rpt_periodo$PORINC_66,
        PER_x = periodo
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

  })

}
