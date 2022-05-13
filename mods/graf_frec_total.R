graf_frec_total_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Indice de Incidencia",
      width = 12,
      plotOutput(outputId = ns("grafico"))
    )
  )
}

graf_frec_total_Server <- function(
  id,
  vals,
  periodo
){

  moduleServer(id, function(input, output, session) {

    grafico <- eventReactive(vals$rpt_periodo, {
      plot_frec_total(
        vals$rpt_periodo$PER,
        vals$rpt_periodo$N,
        vals$rpt_periodo$N_FINAL,
        vals$rpt_periodo$FRSA_N_FINAL,
        PER_x = periodo
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

  })

}
