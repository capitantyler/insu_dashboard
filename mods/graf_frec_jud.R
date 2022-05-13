graf_frec_jud_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Indice de Judicialidad",
      width = 12,
      plotOutput(outputId = ns("grafico"))
    )
  )
}

graf_frec_jud_Server <- function(
  id,
  vals,
  periodo
){

  moduleServer(id, function(input, output, session) {

    grafico <- eventReactive(vals$rpt_periodo, {
      plot_frec_jud(
        vals$rpt_periodo$PER,
        vals$rpt_periodo$JU,
        vals$rpt_periodo$JU_FINAL,
        vals$rpt_periodo$FRSA_JU_FINAL,
        PER_x = periodo
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

    return(grafico)

  })

}
