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

    grafico <- eventReactive(vals,{
      with(
        vals,
        plot_frec_jud(
          PER = PER,
          JU = JU,
          JU_ULT = JU_FINAL,
          FRSA_JU_ULT = FRSA_JU_FINAL,
          PER_x = periodo
        )
      )
    })

    output$grafico <- renderPlot({
      grafico()
    })

    return(grafico)

  })

}
