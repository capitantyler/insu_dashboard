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

    grafico <- eventReactive(vals,{
      with(
        vals,
        plot_frec_total(
          PER = PER,
          N = N,
          N_ULT = N_FINAL,
          FRSA_N_ULT = FRSA_N_FINAL,
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
