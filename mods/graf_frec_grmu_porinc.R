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

    grafico <- eventReactive(vals,{
      with(
        vals,
        plot_frec_grmu_porinc(
          PER = PER,
          GR = GR,
          GR_ULT = GR_FINAL,
          MU = MU,
          MU_ULT = MU_FINAL,
          PORINC_ULT= PORINC_66,
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
