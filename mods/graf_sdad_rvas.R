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

    grafico <- eventReactive(vals$data,{
      with(
        vals$data[["siniestralidad_per"]],
        plot_sdad_rvas(
          PER = PER,
          PRIMA_ANUAL = PRIMA_0_H,
          TOT_LIQ = TOT_LIQ_0_H,
          TOT_RVA = TOT_RVA,
          TOT_IBNER = TOT_IBNER_H,
          PER_x = periodo
        )
      )
    })

    output$grafico <- renderPlot({
      grafico()
      # req(vals$data)
      # with(
      #   vals$data[["siniestralidad_per"]],
      #   plot_sdad_rvas(
      #     PER = PER,
      #     PRIMA_ANUAL = PRIMA_0_H,
      #     TOT_LIQ = TOT_LIQ_0_H,
      #     TOT_RVA = TOT_RVA,
      #     TOT_IBNER = TOT_IBNER_H,
      #     PER_x = periodo
      #   )
      # )
    })

    return(grafico)

  })

}
