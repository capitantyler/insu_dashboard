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

    grafico <- eventReactive(vals$data,{
      with(
        vals$data[["siniestralidad_per"]],
        plot_sdad_prima(
          PER = PER,
          PRIMA_ANUAL = PRIMA_0_H,
          COMI_ANUAL = COMI_r_H,
          ESPECIE = ESP_ULT_H,
          ILT = ILT_ULT_H,
          ILP = ILP_ULT_H,
          JUICIOS = JUI_ULT_H,
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
