#' Gráfico "Indice de Judicialidad"
#'
#' Se acopla frecuencia absoluta y relativa de juicios en un mismo gráfico
#' mediante el método de equivalencia de ejes.
#'
#' @param   PER              numeric  periodo
#' @param   N_JU             numeric  nro de siniestros con juicios
#' @param   N_JU_ULT         numeric  nro de siniestros con juicios ultimate
#' @param   FRSA_JU_ULT      numeric  frecuencia de juicios ultimate
#' @param   PER_x            numeric  eje_x de PER. Default = NULL
#'
#' @return
#' @export
#'
#' @examples
#' per <- 2016:2020
#' rpt <- arma_gt_periodo.f(
#'   contratos = grupo$contratos,
#'   emision = grupo$emision$rpt2,
#'   siniestros = grupo$siniestralidad$rpt1,
#'   frecuencias = grupo$siniestralidad$rpt6,
#'   montos = grupo$siniestralidad$rpt2,
#'   mesMax = mesMax
#' )
#'
#' plot <- plot_frec_jud(
#'   rpt$PER,
#'   rpt$JU,
#'   rpt$N_JU_ULT,
#'   rpt$FRSA_JU_FINAL,
#'   per
#' )
#'
#' grid.newpage()
#' grid.draw(plot)

plot_frec_jud <- function(
  PER,
  JU,
  JU_ULT,
  FRSA_JU_ULT,
  PER_x = NULL
){

  # cálculo del factor de escala
  ymax_juicios <- roundUpNice(pmax(1, max(JU_ULT)))
  ymax_frec_ju <- roundUpNice(
    pmax(0.1, max(FRSA_JU_ULT)),
    nice = c(1,2,4,5,6,8,10,15,20,50,100)
  )
  y_escala_frec <- ymax_juicios / ymax_frec_ju

  # datos
  df <- data.frame(
    PER,
    JU,
    JU_ULT,
    FRSA_JU_ULT
  )
  names(df) <- c(
    "PER",
    "JU",
    "JU_ULT",
    "FRSA_JU_ULT"
  )

  df <- df %>%
    mutate(
      JU_IBNR = JU_ULT - JU
    ) %>%
    select(
      PER,
      JU, JU_IBNR, JU_ULT, FRSA_JU_ULT
    ) %>%
    pivot_longer(
      - c(PER, FRSA_JU_ULT, JU_ULT),
      names_to = "referencia", values_to = "siniestros_ju_q"
    )

  # eje x de tiempo
  p <- ggplot(df, aes(x = PER))
  # siniestros observados e ibnr
  p <- p + geom_bar(
    aes(
      y = siniestros_ju_q,
      fill = factor(referencia, levels = c("JU_IBNR", "JU"), ordered = TRUE)
    ),
    stat = "identity",
    colour = "olivedrab3",
    width = 0.80
  )
  # siniestros totales
  p <- p + geom_text(
    aes(
      label = round(JU_ULT, 0),
      x = PER,
      # coordenada para que no desborde el total de siniestros
      y = pmin(JU_ULT, 0.9 * ymax_juicios),
      group = "Total"
    ),
    vjust = -1,
    colour = "black",
    fontface = "bold"
  )

  # frecuencia totales (transformada)
  p <- p + geom_line(
    aes(y = FRSA_JU_ULT * y_escala_frec, group = "FRJA(acc) ULT"),
    stat = "identity",
    size = 1,
    colour = "tomato2"
    # show.legend = TRUE

  ) +
    geom_point(
      aes(y = FRSA_JU_ULT * y_escala_frec, group = "FRJA(acc) ULT"),
      color = "brown4", size = 3
    )

  # eje x
  if(!is.null(PER_x)) {
    p <- p + coord_cartesian (xlim = c(PER_x[1] - 0.5, PER_x[length(PER_x)] + 0.5))
  }

  # eje secundario: se revierte la transformación anterior
  p <- p + scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~./y_escala_frec,
      name = "FRJA(acc) ULT [%]",
      labels = function(x) paste0(x, "%")
    )
  )

  # formatos ----
  p <- p + theme_classic()
  # colores
  p <- p + scale_discrete_manual(
    aesthetics = "fill",
    name = "Q Juicios xAcc",
    labels = c("IBNR", "Observado"),
    values = c("springgreen3", "olivedrab3")
  )

  # ejes
  p <- p + labs(y = "Juicios ULT", x = NULL)
  # leyenda
  p <- p + theme(
    legend.position = "bottom",
    # legend.position = c(0.2, 0.2),
    legend.background = element_rect(
      fill = alpha("chartreuse", 0.4), color = "olivedrab4", linetype = "solid"
    )
  )
  # líneas de fondo
  p <- p + theme(
    panel.grid.major.y = element_line(
      size = 0.5, linetype = 'solid', colour = "gray80"),
    panel.grid.major.x = element_blank()
  )

  return(p)

}
