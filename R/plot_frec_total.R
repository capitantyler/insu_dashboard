#' Gráfico "Indice de Incidencia"
#'
#' Se acopla frecuenciaabsoluta y relativa en un mismo gráfico
#' mediante el método de equivalencia de ejes.
#'
#' @param   PER              numeric  periodo
#' @param   N                numeric  nro de siniestros
#' @param   N_ULT            numeric  nro de siniestros ultimate
#' @param   FRSA_N_ULT       numeric  frecuencia ultimate
#' @param   PER_x            numeric  eje_x de periodos. Default = NULL
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
#' plot <- plot_frec_total(
#'   rpt$PER,
#'   rpt$N,
#'   rpt$N_ULT,
#'   rpt$FRSA_N_FINAL,
#'   per
#' )
#'
#' grid.newpage()
#' grid.draw(plot)

plot_frec_total <- function(
  PER,
  N,
  N_ULT,
  FRSA_N_ULT,
  PER_x = NULL
){

  # cálculo del factor de escala
  ymax_stros <- roundUpNice(pmax(1, max(N_ULT)))
  ymax_frec <- roundUpNice(
    pmax(0.1, max(FRSA_N_ULT)),
    nice = c(1,2,4,5,6,8,10,15,20,50,100)
  )
  y_escala_frec <- ymax_stros / ymax_frec

  # datos
  df <- data.frame(
    PER,
    N,
    N_ULT,
    FRSA_N_ULT
  )
  names(df) <- c(
    "PER",
    "N",
    "N_ULT",
    "FRSA_N_ULT"
  )


  df <- df %>%
    mutate(
      N_IBNR = N_ULT - N
    ) %>%
    select(
      PER, N, N_IBNR, N_ULT, FRSA_N_ULT
    ) %>%
    pivot_longer(
      - c(PER, FRSA_N_ULT, N_ULT),
      names_to = "referencia", values_to = "siniestros_q"
    )

  # eje x de tiempo
  p <- ggplot(df, aes(x = PER))
  # siniestros observados e ibnr
  p <- p + geom_bar(
    aes(
      y = siniestros_q,
      fill = factor(referencia, levels = c("N_IBNR", "N"), ordered = TRUE)
      # group = referencia
    ),
    stat = "identity",
    colour = "sienna3",
    width = 0.80
  )
  # siniestros totales
  p <- p + geom_text(
    aes(
      label = round(N_ULT, 0),
      x = PER,
      # coordenada para que no desborde el total de siniestros
      y = pmin(N_ULT, 0.9 * ymax_stros),
      group = "Total"
    ),
    vjust = -1,
    colour = "black",
    fontface = "bold"
    # position = position_jitter(height = 0.2)
  )

  # frecuencia totales (transformada)
  p <- p + geom_line(
    aes(y = FRSA_N_ULT * y_escala_frec, group = "FRSA ULT"),
    stat = "identity",
    size = 1,
    colour = "slateblue2"
  ) +
    geom_point(
      aes(y = FRSA_N_ULT * y_escala_frec, group = "FRSA ULT"),
      color = "navyblue", size = 3
    )

  # eje x
  if(!is.null(PER_x)) {
    p <- p + coord_cartesian (xlim = c(PER_x[1] - 0.5, PER_x[length(PER_x)] + 0.5))
  }

  # eje secundario: se revierte la transformación anterior
  p <- p + scale_y_continuous(
    sec.axis = sec_axis(
      guide = guide_axis(check.overlap = TRUE),
      trans = ~./y_escala_frec,
      name = "FRSA ULT [%]",
      labels = function(x) paste0(x, "%")
    )
  )

  # formatos ----
  p <- p + theme_classic()
  # colores
  p <- p + scale_fill_manual(
    name = "Q siniestros",
    labels = c("IBNR", "Observado"),
    values = c("firebrick4", "tan1")
  )

  # ejes
  p <- p + labs(y = "Siniestros ULT", x = NULL)
  # leyenda
  p <- p + theme(
    legend.position = "bottom",
    # legend.position = c(0.2, 0.2),
    legend.background = element_rect(
      fill = alpha("darkorange", 0.4), color = "chocolate4", linetype = "solid"
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
