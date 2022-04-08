#' Gráfico "Indices de Gravedad"
#'
#' Se acopla frecuencia absoluta y porcentaje de incapacidad
#' de siniestros graves en un mismo gráfico
#' mediante el método de equivalencia de ejes.
#'
#' @param   PER              numeric  periodo
#' @param   GR               numeric  nro de siniestros graves
#' @param   GR_ULT           numeric  nro de siniestros graves ultimate
#' @param   MU               numeric  nro de siniestros fallecidos
#' @param   MU_ULT           numeric  nro de siniestros fallecidos ultimate
#' @param   PORINC_ULT       numeric  porcentaje de incapacidad ultimate
#' @param   PER_x            numeric  eje_x de PER. Default = NULL
#'
#' @return
#' @export
#'
#' @examples
#' rpt <- arma_gt_periodo(
#'   contratos = grupo$contratos,
#'   emision = grupo$emision$rpt2,
#'   siniestros = grupo$siniestralidad$rpt1,
#'   frecuencias = grupo$siniestralidad$rpt6,
#'   montos = grupo$siniestralidad$rpt2,
#'   mesMax = mesMax
#' )
#'
#' plot <- plot_frec_porinc_grmu(
#'   rpt$PER,
#'   rpt$GR,
#'   rpt$GR_ULT,
#'   rpt$MU,
#'   rpt$MU_ULT,
#'   rpt$PORINC_ULT,
#'   periodos
#' )
#'
#' grid.newpage()
#' grid.draw(plot)

plot_frec_porinc_grmu <- function(
  PER,
  GR,
  GR_ULT,
  MU,
  MU_ULT,
  PORINC_ULT,
  PER_x = NULL
){

  df <- data.frame(
    PER,
    GR,
    GR_ULT,
    MU,
    MU_ULT,
    PORINC_ULT
  )
  names(df) <- c(
    "PER",
    "GR",
    "GR_ULT",
    "MU",
    "MU_ULT",
    "PORINC_ULT"
  )

  df <- df %>%
    mutate(
      GRMU_ULT = GR_ULT + MU_ULT,
      GRMU = GR + MU,
      GRMU_IBNR = GRMU_ULT - GRMU
    )

  # cálculo del factor de escala
  ymax_grmu <- roundUpNice(pmax(1, max(df$GRMU_ULT)))
  ymax_PORINC_ULT <- roundUpNice(
    pmax(0.1, max(PORINC_ULT)),
    nice = c(1,2,4,5,6,8,10,15,20,50,100)
  )
  y_escala_PORINC_ULT <- ymax_grmu / ymax_PORINC_ULT

  # datos
  df <- df %>%
    select(
      PER,
      GRMU, GRMU_IBNR, GRMU_ULT,
      PORINC_ULT
    ) %>%
    pivot_longer(
      - c(PER, PORINC_ULT, GRMU_ULT),
      names_to = "referencia", values_to = "siniestros_grmu_q"
    )

  # eje x de tiempo
  p <- ggplot(df, aes(x = PER))
  # siniestros observados e ibnr
  p <- p + geom_bar(
    aes(
      y = siniestros_grmu_q,
      fill = factor(referencia, levels = c("GRMU_IBNR", "GRMU"), ordered = TRUE),
      group = referencia
    ),
    stat = "identity",
    colour = "maroon",
    width = 0.80
  )
  # siniestros graves totales
  p <- p + geom_text(
    aes(
      label = round(GRMU_ULT, 0),
      x = PER,
      # coordenada para que no desborde el total de siniestros
      y = pmin(GRMU_ULT, 0.9 * ymax_grmu),
      group = "Total"
    ),
    vjust = -1,
    colour = "black",
    fontface = "bold"
  )

  # incapacidad promedio (transformada)
  p <- p + geom_line(
    aes(y = PORINC_ULT * y_escala_PORINC_ULT, group = "Incapacidad [%]"),
    stat = "identity",
    size = 1,
    colour = "darkorange2"
  ) +
    geom_point(
      aes(y = PORINC_ULT * y_escala_PORINC_ULT, group = "Incapacidad [%]"),
      color = "chocolate3", size = 3
    )

  # eje x
  if(!is.null(PER_x)) {
    p <- p + coord_cartesian (xlim = c(PER_x[1] - 0.5, PER_x[length(PER_x)] + 0.5))
  }

  # eje secundario: se revierte la transformación anterior
  p <- p + scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~./y_escala_PORINC_ULT,
      name = "Incapacidad [%]",
      labels = function(x) paste0(x, "%")
    )
  )

  # formatos ----
  p <- p + theme_classic()
  # colores
  p <- p + scale_discrete_manual(
    aesthetics = "fill",
    name = "Q Stros Graves/Mu",
    labels = c("IBNR", "Observado"),
    values = c("hotpink3", "violetred4")
  )

  # ejes
  p <- p + labs(y = "Siniestros Graves ULT", x = NULL)
  # leyenda
  p <- p + theme(
    legend.position = "bottom",
    # legend.position = c(0.2, 0.2),
    legend.background = element_rect(
      fill = alpha("darkorchid1", 0.4), color = "maroon", linetype = "solid"
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
