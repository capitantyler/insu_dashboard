#' Gráfico "Prima y Siniestralidad"
#'
#' Se acopla prima y siniestros en moneda corriente y
#' porcentaje de siniestralidad en un mismo gráfico
#' mediante el método de equivalencia de ejes.
#'
#' @param PER
#' @param PRIMA_ANUAL
#' @param COMI_ANUAL
#' @param ESPECIE
#' @param ILT
#' @param ILP
#' @param JUICIOS
#' @param PER_x = NULL
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
#' plot <- plot_sdad_prima(
#'   rpt$PER,
#'   rpt$PRIMA_r,
#'   rpt$COMI_r,
#'   rpt$ESP_ULT,
#'   rpt$ILT_ULT,
#'   rpt$ILP_ULT,
#'   rpt$JUI_ULT,
#'   PER_x = periodos
#' )
#'
#' plot

plot_sdad_prima <- function(
  PER,
  PRIMA_ANUAL,
  COMI_ANUAL,
  ESPECIE,
  ILT,
  ILP,
  JUICIOS,
  PER_x = NULL
){

  # PER
  PER_max <- max(PER)
  PER_min <- min(PER)
  if(is.null(PER_x)) PER_x <- PER_min:PER_max
  PER_n <- length(PER_x)

  # ancho de la columna
  step <- 0.4 * (PER_max - PER_min)/(PER_n - 1)
  # distancia entre columnas
  vcol_pad <- 0.05 * step

  # datos
  df <- data.frame(
    PER,
    PRIMA_ANUAL,
    COMI_ANUAL,
    ESPECIE,
    ILT,
    ILP,
    JUICIOS
  )
  names(df) <- c(
    "PER",
    "PRIMA_ANUAL",
    "COMI_ANUAL",
    "ESPECIE",
    "ILT",
    "ILP",
    "JUICIOS"
  )


  df <- df %>%
    mutate(
      TOTAL = ESPECIE + ILT + ILP + JUICIOS,
      across(
        c(COMI_ANUAL, ESPECIE, ILT, ILP, JUICIOS, TOTAL),
        .fns = ~.*(-1)
      ),
      TOTAL_SDAD = if_else(
        PRIMA_ANUAL == 0, 0, TOTAL / PRIMA_ANUAL
      )
    ) %>%
    select(
      - c(TOTAL)
    ) %>%
    pivot_longer(
      - c(PER, TOTAL_SDAD),
      names_to = "tipo", values_to = "valor"
    ) %>%
    mutate(
      grupo = if_else(tipo != "PRIMA_ANUAL", "egreso", "ingreso")
    ) %>%
    # complete(
    #   PER = PER_x
    # ) %>%
    arrange(
      PER, desc(grupo)
    ) %>%
    # ordenado ya por periodo y tipo de variable, se calcula el rdo acumulado
    mutate(
      rdo_acum = cumsum(valor)
    ) %>%
    # datos para el waterfall
    mutate(
      ymax = rdo_acum - valor,
      ymin = rdo_acum,
      xmin = if_else(grupo == "ingreso", PER - !!step, PER + !!vcol_pad),
      xmax = if_else(grupo == "ingreso", PER - !!vcol_pad, PER + !!step)
    ) %>%
    group_by(
      PER
    ) %>%
    mutate(
      # resultado acumulado ($MM)
      y = min(ymin)
    ) %>%
    ungroup()

  # cálculo del factor de escala
  ymax <- roundUpNice(pmax(1, max(df$ymax)))
  ymin <- min(df$ymin)
  ymax_sdad <- roundUpNice(
    pmax(0.1, max(df$TOTAL_SDAD)),
    nice = c(1,2,4,5,6,8,10,15,20,50,100)
  )
  y_escala_sdad <- (ymax - ymin) / ymax_sdad

  # waterfall
  p <- ggplot(df) +
    geom_rect(
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = tipo
      ),
      color = "black"
    )

  # línea de resultado acumulado

  # calculo el signo del resultado acumulado (positivo o negativo)
  signo_rdo_acum <- sign(
    coef(lm(y ~ PER, data = df %>% distinct(PER, y)))[["PER"]]
  )

  # eje x
  if(!is.null(PER_x)) {
    p <- p + coord_cartesian (xlim = c(PER_x[1] - 0.5, PER_x[length(PER_x)] + 0.5))
  }

  p <- p +
    geom_line(aes(PER, y), col = "dodgerblue4", size = 1) +
    geom_point(aes(PER, y), col = "dodgerblue4", size = 2.5) +
    geom_text(
      aes(PER, y, label = comprimir_num(y)),
      # position = position_stack(vjust = 0.5)
      # check_overlap = TRUE
      vjust = 1.2,
      # hjust = - signo_rdo_acum * 1.1
    )

  # formatos ----
  p <- p + theme_classic()
  # colores
  p <- p + scale_fill_manual(
    name = "Movimiento",
    # breaks pisa el orden anterior, podía tmb haber factorizado previamente la vble
    breaks = c(
      "PRIMA_ANUAL",
      "COMI_ANUAL", "ESPECIE", "ILT", "ILP", "JUICIOS"
    ),
    # nuevas etiquetas
    labels = c(
      "Prima",
      "comisiones", "especies", "ILP", "ILT", "juicios"
    ),
    # colores (no hace falta igualar)
    values = c(
      'PRIMA_ANUAL' = "green4",
      "COMI_ANUAL" = "olivedrab3",
      "ESPECIE" = "royalblue3",
      "ILT" = "orangered2",
      "ILP" = "navajowhite4",
      "JUICIOS" = "goldenrod4"
    )
  )

  # ejes


  # eje y
  p <- p + labs(y = "Millones de pesos", x = NULL)
  p <- p + scale_y_continuous(
    labels = function(l) paste0(round(l/1e6,1),"MM")
  )

  # leyenda
  p <- p + theme(
    legend.position = "bottom",
    legend.background = element_rect(
      fill = alpha("lightgrey", 0.4), color = "maroon", linetype = "solid"
    )
  )
  # líneas de fondo
  p <- p + theme(
    axis.line = element_line(color = "gray40", size = 0.5),
    panel.grid.major.y = element_line(
      size = 0.5, linetype = 'solid', colour = "gray80")
  )

  return(p)

}
