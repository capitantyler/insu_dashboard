#' Gráfico "Siniestralidad reservas e IBNER"
#'
#' Se acopla sineistralidad por estadío y tasa de sdsd en un mismo gráfico
#' mediante el método de equivalencia de ejes.
#'
#' PER               numeric  periodo
#' PRIMA_ANUAL       numeric  prima anual
#' TOT_LIQ           numeric  Total liquidado por año accidente
#' TOT_RVA           numeric  Total reservas por año accidente
#' TOT_IBNER         numeric  Total ultimate por año accidente
#'
#' @return
#' @export
#'
#' @examples
#' rpt <- arma_gt_periodo(
#' contratos = grupo$contratos,
#' emision = grupo$emision$rpt2,
#' siniestros = grupo$siniestralidad$rpt1,
#' frecuencias = grupo$siniestralidad$rpt6,
#' montos = grupo$siniestralidad$rpt2,
#' mesMax = mesMax
#' )
#'
#' plot <- plot_sdad_ibner(
#'   rpt$PER,
#'   rpt$PRIMA_r,
#'   rpt$TOT_LIQ,
#'   rpt$TOT_RVA,
#'   rpt$TOT_IBNER,
#'   PER_x = periodos
#' )
#' grid.newpage()
#' grid.draw(plot_sdad_ibner(df))

plot_sdad_ibner <- function(
  PER,
  PRIMA_ANUAL,
  TOT_LIQ,
  TOT_RVA,
  TOT_IBNER,
  PER_x = NULL
){

  # datos
  df <- data.frame(
    PER,
    PRIMA_ANUAL,
    TOT_LIQ,
    TOT_RVA,
    TOT_IBNER
  )
  names(df) <- c(
    "PER",
    "PRIMA_ANUAL",
    "TOT_LIQ",
    "TOT_RVA",
    "TOT_IBNER"
  )

  df <- df %>%
    mutate(
      TOT_ULT = TOT_LIQ + TOT_RVA + TOT_IBNER,
      across(
        c(TOT_LIQ, TOT_RVA, TOT_IBNER, TOT_ULT),
        .fns = ~if_else(PRIMA_ANUAL == 0, 0, ./PRIMA_ANUAL)
      )
    )

    # cálculo del factor de escala
    ymax_ult <- 1
    ymin_ult <- min(df$TOT_IBNER/df$TOT_ULT)
    ymax_sdad <- roundUpNice(
      pmax(0.1, max(df$TOT_ULT)),
      nice = c(1,2,4,5,6,8,10,15,20,50,100)
    )
    y_escala_sdad <- ymax_ult / ymax_sdad

    df <- df %>%
    select(
      PER, TOT_LIQ, TOT_RVA, TOT_IBNER, TOT_ULT
    ) %>%
    pivot_longer(
      - c(PER, TOT_ULT),
      names_to = "estado", values_to = "sdad"
    ) %>%
    # cálulo de la posición de etiquetas
    group_by(
      PER
    ) %>%
    mutate(
      sdad_prop = sdad / sum(abs(sdad)),
      estado = factor(
        estado,
        levels = c("TOT_IBNER", "TOT_RVA", "TOT_LIQ"),
        labels = c("ibner", "rva", "liq"),
        ordered = TRUE
      ),
      label_ypos = cumsum(sdad_prop),
      label_ypos = label_ypos/2 + if_else(row_number() != 1, lag(label_ypos)/2, 0),
      # reacomodo IBNER si es negativo
      label_ypos = if_else(
        sdad_prop < 0,
        sdad_prop / 2,
        label_ypos
      ),
      TOT_ULT_label = paste0(round(TOT_ULT * 100, 1), "%")
      #w = strwidth(TOT_ULT_label, 'inches') * 1.05,
      #h = strheight(TOT_ULT_label, 'inches') * 1.05
    ) %>%
    ungroup()

  # eje x de tiempo
  p <- ggplot(df, aes(x = PER))

  # siniestralidad por tipo
  p <- p + geom_bar(
    aes(y = sdad_prop, fill = estado, group = estado),
    stat = "identity",
    colour = "gray33",
    width = 0.80
  )
  # siniestralidad ULT (transformada)
  p <- p + geom_path(
    aes(y = TOT_ULT * y_escala_sdad, group = "sdad ULT [%]"),
    linejoin = "mitre",
    stat = "identity",
    size = 1,
    colour = "firebrick1"
  ) +
    geom_point(
      aes(y = TOT_ULT * y_escala_sdad, group = "sdad ULT [%]"),
      shape = "diamond filled", fill = "orchid",
      color = "firebrick3", size = 2
    ) +
    geom_label(
      aes(
        y = TOT_ULT * y_escala_sdad,
        label = TOT_ULT_label
      ),
      # direction = "y", # no se puede, genera triplicado
      position =  position_nudge(
        # y = -0.1
        y = ifelse(df$TOT_ULT > 0.2, - 0.1, 0.1)
      ),
      fill = "tomato",
      size = 4
    )

  # siniestralidad por tipo (texto)
  # anulado, demasiados números
  # p <- p + geom_text_repel(
  #     aes(
  #       y = label_ypos,
  #       label = paste0(round(sdad * 100, 1), "%")
  #     ),
  #     direction = "y",
  #     point.padding = 0.5,
  #     color = "black",
  #     size = 3,
  #     fontface = "italic"
  #   )

  # eje x
  if(!is.null(PER_x)) {
    p <- p + coord_cartesian(
      xlim = c(PER_x[1] - 0.5, PER_x[length(PER_x)] + 0.5)
    )
  }

  # ejes y

  p <- p + scale_y_continuous(
    limits = c(ymin_ult, ymax_ult),
    breaks = NULL,
    # eje secundario: se revierte la transformación anterior
    sec.axis = sec_axis(
      trans = ~./y_escala_sdad,
      name = "sdad ULT [%]",
      labels = function(x) paste0(x * 100, "%")
    )
  )

  # formatos ----
  p <- p + theme_classic()
  # colores
  p <- p + scale_fill_manual(
    # breaks pisa el orden anterior, podía tmb haber factorizado previamente la vble
    breaks = c(
      "ibner", "rva", "liq"
    ),
    # nuevas etiquetas
    labels = c(
      "ibner", "reservas", "liquidado"
    ),
    # colores (no hace falta igualar)
    values = c(
      "liq" = "darkslateblue",
      "rva" = "lightskyblue",
      "ibner" = "lightsteelblue2"
    )
  )

  # ejes
  p <- p + labs(y = NULL, x = NULL)

  p <- p + theme(
    legend.position = "bottom",
    # legend.position = pos_leyenda,
    legend.background = element_rect(
      fill = alpha("steelblue", 0.4), color = "steelblue", linetype = "solid"
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
