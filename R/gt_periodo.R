#' gt_periodo: Arma cuadro de informe integral
#'
#' @param df  data.frame resultado de arma_gt_periodo
#' @param modo_moneda character  modo de moneda para personalizar anotaciones
#' @param siniestralidad_mala  numeric. Vector de siniestralidades, de la más
#'        reciente a la más antigua.
#' @param nota_al_pie_especial  character  nota que se quiera agregar
#'
#' @return
#' @export
#'
#' @examples
gt_periodo <- function(
  df,
  modo_moneda,
  siniestralidad_target = 1.10,
  nota_al_pie_especial = ""
){

## parámetros de diseño ----

# colores en https://htmlcolorcodes.com/es/nombres-de-los-colores/
frecuencias_color <- "RebeccaPurple" #"#663399"
fondo_cobertura_color <- "LightGreen"
fondo_frecuencias_color <- "LightSteelBlue"
fondo_contable_color <- "DarkKhaki"
fondo_siniestralidad_color <- "SandyBrown"
fondo_notas_color <- "Gainsboro"
lineas_color <- "Silver"
siniestralidad_mala_color <- "brown3" #"#DC143C"
siniestralidad_buena_color <- "chartreuse3" #"#DC143C"


foot_moneda <- case_when(
  modo_moneda == "historica" ~
    "Prima y Siniestros a moneda corriente",
  modo_moneda == "prima historica" ~
    "Prima histórica y Siniestros a moneda actual",
  modo_moneda == "prima actual" ~
    "Prima y Siniestros revaluadas a moneda actual",
  modo_moneda == "prima emitida" ~
    "Prima y Siniestros revaluadas en moneda al momento de emitirse",
  TRUE ~ ""
)

fill_credibilidad <- function(
  gtobj,
  columnas,
  credibilidad,
  color
){

  credibilidad <- enquo(credibilidad)  #enquo
  credibilidad <- gtobj$`_data` %>% pull(!!credibilidad) #unquo

  for(i in seq_along(credibilidad)){
    gtobj <- gtobj %>%
      tab_style(
        style = cell_fill(
          color = color,
          alpha = credibilidad[i]
        ),
        locations = cells_body(columns = all_of(columnas), rows = i)
      )
  }
  return(gtobj)
}

# función para definir límites de color
logistic_pattern <- function(...) {
  args <- list(...)
  x <- args[[1]]
  thresholds <- args[[2]]
  zeros <- numeric(length(x))

  #sigma for normalizacion.
  # maybe uniform sigma <- (pmax - pmin)/sqrt(12)
  sigma <- 0.2

  y <- 1/(1+exp(-(x - thresholds)/sigma))

  y

}

number <-function(number){
  result <- if_else(
    abs(number) < 1000000,
    format(
      number,
      nsmall = 0,
      #digits = 2 ,
      big.mark = ".",
      decimal.mark = ","
    ),
    paste0(
      format(
        number/1000000,
        digits = 3,
        drop0trailing = FALSE,
        big.mark = ".",
        decimal.mark = ","
      ),
      "MM"
    )
  )
  result <- paste0("$", result)
  return(result)
}

####----

# determino el número de periodos

n_periodos <- nrow(df)
siniestralidad_target <- siniestralidad_target[1:n_periodos]
siniestralidad_target <- rev(data.table::nafill(siniestralidad_target, type = "locf"))
# GT

df <- cbind(df, siniestralidad_target = siniestralidad_target)

gt_periodo <- gt(
  df,
  rowname_col = "PER_ETIQUETA"
) %>%
  # ocultar y mover columnas ----
  cols_hide(
    c(
      PER,
      CONTRATOS,
      IBNER_CONFIANZA,
      N,
      MU, MU_FINAL,
      GR,	PORINC_66,
      JU,	VALOR_PORINC,
      COMI_r,	COMI_r_H,
      TOT_LIQ_0_H,
      ILT_ULT_H, ESP_ULT_H, ILP_ULT_H, JUI_ULT_H
    )
  ) %>%
  # formatos de columna ----
  # números enteros
  fmt_number(
    columns = c(
      CONTRATOS,
      TRABAJADORESPROM,
      N_FINAL, GR_FINAL, JU_FINAL
    ),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  fmt_percent(
    columns = c(
      `CxT%`, `COMI%`, FRSA_N_FINAL, FRSA_GR_FINAL, FRSA_JU_FINAL,
      `TOT_ULT_H_%`, `RDO_H_%`
    ),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  fmt_percent(
    columns = c(IBNER_CONFIANZA),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  # salario
  fmt_currency(
    columns = c(SALARIO),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  fmt(
    columns = c(
      PRIMA_r,
      TOT_LIQ, TOT_RVA,
      PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H, RDO_H
    ),
    fns = number
  ) %>%
  # subtítulos de columnas ----
  tab_spanner(
    label = "Cobertura",
    columns = c(
      # CONTRATOS,
      TRABAJADORESPROM, SALARIO, PRIMA_r,
      `CxT%`, `COMI%`
    )
  ) %>%
  tab_spanner(
    label = "Frecuencias",
    columns = c(
      N_FINAL, FRSA_N_FINAL,
      GR_FINAL, FRSA_GR_FINAL,
      JU_FINAL, FRSA_JU_FINAL
    )
  ) %>%
  tab_footnote(  #nota a la frecuencia
    footnote = "Frecuencias proyectadas a valor definitivo.",
    locations = cells_column_spanners(
      spanners = "Frecuencias"
    )
  ) %>%
  tab_spanner(
    label = "Contable",
    columns = c(
      TOT_LIQ, TOT_RVA
    )
  ) %>%
  tab_spanner(
    label = "Siniestralidad",
    columns = c(
      PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H,
      IBNER_CONFIANZA,
      `TOT_ULT_H_%`, RDO_H, `RDO_H_%`
    )
  ) %>%
  tab_footnote(  #nota a la moneda de siniestralidad
    footnote = paste0(
      HTML(
        foot_moneda, " ", nota_al_pie_especial
      )
    ),
    locations = cells_column_spanners(
      spanners = "Siniestralidad"
    )
  ) %>%
  # combinaciones de columnas ----
  cols_merge(
    c(N_FINAL, FRSA_N_FINAL),
    hide_columns = c(FRSA_N_FINAL),
    pattern = "{1}<br><span class = 'frecuencias'>{2}</span>"
  ) %>%
  cols_merge(
    c(GR_FINAL, FRSA_GR_FINAL),
    hide_columns = c(FRSA_GR_FINAL),
    pattern = "{1}<br><span class = 'frecuencias'>{2}</span>"
  ) %>%
  cols_merge(
    c(JU_FINAL, FRSA_JU_FINAL),
    hide_columns = c(FRSA_JU_FINAL),
    pattern = "{1}<br><span class = 'frecuencias'>{2}</span>"
  ) %>%
  cols_merge(
    c(TOT_ULT_H, `TOT_ULT_H_%`),
    hide_columns = c(`TOT_ULT_H_%`),
    pattern = "{1}<br><em><b><span style='font-size:105%'>{2}</span></b></em>"
  ) %>%
  cols_merge(
    c(RDO_H, `RDO_H_%`),
    hide_columns = c(`RDO_H_%`),
    pattern = "{1}<br><em><b><span style='font-size:105%'>{2}</span></b></em>"
  ) %>%

  # estilo de columnas, encabezados y demás elementos ----
  ## a. No se puede aplicar formato directo a las columnas fundidas con otras,
  ##    hay que usar text_transform
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_stub()
  ) %>%
  # color recuadro libre arriba
  tab_style(
    style = cell_fill(color = fondo_notas_color),
    locations = cells_column_labels(PER_ETIQUETA)
  ) %>%
  # color cobertura
  tab_style(
    style = list(
      cell_fill(color = fondo_cobertura_color),
      cell_borders(
        sides = c("left", "right"),
        color = lineas_color, style = "dotted", weight = px(1)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = "Cobertura"
      ),
      cells_body(
        c(TRABAJADORESPROM, SALARIO, PRIMA_r, `CxT%`, `COMI%`)
      )
    )
  ) %>%
  # color frecuencias
  tab_style(
    style = list(
      cell_fill(color = fondo_frecuencias_color),
      cell_borders(
        sides = c("left", "right"),
        color = lineas_color, style = "dotted", weight = px(1)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = "Frecuencias"
      ),
      cells_body(
        c(N_FINAL, GR_FINAL, JU_FINAL)
      )
    )
  ) %>%
  # color contable
  tab_style(
    style = list(
      cell_fill(color = fondo_contable_color),
      cell_borders(
        sides = c("left", "right"),
        color = lineas_color, style = "dotted", weight = px(1)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = "Contable"
      ),
      cells_body(
        c(TOT_LIQ, TOT_RVA)
      )
    )
  ) %>%
  # color siniestralidad
  # data_color(
  #   columns = c(PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H),
  #   colors = pal(`TOT_ULT_H_%`, siniestralidad_mala),
  #   apply_to = "fill",
  #   autocolor_text = FALSE
  # ) %>%
  tab_style(
    style = cell_fill(color = "gray85"),
    locations = cells_body(
      columns = c(PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H),
      rows = between(logistic_pattern(`TOT_ULT_H_%`, siniestralidad_target), 0.45, 0.55)
    )
  ) %>%
  tab_style(
    style = cell_fill(
      # color = scales::col_bin(.$threshold, domain = colfunc(10))
      color = siniestralidad_buena_color
    ),
    locations = cells_body(
      columns = c(PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H),
      rows = logistic_pattern(`TOT_ULT_H_%`, siniestralidad_target) < 0.45
    )
  ) %>%
  tab_style(
    style = cell_fill(
      color = siniestralidad_mala_color
    ),
    locations = cells_body(
      columns = c(PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H),
      rows = logistic_pattern(`TOT_ULT_H_%`, siniestralidad_target) > 0.55
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = fondo_siniestralidad_color),
      cell_borders(
        sides = c("left", "right"),
        color = lineas_color, style = "dotted", weight = px(1)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = "Siniestralidad"
      ),
      cells_body(
        # c(PRIMA_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H, RDO_H)
        RDO_H
      )
    )
  ) %>%
  # fill_credibilidad (
  #   columnas = c("TOT_IBNER_H" ,"TOT_ULT_H", "RDO_H", "TOT_ULT_H"),
  #   credibilidad = IBNER_CONFIANZA,
  #   color = fondo_siniestralidad_color
  # ) %>%
  # separadores de columnas principales----
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = lineas_color, style = "solid", weight = px(3)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = everything()
      ),
      cells_body(
        c(TRABAJADORESPROM, N_FINAL, TOT_LIQ, PRIMA_0_H)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = lineas_color, style = "solid", weight = px(3)
      )
    ),
    locations = list(
      cells_column_spanners(
        spanners = everything()
      ),
      cells_body(
        c(`COMI%`, JU_FINAL, TOT_RVA, RDO_H)
      )
    )
  ) %>%
  # alineación de columnas ----
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # oculto las siguientes columnas
  cols_hide(
    columns = c(
      `TOT_LIQ_0_H_%`,	`TOT_RVA_H_%`,	`TOT_IBNER_H_%`,	`ILT_ULT_H_%`,
      `ESP_ULT_H_%`,	`ILP_ULT_H_%`,	`JUI_ULT_H_%`,
      siniestralidad_target
    )
  ) %>%
  # nombre de las columnas visibles ----
  cols_label(
    TRABAJADORESPROM = HTML("Promedio<br>cápitas"),
    SALARIO = "Salario",
    PRIMA_r = HTML("Prima<br>anual"),
    `COMI%` = "Com%",
    `CxT%` = "CxT%",
    N_FINAL = "Siniestros",
    GR_FINAL = "Graves",
    JU_FINAL = "Juicios",
    TOT_LIQ = "Liquidado",
    TOT_RVA = "Reservas",
    PRIMA_0_H = HTML("Prima<br>Sdad"),
    TOT_INC_0_H = HTML("Incurrido<br>Sdad"),
    TOT_IBNER_H = "IBNER",
    TOT_ULT_H = HTML("Total<br>Sdad"),
    RDO_H = HTML("Rdo<br>Técnico")
  ) %>%
  tab_options(
    # opciones para la tabla
    footnotes.marks = "*",
    footnotes.sep = " ",
    footnotes.background.color = fondo_notas_color
  )

  return(gt_periodo)

}
