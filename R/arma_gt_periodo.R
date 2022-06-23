## funciones gt_periodo ----

arma_gt_periodo <- function(
  contratos,
  emision_per,
  montos_per,
  frecuencias_per,
  mes_rolling,
  periodos_n = 5L,
  credibilidad_IBNR = c(1, 1, 0.85, 0.65, 0.4)
){

  mesCierre <- max(emision_per[["MES"]])
  perCierre <- max(emision_per[["PER"]])
  per_x <- (perCierre - periodos_n + 1L):perCierre

  emision_per <- agrega_periodo_cobertura(emision_per)

  frecuencias_per <- frecuencias_per %>%
    select(
      -any_of(c("SALARIO", "PORINC", "PORINC_66", "EDAD"))
    ) %>%
    group_by(
      PER
    ) %>%
    summarise(
      across(
        everything(),
        list(~sum(.,na.rm = TRUE)),
        .names = "{col}"
      ),
      .groups = "drop"
    )

  frsa_per <- agrega_periodo_frecuencias(
    emision_per,
    frecuencias_per
  )

  # estandarizo nombres de columnas de monto y prima,
  # 1 - selecciono columnas que se utilizarán
  montos_per <- montos_per %>% select(
    all_of(c(
      "PER",
      "PRIMA_0_H", # Prima usada usada para calcular siniestralidad
      "COMI_r_H", # comisión usada para resultado (me gustaría se llame COMI_0_H)
      "ILP_LIQ", "ILP_RVA",  # para calcular VALOR_PORINC observado
      "TOT_LIQ", "TOT_RVA",  # valores contables corrientes
      # siniestralidades a moneda homogénea
      "TOT_LIQ_0_H", "TOT_RVA_H", "TOT_IBNER_H", "TOT_ULT_H",
      "TOT_LIQ_0_H_%", "TOT_RVA_H_%", "TOT_IBNER_H_%", "TOT_ULT_H_%",
      "ESP_ULT_H", "ILT_ULT_H", "ILP_ULT_H", "JUI_ULT_H",
      "ESP_ULT_H_%", "ILT_ULT_H_%", "ILP_ULT_H_%", "JUI_ULT_H_%",
      "RDO_H", "RDO_H_%"
    ))
  )
  # 2 - quito posible sufijo "_H"
  # names(montos_per) <- gsub("_H$", "", names(montos_per))

  tablero <- emision_per %>%
    left_join(
      frecuencias_per, by = "PER"
    ) %>%
    left_join(
      frsa_per, by = "PER"
    ) %>%
    left_join(
      montos_per,
      by = "PER"
    ) %>%
    arrange(
      PER
    ) %>%
    mutate(
      # valor de porcentaje de incapacidad observado
      VALOR_PORINC = if_else(
        PORINCs == 0, 0, (ILP_LIQ + ILP_RVA) / PORINCs
      ),
      PORINC_66 = if_else(
        GR_66 == 0L,
        0,
        PORINC_66s / GR_66
      ),
      PER_ETIQUETA = paste0(
        PER * 100L + if_else(mes_rolling == 12L, 1, mes_rolling + 1),
        "-",
        (PER + if_else(mes_rolling == 12L, 0L, 1L)) * 100
        + if_else(PER == perCierre, mesCierre %% 100L, mes_rolling)
      )
    ) %>%
    mutate(
      across(
        where(is.integer),
        ~ replace_na(., 0L)
      ),
      across(
        where(is.numeric),
        ~ replace_na(., 0)
      )
    )

  tablero <- tablero  %>%
    mutate(
      # incurrido a valor actual
      TOT_INC_0_H = TOT_LIQ_0_H + TOT_RVA_H
    ) %>%
    select(   ## hago un select para que queden las columnas ordenadas
      PER, PER_ETIQUETA, CONTRATOS,
      # TRABAJADORES,
      TRABAJADORESPROM, SALARIO,
      PRIMA_r, COMI_r, `CxT%`, `COMI%`,
      N, N_FINAL, GR, GR_FINAL, MU, MU_FINAL, JU, JU_FINAL,
      FRSA_N_FINAL, FRSA_GR_FINAL, FRSA_JU_FINAL,
      PORINC_66, VALOR_PORINC,
      TOT_LIQ, TOT_RVA,
      PRIMA_0_H, COMI_r_H, TOT_LIQ_0_H, TOT_INC_0_H, TOT_IBNER_H, TOT_ULT_H,
      ILT_ULT_H, ESP_ULT_H, ILP_ULT_H, JUI_ULT_H,
      `TOT_LIQ_0_H_%`, `TOT_RVA_H_%`, `TOT_IBNER_H_%`, `TOT_ULT_H_%`,
      `ILT_ULT_H_%`, `ESP_ULT_H_%`, `ILP_ULT_H_%`, `JUI_ULT_H_%`,
      RDO_H, `RDO_H_%`
    )


  if(periodos_n > 5L){
    credibilidad_IBNR <- c(rep(1, periodos_n - 5L), credibilidad_IBNR)
  } else {
    credibilidad_IBNR <- credibilidad_IBNR[(5L-periodos_n + 1L):5L]
  }

  tablero$IBNER_CONFIANZA <- credibilidad_IBNR[per_x %in% tablero$PER]

  return(tablero)

}
