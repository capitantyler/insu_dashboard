reportes_siniestralidad <- function(
  data,
  emision,
  contratos_x_cluster,
  args_siniestralidad
){

  data$siniestros <- adjunta_ultimates_apriori_siniestros(
    metodo_IBNER = args_siniestralidad$metodo_IBNER,
    siniestros = data$siniestros,
    emision = emision$rpt1, #data$emiorig,
    ultimates = ultimates_GSC.data,
    mes_max = args_siniestralidad$mes_corte_datos, # mes_corte_datos(),
    patron_fda = patron_fda
  )


  if(args_siniestralidad$metodo_IBNER == "BF_modificado"){

    cols_rpt1_sdad <- c(
      "CONTRATO", "CLIENTE", "INIVIG",  "MESACC",
      "SALARIO_EMI", "SALARIO_EMI_0",
      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
      "ILT_LIQ_0", "ESP_LIQ_0", "ILP_LIQ_0", "JUI_LIQ_0", "TOT_LIQ_0",
      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
      "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
    )

    cols_agrupar_rpt2_sdad <- c(
      "CONTRATO", "CLIENTE", "INIVIG",
      "SALARIO_EMI", "SALARIO_EMI_0",
      "MESACC"
    )

  } else {

    cols_rpt1_sdad <- c(
      "CONTRATO", "CLIENTE", "INIVIG",  "MESACC",
      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
      "ILT_LIQ_0", "ESP_LIQ_0", "ILP_LIQ_0", "JUI_LIQ_0", "TOT_LIQ_0",
      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
      "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
    )

    cols_agrupar_rpt2_sdad <- c(
      "CONTRATO", "CLIENTE", "INIVIG",
      "MESACC"
    )
  }

  cols_rpt6_sdad <- c(
  "PER",
  "N", "N_ULT", "GR", "GR_66", "MU", "JU", "JN",
  "PORINCs", "PORINC_66s"
  )

  siniestralidad <- calcula_reporte_siniestros(
    reportes = c("rpt1", "rpt6"),
    denuncias_x_cluster = contratos_x_cluster,
    data = data,
    columnas_reportes = list(
      rpt1 = cols_rpt1_sdad,
      rpt6 = cols_rpt6_sdad
    ),
    args_siniestralidad
  )

  siniestralidad[["rpt2"]] <- siniestralidad[["rpt1"]][,
     lapply(.SD, sum),
     keyby = cols_agrupar_rpt2_sdad,
     .SDcols = c(
       "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
       "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
       "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
       "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
       "ILT_LIQ_0", "ESP_LIQ_0", "ILP_LIQ_0", "JUI_LIQ_0", "TOT_LIQ_0",
       "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
     )
  ]

  return(siniestralidad)

}
