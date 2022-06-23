adjunta_ultimates_apriori_siniestros <- function(
  metodo_IBNER,
  siniestros,
  emision,
  ultimates,
  mes_max,
  patron_fda,
  metodos_BF = c("CL", "BF", "BF_modificado")
){
  ## preparación de datos para IBNER ----

  if(metodo_IBNER %in% metodos_BF){

    # 1 - calculo el ibnr puro en nro y pesos ----

    # las frecuencias y primas puras que se pasan son MENSUALES.
    ibnr_puro <- ultimates[,.(
      MES_INI, MES_FIN,
      N_FREC_ULT = N_FREC_ULT/12,
      ILT_IBNR_PP, ESP_IBNR_PP, ILP_IBNR_PP, JUI_IBNR_PP
    )
    ][
      # unequal join para obtener ultimates entre fechas
      # duplico col MES para que no desaparezca con el join
      emision[, MES_EMI := MES],
      on = .(MES_INI <= MES_EMI, MES_FIN >= MES_EMI)
    ]

    # selección
    ibnr_puro[,.(
      CONTRATO, MES, TRABAJADORES_r,
      N_FREC_ULT, ILT_IBNR_PP, ESP_IBNR_PP, ILP_IBNR_PP, JUI_IBNR_PP
    )]
    # delay
    ibnr_puro[,
       DELAY := pmax(
         (mes_max %/% 100- MES %/% 100)  * 12L +
           (mes_max %% 100- MES %% 100) + 1L,
         1L
       )
    ]

    # frecuencia IBNR de casos
    ibnr_puro[,
       N_FREC_IBNR := fifelse(
         DELAY > length(patron_fda),
         0L,
         N_FREC_ULT * (1- 1/patron_fda[DELAY])
       )
    ]
    # IBNR puro y absoluto de casos y prima pura,
    # agrupado por CONTRATO y MES
    ibnr_puro[,
       c('N_IBNR', 'ILT_IBNR', 'ESP_IBNR', 'ILP_IBNR', 'JUI_IBNR') := lapply(
         .SD, "*", TRABAJADORES_r
       ),
       .SDcols = c('N_FREC_IBNR', 'ILT_IBNR_PP', 'ESP_IBNR_PP', 'ILP_IBNR_PP', 'JUI_IBNR_PP')
    ][,
      lapply(.SD, sum), by = list(CONTRATO, MES),
      .SDcols = c('N_IBNR', 'ILT_IBNR', 'ESP_IBNR', 'ILP_IBNR', 'JUI_IBNR')
    ]

    ## 2 - ultimates a priori para BF Modificado ----

    siniestros[,
      # unequal join para obtener ultimates entre fechas
      # duplico col MESACC para que no desaparezca con el join
      c("MES_ACC_INI", "MES_ACC_FIN") := list(MESACC, MESACC)
    ]

    if(metodo_IBNER %in% c("BF", "BF_modificado")){

      if(metodo_IBNER == "BF"){
        # agrego los costos medios ultimate por siniestro
        siniestros <- siniestros[ultimates[,
            .(
              MES_INI, MES_FIN,
              ILT_ULT_CM0, ESP_ULT_CM0, ILP_ULT_CM0, JUI_ULT_CM0
            )
          ],
          nomatch = NULL,
          on = .(MES_ACC_INI >= MES_INI, MES_ACC_FIN <= MES_FIN)
        ]
      }

      if(metodo_IBNER == "BF_modificado"){

        # dato por contrato
        siniestros <- siniestros[
          emision[,.(CONTRATO, MES, SALPROM)],
          SALARIO_EMI := i.SALPROM,
          on = c("CONTRATO" = "CONTRATO", "MESACC" = "MES")
        ]

        # agrego los costos medios ultimate por siniestro
        siniestros <- siniestros[ultimates[,
          .(
            MES_INI, MES_FIN, SALARIO_EMI_0 = SALARIO_EMI,
            ILT_ULT_CM0, ESP_ULT_CM0, ILP_ULT_CM0, JUI_ULT_CM0
          )
        ],
        nomatch = NULL,
        on = .(MES_ACC_INI >= MES_INI, MES_ACC_FIN <= MES_FIN)
        ]
      }

      siniestros[,
        c("MES_ACC_INI", "MES_ACC_FIN") := NULL
      ]

      setnames(
        siniestros,
        c('ILT_ULT_CM0', 'ESP_ULT_CM0', 'ILP_ULT_CM0', 'JUI_ULT_CM0'),
        c('ILT_ULT_H', 'ESP_ULT_H', 'ILP_ULT_H', 'JUI_ULT_H')
      )

      # agrego CM para juicios pre 2017
      siniestros[,
        JUI_ULT_2017_H := JUI_ULT_H
      ]

      setkey(siniestros, DENUNCIA)

    }

  }

  return(siniestros)

}

