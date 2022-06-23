filtra_datos_tablero <- function(
  grupo_tipo,
  grupo_id,
  mes_min,
  mes_max,
  sin_covid19
){

  if(
    grupo_tipo != "Asociart"
  ){
    filtroContratos <- filtrar_nros_contrato(
      grupo_tipo, grupo_id
    )
  } else {
    filtroContratos <- unique(
      contratos.data[
        TIPO != "D" ,
        .(CONTRATO, GRUPO_EC)
      ][
        emiorig.data[MES >= mes_min & MES <= mes_max,
           .(
             PRIMA = sum(PRIMA),
             TRABAJADORES = sum(TRABAJADORES[MES == mes_max])
           ),
           by = CONTRATO
        ],
        on = .(CONTRATO),
        nomatch = NULL
      ][
        PRIMA > 0 &
          TRABAJADORES < 50 & GRUPO_EC == "SD"
      ][["CONTRATO"]]
    )
  }

  data <- filtrar_tablas(
    filtroContratos
  )

  # filtros por fecha
  data$emiorig <- data$emiorig[
    MES >= mes_min & MES <= mes_max
  ]

  if(
    length(filtroContratos) == 0 ||
    nrow(data$emiorig) == 0
  ){

    sendSweetAlert(
      #session = session,
      title   = HTML("Ups..."),
      text    = HTML("No hay datos en el periodo para los contratos seleccionado..."),
      type    = "warning",
      html    = TRUE
    )
    req(FALSE)
  }

  data$liquidaciones <- data$liquidaciones[
    MESLIQ <= mes_max
  ]

  data$reservas <- data$reservas[
    MES <= mes_max
  ]

  data$siniestros <- data$siniestros[
    MESACC <= mes_max,
    .(
      CONTRATO, DENUNCIA, SINIESTRO, CUIL, SEXO, TRABAJADOR, EDAD, F_INGRESO,
      MESACC, F_ACC, F_BAJA, F_ALTA, ESTADO_CONSECUENCIA,
      ESTADO_STRO, ESTADO_SINIESTRO,
      IBM, SALARIO, PORINC, CIE10_DESCR, DIAS, JUI, ORIGEN, TERMINACION,
      FLAG_COVID
    )
  ]

  setkey(data$siniestros, DENUNCIA)

  # filtro de covid
  if(sin_covid19 == TRUE) {
    data$siniestros <- data$siniestros[
      FLAG_COVID == FALSE
    ]
    data$juicios <- data$juicios[
      data$siniestros, .SD,
      on = .(DENUNCIA),
      nomatch = 0
    ]
    data$liquidaciones <- data$liquidaciones[
      data$siniestros, .SD, nomatch = 0
    ]
    data$reservas <- data$reservas[
      data$siniestros, .SD, nomatch = 0
    ]
  }

  return(data)

}
