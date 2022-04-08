#' calcula_siniestralidad
#'
#' Devuelve un data.frame/data.table con los campos de emisión y siniestralidad
#' en la moneda especificada por el parámetro modo_moneda.
#'
#' Se agrupan los resultados con las columnas columnas_agrupar. Si no se
#' especifica, se agrupa por las columnas de reporte_emision_mes.
#'
#' Se detallan las columnas de ambos data.frames/tables y se agregan
#'  - las expresadas en otro moneda con el sufijo sufijo
#'  - los índices de siniestralidad sobre columna_prima.
#'
#' @param reporte_emision_mes   data.frame  emision por MES y CONTRATO
#' @param reporte_siniestralidad_mes  data.frame  siniestralidad pesos
#'                                                por MESACC y CONTRATO
#' @param modo_moneda           character   modo de moneda
#'    -- "NO"               sin cambios
#'    -- "prima historica"  siniestros a moneda actual y prima a moneda emitida
#'    -- "prima emitida"    siniestros y prima a moneda emitida
#'    -- "prima actual"     siniestros y prima a moneda mes_datos.
#' @param indice
#' @param mes_datos
#' @param columnas_siniestralidad
#' @param sufijo     character  sufijo de las columnas transformadas
#' @param columna_prima    character  nombre de la columna prima (default: "PRIMA_r")
#' @param columna_comision character  nombre de la columna comisión (default: "COMI_r")
#' @param columnas_agrupar   character  vector de nombres de columnas a grupar
#'
#' @return
#' @export
#'
#' @examples
#' @note
#' Se asume siniestros pasados a moneda actual (mes_datos) y prima a moneda
#' histórica...
#'
calcula_siniestralidad <- function(
  reporte_emision_mes,
  reporte_siniestralidad_mes,
  modo_moneda = "NO",
  indice,
  mes_datos,
  columna_prima = "PRIMA_r",
  columna_comision = "COMI_r",
  columnas_siniestralidad = NULL,
  columnas_agrupar = NULL,
  sufijo = ""
){
  UseMethod("calcula_siniestralidad")
}

calcula_siniestralidad.data.table <- function(
  reporte_emision_mes,
  reporte_siniestralidad_mes,
  modo_moneda = "NO",
  indice,
  mes_datos,
  columna_prima = "PRIMA_r",
  columna_comision = "COMI_r",
  columnas_siniestralidad = NULL,
  columnas_agrupar = NULL,
  sufijo = ""
){

  # columna_prima <- enquo(columna_prima)
  # columnas_siniestralidad <- enquo(columnas_siniestralidad)
  # columnas_agrupar <- enquo(columnas_agrupar)

  # si no se pasan las columna spar aagrupar, se agrupa según reporte emisión
  if(is.null(columnas_agrupar)){
    columnas_agrupar <- quos(
      names(reporte_emision_mes)
    )
  }

  modo_moneda <- arg_match0(
    modo_moneda,
    c("historica", "prima historica", "prima actual", "prima emitida")
  )

  # 1 - nombres de columna ----

  columnas_prima <- c(
    "MASA", "MASA_r", "MASA_desest",
    "PRIMA", "PRIMA_r", "PREMIO", "PREMIO_r",
    "PRIMADOM", "PRIMADOM_r", "PREMIODOM", "PREMIODOM_r",
    "PRIMAsinDOM", "PRIMAsinDOM_r", "PREMIOsinDOM", "PREMIOsinDOM_r",
    "COMI", "COMI_r"
  )

  if(is.null(columnas_siniestralidad)){
    columnas_siniestralidad <- names(reporte_siniestralidad_mes)[
      grepl(
        "(ILT|ESP|ILP|JUI|TOT)_(LIQ|RVA|INC|IBNER|ULT)((_0)?)$",
        names(reporte_siniestralidad_mes)
      )
    ]
  }

  columna_prima_sym <- sym(columna_prima)
  columna_prima0 <- str_c("PRIMA_0", sufijo)
  # columna_prima0_sym <- sym(columna_prima0)
  columna_comision0 <- str_c(columna_comision, sufijo)
  columna_TOT_ULT <- str_c("TOT_ULT", sufijo)

  # filtro cuáles columnas existen

  columnas_prima <- columnas_prima[
    columnas_prima %in% names(reporte_emision_mes)
  ]
  columnas_siniestralidad <- columnas_siniestralidad[
    columnas_siniestralidad %in% names(reporte_siniestralidad_mes)
  ]
  columnas_prima_H <- paste0(columnas_prima, sufijo)

  # evito columnas de siniestralidad no homogéneas
  columnas_siniestralidad_0 <- grep(
    "_(LIQ_0|INC_0|RVA|IBNER|ULT)$",
    columnas_siniestralidad,
    value = T
  )
  columnas_siniestralidad_H <- paste0(columnas_siniestralidad_0, sufijo)

  ## 2. conjunción emisión con siniestralidad ----

  # elimino nombres comunes de la tabla siniestralidad
  varList <- names(reporte_siniestralidad_mes)[
    !(names(reporte_siniestralidad_mes) %in% names(reporte_emision_mes))
  ]
  varList <- unique(c("CONTRATO", varList))

  planilla <- reporte_siniestralidad_mes[,
    ..varList
  ][reporte_emision_mes,
    on = .(CONTRATO, MESACC = MES),
    allow.cartesian = TRUE
  ]
  setnames(planilla, "MESACC", "MES")

  ## 3. moneda homogénea ----

  # agrego indice
  if(modo_moneda %in% c("prima emitida", "prima actual")){
    
    indice <- indice[, .(MES, INDICE)]
    setkey(indice, MES)
    indice_base <- indice[MES == mes_datos][["INDICE"]]
    
    planilla <- merge(
      planilla,
      indice[, .(MES, INDICE)],
      all = FALSE,
      by = "MES",
      allow.cartesian = TRUE
    )
  }
  
  # duplico las columnas necesarias

  planilla[,
    (columna_prima0) := get(columna_prima)
  ]

  planilla[,
    paste0(c(columnas_prima, columnas_siniestralidad_0), sufijo) :=
    setDT(mget(c(columnas_prima, columnas_siniestralidad_0)))
  ]

  if(nrow(planilla) != 0) {

    if(modo_moneda == "prima emitida"){

      planilla[,
       paste0(columnas_siniestralidad_0, sufijo) :=
       .SD * (INDICE / ..indice_base),
       .SDcols = columnas_siniestralidad_0
      ][,
        INDICE := NULL
      ]

    }

    if(modo_moneda == "prima actual"){

      planilla[,
        paste0(columnas_prima, sufijo) :=
        .SD * (..indice_base / INDICE ),
        .SDcols = columnas_prima
      ][,
        INDICE := NULL
      ]

      planilla[,
        (columna_prima0) := get(paste0(columna_prima, sufijo))
      ]

    }

  }

  ## 4. agrupo y sumo----

  columnas_sumar <- unique(c(
    columnas_prima, columnas_prima_H, columna_prima0,
    columnas_siniestralidad, columnas_siniestralidad_H
  ))

  planilla <- planilla[,
    lapply(.SD, sum, na.rm = TRUE),
    by = columnas_agrupar,
   .SDcols = columnas_sumar
  ]

  ## 5. calculo los índices ----

  planilla[,
    paste0(columnas_siniestralidad_H, "_%") := lapply(
      .SD,
      function(col) fifelse(
        get(columna_prima0) == 0,
        0,
        col / get(columna_prima0)
      )
    ),
    .SDcols = columnas_siniestralidad_H
  ]

  # resultado período $
  planilla[,
   paste0(paste0("RDO", sufijo)) := get(columna_prima0) -
     get(columna_comision0) - get(columna_TOT_ULT)
  ]

  # resultado período %
  planilla[,
    paste0(paste0("RDO", sufijo, "_%")) := fifelse(
      get(columna_prima0) == 0,
      0,
      get(paste0("RDO", sufijo)) / get(columna_prima0)
    )
  ]

  return(planilla)

}

calcula_siniestralidad.data.frame <- function(
  reporte_emision_mes,
  reporte_siniestralidad_mes,
  modo_moneda = "NO",
  indice,
  mes_datos,
  columna_prima = "PRIMA_r",
  columna_comision = "COMI_r",
  columnas_siniestralidad = NULL,
  columnas_agrupar = NULL,
  sufijo = ""
){

# columna_prima <- enquo(columna_prima)
# columnas_siniestralidad <- enquo(columnas_siniestralidad)
# columnas_agrupar <- enquo(columnas_agrupar)

# si no se pasan las columna spar aagrupar, se agrupa según reporte emisión
if(is.null(columnas_agrupar)){
  columnas_agrupar <- quos(
    names(reporte_emision_mes)
  )
}

modo_moneda <- arg_match0(
  modo_moneda,
  c("historica", "prima historica", "prima actual", "prima emitida")
)

# 1 - nombres de columna ----

columnas_prima <- c(
  "MASA", "MASA_r", "MASA_desest",
  "PRIMA", "PRIMA_r", "PREMIO", "PREMIO_r",
  "PRIMADOM", "PRIMADOM_r", "PREMIODOM", "PREMIODOM_r",
  "PRIMAsinDOM", "PRIMAsinDOM_r", "PREMIOsinDOM", "PREMIOsinDOM_r",
  "COMI", "COMI_r"
)

if(is.null(columnas_siniestralidad)){
  columnas_siniestralidad <- names(reporte_siniestralidad_mes)[
    grepl(
      "(ILT|ESP|ILP|JUI|TOT)_(LIQ|RVA|INC|IBNER|ULT)((_0)?)$",
      names(reporte_siniestralidad_mes)
    )
  ]
}

columna_prima_sym <- sym(columna_prima)
columna_prima0 <- str_c("PRIMA_0", sufijo)
columna_prima0_sym <- sym(columna_prima0)
columna_comision0 <- str_c(columna_comision, sufijo)
columna_comision_sym <- sym(columna_comision0)
columna_TOT_ULT <- str_c("TOT_ULT", sufijo)
columna_TOT_ULT_sym <- sym(columna_TOT_ULT)

# filtro cuáles columnas existen

columnas_prima <- columnas_prima[
  columnas_prima %in% names(reporte_emision_mes)
]
columnas_siniestralidad <- columnas_siniestralidad[
  columnas_siniestralidad %in% names(reporte_siniestralidad_mes)
]
columnas_prima_H <- paste0(columnas_prima, sufijo)

# evito columnas de siniestralidad no homogéneas
columnas_siniestralidad_0 <- grep(
  "_(LIQ_0|INC_0|RVA|IBNER|ULT)$",
  columnas_siniestralidad,
  value = T
)
columnas_siniestralidad_H <- paste0(columnas_siniestralidad_0, sufijo)



## 2. conjunción emisión con siniestralidad ----

# elimino nombres comunes de la tabla siniestralidad
varList <- names(reporte_siniestralidad_mes)[
  !(names(reporte_siniestralidad_mes) %in% names(reporte_emision_mes))
]
varList <- unique(c("CONTRATO", varList))

planilla <- left_join(
  reporte_emision_mes,
  reporte_siniestralidad_mes %>% select(all_of(varList)),
  by = c("CONTRATO", "MES" = "MESACC")
)

## 3. moneda homogénea ----

# duplico las columnas necesarias

planilla <- planilla %>%
  mutate(
    !! columna_prima0 := {{ columna_prima_sym }},
    across(
      all_of(c(columnas_prima, columnas_siniestralidad_0)),
      ~ .,
      .names = paste0("{col}", sufijo)
    )
  )

if(nrow(planilla) != 0) {

  if(modo_moneda == "prima emitida"){

    indice_base <- indice[indice$MES == mes_datos,][["INDICE"]]

    planilla <- planilla %>%
      inner_join(
        indice %>% select(MES, INDICE),
        by = "MES"
      ) %>%
      mutate(
        across(
          all_of(columnas_siniestralidad_0),
          ~ . * (INDICE / !!indice_base),
          .names = paste0("{col}", sufijo)
        ),
        INDICE = NULL
      )

  }

  if(modo_moneda == "prima actual"){

    indice_base <- indice[indice$MES == mes_datos,][["INDICE"]]

    planilla <- planilla %>%
      inner_join(
        indice %>% select(MES, INDICE),
        by = "MES"
      ) %>%
      mutate(
        across(
          all_of(columnas_prima),
          ~ . * (!!indice_base / INDICE),
          .names = paste0("{col}", sufijo)
        ),
        INDICE = NULL
      ) %>%
      mutate(
        !! columna_prima0 := !! sym(paste0(columna_prima, sufijo))
      )

  }

}

## 4. agrupo y sumo----

planilla <- planilla %>%
  group_by(
    across(
      all_of(columnas_agrupar)
    )
  ) %>%
  summarise(
    across(
      all_of(
        unique(
          c(
            columnas_prima,
            columnas_prima_H,
            columna_prima0,
            columnas_siniestralidad,
            columnas_siniestralidad_H
          )
        )
      ),
      ~sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

## 5. calculo los índices ----

planilla <- planilla %>%
  mutate(
    across(
      all_of(columnas_siniestralidad_H),
      ~if_else(
        {{ columna_prima0_sym }} == 0,
        0,
        . / {{ columna_prima0_sym }}
      ),
      .names = "{col}_%"
    )
  )  %>%
  mutate(
    # resultado período
    !! paste0("RDO", sufijo) :=
      {{ columna_prima0_sym }} -
      {{ columna_comision_sym }} -
      {{ columna_TOT_ULT_sym}}
  ) %>%
  mutate(
    # resultado período %
    !! paste0("RDO", sufijo, "_%") := if_else(
      {{ columna_prima0_sym }}  == 0,
      0,
      !! sym(paste0("RDO", sufijo)) / {{ columna_prima0_sym }}
    )
  )

return(planilla)

}
