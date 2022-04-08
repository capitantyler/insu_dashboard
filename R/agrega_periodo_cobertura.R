#' Función agrega_periodo_cobertura
#'
#' emision agrupada por periodo
#'
#' @param emision_periodo    emision original, o agrupada por periodo y contrato
#'
#' @return
#' @export
#'
#' @examples
agrega_periodo_cobertura <- function(
  emision_periodo
){

  cobertura <- tryCatch({
    emision_periodo %>%
      group_by(PER) %>%
      summarise(
        across(
          c(
            TRABAJADORES, TRABAJADORESMESsinDOM, TRABAJADORESMES,
            TRABAJADORESMES_r,
            MASA_r, MASA_desest, PRIMA_r, PREMIO_r, COMI_r
          ),
          .fns = ~sum(., na.rm = TRUE)
        ),
        CONTRATOS = n_distinct(CONTRATO),
        MESES = mean(MESES),
        .groups = "drop"
      ) %>%
      mutate (
        # CONTRATOS = MESES / 12,
        TRABAJADORESPROM =  TRABAJADORESMES / MESES,
        `CxT%` = if_else(MASA_r == 0, 0, PREMIO_r / MASA_r),
        `COMI%` = if_else(PREMIO_r == 0, 0, COMI_r / PREMIO_r),
        SALARIO = if_else(
          TRABAJADORESMESsinDOM== 0, 0,
          MASA_desest / TRABAJADORESMESsinDOM
        )
      ) %>%
      select( #variables a exponer
        PER,
        CONTRATOS, TRABAJADORES, TRABAJADORESMES_r,
        TRABAJADORESPROM, SALARIO, PREMIO_r, PRIMA_r,
        COMI_r, `CxT%`, `COMI%`
      ) %>%
      arrange ( # ordeno por AÑO
        PER
      )

  },
  error = function(e){
    message("ocurrió un error en agrega_periodo_cobertura")
    message(e)
  }
  )

  return(cobertura)

}

