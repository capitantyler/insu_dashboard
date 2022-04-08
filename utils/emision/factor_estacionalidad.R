#' factor_estacionalidad
#'
#' Devuelve el factor de estacionalidad asociado al mes AAAAMM y región ID que
#' se especifica.
#'
#' @param MES integer mes formato AAAAMM
#' @param REG_ID integer ID de región. Si no se ingresa se toma el promedio.
#'
#' @return numeric factor de estacionalidad
#' 
#' @note 
#' 
#' @export
#'
#' @examples
factor_estacionalidad <- function(
  MES,
  REG_ID = 99,
  estacionalidad = data.frame(
    mes = as.integer(rep(1:12, 5)),
    REG_ID = as.integer(rep(c(0:3,99), each =12)),
    factor = c(
      1.46,	1.06,	0.98,	1.10,	1.01,	1.00,	1.45,	1.00,	0.98,	1.00,	1.00,	1.00,
      1.42,	1.00,	0.95,	0.94,	0.96,	1.00,	1.40,	1.00,	0.98,	1.00,	1.00,	1.00,
      1.46,	1.00,	0.97,	0.97,	0.99,	1.00,	1.42,	1.00,	0.98,	1.00,	1.00,	1.00,
      1.46,	1.04,	0.95,	1.01,	1.00,	1.00,	1.42,	1.00,	0.98,	1.00,	1.00,	1.00,
      1.46,	1.05,	0.98,	1.10,	1.00,	1.00,	1.44,	1.00,	0.98,	1.00,	1.00,	1.00
    )
  )
){
  
  library(dplyr)
  
  mes <- as.integer(MES %% 100)

  # n <- 1:length(estacionalidad$factor)
  # i <- (estacionalidad$REG_ID == REG_ID) & (estacionalidad$mes == mes)
  # factor <- estacionalidad$factor[i]
  # return(factor)
  
  
  df <- data.frame(
    mes = mes,
    REG_ID = REG_ID
  ) %>%
  left_join(
    estacionalidad,
    by = c("mes", "REG_ID")
  )
  return(df$factor)
    
}
