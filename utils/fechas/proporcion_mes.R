#' proporcion_mes
#'
#' calcula la proporción del mes que se ha cubierto
#'
#' @param mes         datetime  mes
#' @param inivig      datetime  mes de inicio de vigencia contrato
#' @param rescision   datetime  mes de rescision contrato
#'
#' @return
#' @export
#'
#' @examples
#'
#'
proporcion_mes <- function(
    mes,
    inivig,
    rescision,
    escala_premio = 1e10
  ) {

  library(lubridate)
  library(dplyr)
  
  # necesito este if porque el 06-07-2017 se
  # corrigió en la tabla de meses el mes agosto que figuraba con 30 días
  # ajuste_201708 <- -1L * as.integer(
  #   (month(mes) == 8 & year(mes) < 2017)
  # )
  
  inimes <- floor_date(mes, "month")
  finmes <- ceiling_date(mes, "month") #- ajuste_201708
  finmes <- finmes - match(
    year(mes) < 2017 && month(mes) == 8, 
    -1,
    nomatch = 0
  )
  # finmes <- if_else(
  #   month(mes) == 8 & year(mes) < 2017,
  #   finmes - 1,
  #   finmes
  # )

  inicio <- pmax(inivig, inimes)
  fin <- if_else(
    rescision == "1900-01-01" | rescision == finmes - 1, 
    finmes, 
    pmin(rescision, finmes)
  )

  # el redondeo es a dos decimales en la base diaria (Gladys)
  diario <- round(escala_premio / days_in_month(mes), digits = 2L)
  prop0 <- pmax(0L, (fin - inicio)) * diario
  prop <- prop0 / escala_premio
  
  # redondeo a la cifra que tenga menos dígitos significativos
  digitos_signif <- pmin(
    nchar(sub('^\\d+\\.', '', sub('0+$', '', format(prop0, scientific = FALSE)))),
    nchar(sub('^\\d+\\.', '', sub('0+$', '', format(escala_premio + 1, scientific = FALSE))))
  )

  # aplico dígitos significativos si pudo calcularse
  if(length(digitos_signif) != 0){
    prop <- signif(prop, digitos_signif)
  }
  
  return(
    prop
  )
}
