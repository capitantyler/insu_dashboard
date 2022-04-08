#' IBNR_siniestros_tasa
#'
#' Devuelve el factor de IBNR de siniestros
#'
#' @param MESACC       integer mes de accidente formato AAAAMM
#' @param MESFIN       integer mes de observación formato AAAAMM
#' @param patron_fda   numeric  factor acumulaod  a aplicar al momento de grabación
#'
#' @return  numeric   factor de IBNR
#' @export
#'
#' @note
#' fuente:
#'
#'  file:///\\acen0104\Gerencia_Técnica\siniestros\frecuencia\Estacionalidad%20siniestros\IBNR%20Denuncias.xlsx
#'
#' @examples
#'
IBNR_siniestros_tasa <- function(
  MESACC,
  MESFIN,
  patron_fda = c(
    1.17432,
    1.04735,
    1.02856,
    1.01803,
    1.01068,
    1.00507
  ),
  cola = 1
){

  DELAY <- pmax(
    # si no se eliminan posibles delays negativos, se produce el siguiente error:
    # Error in patron_fda[DELAY[]] :
    # solamente 0's pueden ser mezclados con subscritos negativos
    (MESFIN %/% 100- MESACC %/% 100)  * 12L +
    (MESFIN %% 100- MESACC %% 100) + 1L,
    1L
  )
  if_else(
    DELAY > length(patron_fda),
    cola,
    patron_fda[DELAY]
  )

}
