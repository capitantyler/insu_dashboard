#' AAAAMM_ejercicio
#'
#' devuelve el ejercicio económico de una fecha en formato AAAAMM
#'
#' @param x              integer   fecha en formato AAAAMM
#' @param cierreDeMes    integer   mes de cierre. Default (6)
#'
#' @return     integer   ejercicio económico
#' @export
#'
#' @examples
#'
#' AAAAMM_ejercicio(201902)
#' [1] 2018
#' AAAAMM_ejercicio(201902, 12)
#' [1] 2019
#'
#' @notes
#'
#'
AAAAMM_ejercicio <- function(x, cierreDeMes = 6L) {
  if_else(
    between(x %% 100, 0, cierreDeMes) & cierreDeMes != 12L, 
    x %/% 100 - 1L, 
    x %/% 100
  )
}
