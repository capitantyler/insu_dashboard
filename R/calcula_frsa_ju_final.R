#' Calcula frecuancia judicial definitiva
#'
#' @param FRJA
#' @param FRJN
#'
#' @return
#' @export
#'
#' @examples
#'
#' @notes  hay que mejorarla
#'
calcula_frsa_ju_final <- function(FRJA, FRJN){
  pmax(FRJA, FRJN)
}
