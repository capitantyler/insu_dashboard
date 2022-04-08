#' Números lindos para los máximos de escalas
#'
#' @param x      numeric  el valor a reescalar hacia arriba
#' @param nice   numeric  la lista de bases de escala
#'
#' @return
#' @export
#'
#' @examples
#'
roundUpNice <- function(
  x,
  nice = c(1,2,4,5,6,8,10,15,20,50,100)
){
  if(length(x) != 1) stop("roundUpNice: 'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}
