#' rescaleUp
#' ---------
#' Reescalamiento lineal con la fórmula
#'
#'         (b-a)(x - min)
#' f(x) = --------------  + a
#'           max - min
#'
#' donde min y max se calculan de x.
#' Si x es un único valor o min(x) = max(x), se devuelve b.
#' Si b <= a, se devuelve error
#'
#' @param x  numeric   vector a re-escalar
#' @param a  numeric   nuevo mínimo de escala
#' @param b  numeric   nuevo máximo de escala
#'
#' @return
#' @export
#'
#' @examples
rescaleUp <- function(
  x, a = 0, b = 1
){
  if(b-a < 0) stop("rescaleUp: a debe ser menor a b para la nueva escala")
  if(length(x) != 1){
    x_min <-  min(x)
    x_max <-  max(x)
    if(
      x_min != x_max
    ) {
      return(
        a + (b-a)*(x - x_min)/(x_max - x_min)
      )
    } else {
      return(b)
    }
  } else {
    return(b)
  }
}
