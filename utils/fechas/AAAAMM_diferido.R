#' AAAAMM_diferido
#'
#' @param x    integer   fecha formato AAAAMM
#' @param h    integer   diferimiento en meses 
#'
#' @return     integer   fecha formato AAAAMM
#' @export
#'.
#' @examples
#' AAAAMM_diferido(201901, -13)
#' 201712
#'
#'
AAAAMM_diferido <- function(x, h) {
  
  library(dplyr)
  
  anio <- x %/% 100
  mes <- x %% 100
  
  h_anio <- h %/% 12 
  h_meses <- h %% 12
  
  anios <- anio + h_anio
  meses <- mes + h_meses

  AAAAMM_diferido <- (anios + meses %/% 12) * 100 +
    if_else(
      meses %% 12 == 0,
      -88,
      meses %% 12
    )
  
  return(as.integer(AAAAMM_diferido))
}
