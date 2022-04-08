#' ultimate_DFM
#'
#' Calcula el ultimate a pesos constantes mediante el método de
#' factores de desarrollo (DFM).
#'
#' ultimate = [pagos acum] x fda x tail
#'
#' Las observaciones son ternas o(d, t)
#'
#' Los factores de desarrollo d se asumen inician en 0. Si existen d que no están
#' en fda_d, se estiman con splines polinómicos.
#'
#' Los factores de inflación t se asumen terminan en 0 (última diag). Si existen
#' t que no están en fct_inf, se estiman con splines polinómicos.
#'
#'
#' @param o         # observaciones, en períodos
#' @param d         # desarrollo, en períodos [0,D]
#' @param t         # tiempo, en períodos [T,0]
#' @param fda_d     # factores de desarrollo homogéneos
#' @param fct_inf   # coeficiente de inflación pasada despecto a la última diagonal
#'                  # siendo la primer posición, l amás reciente (generalmente).
#' @param tail_d    # factor de cola optativo
#' @param estricto  # logical.  Si es falso, intenta estirar los fda y fct_inf con
#'                              splines polinómicos. Si es verdadero, tira error
#'                              si las series de fda o fct_inf son cortas.
#'
#' @return
#' @export
#'
#' @examples
ultimate_DFM <- function(
  o,
  d,
  t,
  fda_d,
  fct_inf,
  tail_d = 1,
  estricto = TRUE
){
  
  # ubico última diagonal
  D <- d[1] + -t[1] # D vale lo mismo para toda observación
  t_max <- D+1
  
  # completo factor desarrollo si es necesario
  # los fda son uno menos que el valor observado d (d=0, 1,....,D, tail)
  if(tail_d != 1)
    fda_d <- c(fda_d * tail_d, tail_d)

  if(!estricto){
    # reviso fdas
    if(length(fda_d) < D) {
      warning("no alcanzan los fda. Se extiende con unos")
      fda_d <- c(fda_d, rep(1, D - length(fda_d)))
    }
    
  }
  
  if(fct_inf != 1){
    # COEFICIENTES INFLACIONARIOS PASADOS ----
    #t_max <- max(-t+1)
    
    if(!estricto){
      # completo factor inflación si es necesario
      if(length(fct_inf) < t_max) {
        fct_inf <- spline(
          fct_inf, method = "natural",
          n = t_max, xmax = t_max
          #n = length(fct_inf), xmax = length(fct_inf)
        )$y
      }
    }
    
    fct_inf <- fct_inf[1:t_max]
    
    # sea x un valor incurrido en el momento t, siendo t = 0 para la diagonal
    x <- numeric(t_max)
    x[-t+1] <- o
    
    # des-inflo los valores históricos y aplico la inflación acumulada
    y <- x / rev(fct_inf)

  } else {
    y = x
  }
    
  # acumulo
  Y <- sum(y)

  # FACTORES DE DESARROLLO ACUMULADOS ----
  # es D o D+1??
  ultimate <- Y * fda_d[D+1]

  return(ultimate)

}
