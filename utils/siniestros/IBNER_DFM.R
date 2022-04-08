#' IBNER_DFM
#'
#' Calcula y distribuye el costo pendiente de un conjunto de siniestros del mismo
#' período de ocurrencia mediante el algoritmo de Bornhuetter-Fergusson.
#'
#' IBNER[D+1,D+2,...] = ultimate x (% sin pagar[D+1,D+2,...])
#'
#' @param d         # desarrollo, en períodos [0,D]
#' @param fda_d     # factores de desarrollo homogéneos
#' @param t         # tiempo, en periodos. [-T,0]. Necesario para obtener D = d-t
#' @param fct_inf_fut # coef de inflación futura despecto a la última diagonal
#' @param tail_d    # factor de cola optativo
#' @param ult       # ultimate esperado
#' @param estricto  # logical.  Si es falso, intenta estirar los fda y fct_inf con
#'                              splines polinómicos.
#'                              si las series de fda o fct_inf son cortas.
#'
#' @return
#' @export
#'
#' @notes @bc y @o deben estar en la misma escala (pesos o tasa).
#'
#' @examples
IBNER_DFM <- function(
  # o,
  d,
  t,
  fda_d,
  ult,
  fct_inf_fut = 1,
  tail_d = 1,
  estricto = TRUE
){

  # FACTORES DE DESARROLLO ACUMULADOS ----

  # número de factores
  D <- d[1] + -t[1] # D vale lo mismo para toda observación
  # completo factor desarrollo si es necesario
  # los fda son uno menos que el valor observado d (d=0, 1,....,D, tail)
  if(tail_d != 1)
    fda_d <- c(fda_d * tail_d, tail_d)

  if(!estricto){
    if(length(fda_d) < D) {
      warning("no alcanzan los fda. Se extiende con un spline")
      fda_d <- rev(spline(
        rev(fda_d), method = "natural",
        n = D+1, xmax = D+1
      )$y)
    }
  }

  # agrego a los fda la cola y un 1 para que se pueda dividir el más desarrollado
  fda_d <- c(fda_d, 1)

  fda <- fda_d[(D+2):length(fda_d)]
  fda_lag <- fda_d[(D+1):(length(fda_d) - 1)]

  # INFLACION FUTURA ----
  # completo factor inflación futura si es necesario

  if(length(fct_inf_fut) == 1){
    # trátase de un factor constante
    fct_inf_fut <- cumprod(rep(fct_inf_fut, length(fda)))
  } else {
    if(length(fct_inf_fut) < length(fda)) {
      fct_inf_fut <- spline(
        fct_inf_fut, method = "natural",
        n = length(fda), xmax = length(fda)
      )$y
    }
    fct_inf_fut <- fct_inf_fut[1:length(fda)]
  }

  fct_inf_fut <- fct_inf_fut[1:length(fda)]

  x_est <- ult * (1/fda - 1/fda_lag) * fct_inf_fut

  return(x_est)

}
