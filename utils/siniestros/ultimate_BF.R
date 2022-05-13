#' ultimate_BF
#'
#' distribuye el costo total de un conjunto de siniestros del mismo
#' período de ocurrencia mediante el algoritmo de Bornhuetter-Fergusson.
#'
#' IBNER = burning cost x (% sin pagar)
#' Ultimate = pagos actuales + IBNER
#'
#' Se obtiene el desarrollo máximo como D = max(d)+max(t)
#' experimental: también da d[1] + t[1], ya que la suma siempre da D
#'
#' @param d         # desarrollo, en períodos (inicio = 0)
#' @param t         # tiempo, en períodos (hoy = 0)
#' @param o         # observaciones, en períodos
#' @param ibner     # pagos pendientes, en períodos
#' @param estricto  # logical.  Si es falso, intenta estirar los fda y fct_inf con
#'                              splines polinómicos. Si es verdadero, tira error
#'                              si las series de fda o fct_inf son cortas.
#'
#' @param fct_inf   # numeric. Vector de factores de inflación, siendo la
#'                             primer posición la más reciente (generalmente 1)
#'
#' @return
#' @export
#' @note habría que ver si se puede guardar d y t como vectores continuos
#' lobstr::obj_size(df[[8]][[1]][["d"]]) 
#' > 1,504 B
#' lobstr::obj_size(0:192)
#' > 680 B
#'
#' @examples
ultimate_BF <- function(
  d,
  t,
  o,
  ibner,
  fct_inf = 1,
  estricto = TRUE
){

  D <- d[1] + -t[1]
  x <- numeric(D+1) #genero vector de ceros
  x[d+1] <- o
  #t_max <- max(-t+1)
  t_max <- D+1
  
  if(length(fct_inf) != 1){
    # si se pasa fct_inf, se está pidiendo moneda homogénea

    if(estricto){
      if(length(fct_inf) < t_max){
        stop("ultimate_BF: vector fct_inf corto")
      } 
    } else {
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
    
    # if(length(fct_inf) != length(x)){
    #   print(x)
    #   print(fct_inf)
    #   readline(prompt = "Press [enter]")
    # }
    
    x <- x / rev(fct_inf)
    
  }

  ibner <- unlist(ibner)
  d <- 0:(D+length(ibner))
  t <- d - D
  x_full <- c(x, ibner)
  # o_full <- c(x, rep(0, length(ibner)))

  # return(list(d = d, t = t, o = o, x = x_full))
  return(list(d = d, t = t, x = x_full))
  
}
