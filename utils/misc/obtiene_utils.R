#' obtiene_utils
#'
#' Obtiene direcciones de repositorio de utils
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
obtiene_utils <- function(
  path = "//acen0104/Gerencia_T\u00e9cnica/programaci\u00F3n/R/02_Code/utils/"
){
  # #en AWS
  # path = iconv("D:/Proyectos_R/etc/", to = "UTF-8")

  repositorio <- list()
  repositorio[1] <- path
  repositorio[2] <- paste0(repositorio[1], "bases/")
  repositorio[3] <- paste0(repositorio[1], "fechas/")
  repositorio[4] <- paste0(repositorio[1], "contratos/")
  repositorio[5] <- paste0(repositorio[1], "emision/")
  repositorio[6] <- paste0(repositorio[1], "siniestros/")
  repositorio[7] <- paste0(repositorio[1], "estilos/")
  repositorio[8] <- paste0(repositorio[1], "excel/")
  repositorio[9] <- paste0(repositorio[1], "plot/")
  repositorio[10] <- paste0(repositorio[1], "misc/")
  repositorio[11] <- paste0(repositorio[1], "tests/")


  names(repositorio) <- c(
    "raiz", "bases", "fechas", "contratos", "emision",
    "siniestros", "estilos", "excel", "plot", "misc",
    "tests"
  )

  return(repositorio)
}
