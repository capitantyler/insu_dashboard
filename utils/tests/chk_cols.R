#' Chequeo de columnas en objeto
#'
#' @param obj     data.frame, tibble, data.table u otro objeto con attr "names"
#' @param cols    character  lista de nombres de columna incluidos en names
#'
#' @return
#' @export
#'
#' @examples
#' > chk_cols(cars, "autito")
#' Error in chk_cols(cars, "autito") :
#' El argumento cars no tiene la/s columna/s autito

chk_cols <- function(
  obj, cols, stop = FALSE
){
  I <- cols %in% names(obj)
  if(!all(I)) {
    msg = paste0(
      "El argumento ", deparse(substitute(obj)),
      " no tiene la/s columna/s ", cols[!I], collapse = ", "
    )
    if(stop) stop(msg) else warning(msg)
  }

}
