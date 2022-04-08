#' comprimir números
#' -----------------
#' Comprime un número hasta obtener un número de dos decimales
#'
#' @param tx
#'
#' @return
#' @export
#'
#' @examples
comprimir_num <- function(tx, dec = 2) {
  num <- as.numeric(gsub("\\,", "", as.character(tx)))
  sign <- ifelse(num < 0, "-", "")
  div <- findInterval(
      abs(num),
      c(0, 1e3, 1e6, 1e9, 1e12)
    )
  paste0(
    round(num/10^(3*(div-1)), dec),
    c(""," Mil"," Mill"," milMM"," B")[div]
  )
}
