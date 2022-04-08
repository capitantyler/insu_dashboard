## FUNCIONES de uso comun ##

is.not.null <- function(
  # negado de is.null
                        x
) {
  !is.null(x)
}

"%!in%" <- function(
  # negado de a %in% b
                    a, b
) {
  !a %in% b
}

my.min <- function(x) {
  # mínimo que funciona cuando no hay valores
  ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
}

my.max <- function(x) {
  # máximo que funciona cuando no hay valores
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
}
