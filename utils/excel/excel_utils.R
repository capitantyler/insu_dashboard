# funciones Excel ----

library(dplyr)
library(openxlsx)

colExcel2num <- function(x) {
  p <- seq(from = nchar(x) - 1, to = 0)
  y <- utf8ToInt(x) - utf8ToInt("A") + 1L
  S <- sum(y * 26^p)
  return(S)
}

## Converts a number to base 26, returns a vector for each "digit"
b26 <- function(n) {
  stopifnot(n >= 0)
  if (n <= 1) return(n)
  n26 <- rep(NA, ceiling(log(n, base = 26)))
  for (i in seq_along(n26)) {
    n26[i] <- (n %% 26)
    n <- n %/% 26
  }
  return(rev(n26))
}

## Retorna el nombre de columna Excel según la posición de columna
## A, B, C, ..., Z, AA, AB, AC, ..., AZ, BA, ...
colnum2Excel <- function(n, lower = FALSE) {
  let <- if (lower) letters else LETTERS
  base26 <- b26(n)
  i <- base26 == 0
  base26[i] <- 26
  base26[lead(i, default = FALSE)] <- base26[lead(i, default = FALSE)] - 1
  paste(let[base26], collapse = "")
}

## Retorna el número de columna en formato Excel
## A, B, C, ..., Z, AA, AB, AC, ..., AZ, BA, ...
## buscando el número de columna en el df
varnum2Excel <- function(df, colname, lower = FALSE) {
  index <- match(colname, names(df))
  stopifnot(index > 0)
  return(colnum2Excel(index))
}

filanro_metrica <- function(
  # devuelve el número de fila en el campo métrica
  data, 
  metrica
){
  mutate(data, f = row_number()) %>%
    filter(metrica == !!metrica) %>% 
    pull(f)
}

colExcel2num <- function(
  #convierte letra de columna excel en número de columna
  x
) {
  p <- seq(from = nchar(x) - 1, to = 0)
  y <- utf8ToInt(x) - utf8ToInt("A") + 1L
  S <- sum(y * 26^p)
  return(S)
}

colExcelAutofit <- function(
  # función que intenta medir en pixeles el largo de una cadena de texto
  df, fontsize, n = 50, span = 2
) {
  if(nrow(df) == 0L){
    return()
  } else {
    defaultsize <- 12
    # multiplier depende de tipo de fuente {1.61803 número de oro, 2, 
    # o un número del 1 al 2}
    multiplier <- 1.61803
    width_vec <- apply(
      head(df, n), 2,
      function(x) max(nchar(as.character(x)) + span, na.rm = TRUE)
    )
    width_vec_header <- nchar(colnames(df)) + span
    max_vec_header <- pmax(width_vec, width_vec_header)
    return(max_vec_header * multiplier^((fontsize - defaultsize) / 2))
  }
}


addStyles <- function(
  # aplicar varios estilos 
  wb, sheet, estilos, rows, cols, gridExpand = FALSE, stack = TRUE
){
  if (!"Workbook" %in% class(wb)) 
    stop("First argument must be a Workbook.")
  invisible(
    for(estilo in seq_along(estilos)){
      addStyle(
        wb = wb, sheet = sheet,
        style = estilo,
        rows = rows, cols = cols,
        gridExpand = gridExpand, stack = stack)
    })
}

ExcelprintArea <- function(
  # redefine print area
  wb, sheet,
  rows, cols
) {
  if (!"Workbook" %in% class(wb)) 
    stop("First argument must be a Workbook.")
  printArea <- paste0(
    "<definedName name=\"_xlnm.Print_Area\" localSheetId=\"0\">",
    sheet, 
    "!$", int2col(cols[1]), "$", rows[1], 
    ":$", int2col(cols[length(cols)]), "$", rows[length(rows)],
    "</definedName>"
  )
  wb$workbook["definedNames"]<- printArea
  invisible(0)
}
