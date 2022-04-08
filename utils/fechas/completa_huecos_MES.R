#' completa_huecos_MES
#'
#' Completa huecos mensuales en una tabla.
#'
#' @param df
#' @param mes_min
#' @param mes_max
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @notes
#' Se probaron varias estrategias para mejorar la velocidad
#' a) se dividió AAAAMM en AAAA y MM, luego se hizo producto cartesiano de cada
#' uno de ellos, se volvió a unir y filtrar
#' b) se usó seq() convirtiendo todo a Date (lento)
#' c) se usa un vector auxiliar con los meses posibles y se hace un crossjoin
#' inicial entre DENUNCIAS y estos meses posibles. Luego se filtra con los
#' mes_min y mes_max previamente calculados.
#' 
completa_huecos_MES <- function(
  ...
){
  UseMethod("completa_huecos_MES")
}

completa_huecos_MES.data.table <- function(
  df,
  col_mes,
  mes_max
){
  
  if(nrow(df) == 0L) return(df)
  
  col_mes_str <- deparse(substitute(col_mes))
  
  keycols <- unique(c("DENUNCIA", col_mes_str, key(df)))
  setkeyv(df, keycols)
  
  # 1. creo vector general de referencia
  ref <- data.table(
    MES = min(df[[col_mes_str]]):pmin(
      AAAAMM_diferido(max(df[[col_mes_str]]), 1L), 
      mes_max
    )
  )
  ref <- ref[between(MES %% 100, 1L, 12L)]
  
  # 3.armo índice
  
  indx <- CJ(unique(df$DENUNCIA), ref[["MES"]])
  setnames(indx, c("V1", "V2") , c("DENUNCIA", col_mes_str))
  setkeyv(indx, c("DENUNCIA", col_mes_str))
  #indx[,tmp_var := 0]
  
  # 2.calculo mínimos y máximos de col_mes por DENUNCIA y lo sagrego al índice
  
  if(!is.null(mes_max)){

    df[, 
      `:=`(
        mes_min = min(.SD),
        mes_max = pmin(
          AAAAMM_diferido(max(.SD), 1L), 
          mes_max
        )
      ),
      .SDcols = col_mes_str,
      by = DENUNCIA
    ]
    
  } else {
    
    df[, 
       `:=`(
         mes_min = min(.SD),
         mes_max = max(.SD)
      ),
      .SDcols = col_mes_str,
      by = DENUNCIA
    ]
    
  }

  indx <- indx[unique(df[,.(DENUNCIA, mes_min, mes_max)]),on = "DENUNCIA"]
  # UPDATE: recorto un poco el tamaño enorme de indx
  indx <- indx[
    MES >= mes_min
  ][
    MES <= mes_max
  ]
  
  df[, c("mes_min", "mes_max") := NULL]
  indx[, c("mes_min", "mes_max") := NULL]

  # 4.hago el full_join por DENUNCIA y MES
  # df <- df[indx, roll=TRUE] 
  df <- df[indx] 
  
  return(df)

}
