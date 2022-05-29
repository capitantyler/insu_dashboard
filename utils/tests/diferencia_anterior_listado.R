# diferencias en bases de reservas
diferencia_anterior_listado <- function(
  base_ant,
  base_post,
  by,
  incluir_sin_cambios = FALSE,
  ...
){
  
  UseMethod("diferencia_anterior_listado")

}

diferencia_anterior_listado.data.frame <- function(
  base_ant,
  base_post,
  by,
  incluir_sin_cambios = FALSE,
  ...
){
  
  # columnas_metricas <- list(...)
  columnas_metricas <- c(...)

  # columnas comunes que no son ni key (DENUNCIA) ni métricas
  columnas_comunes <- intersect(
    names(base_ant), names(base_post)
  )
  columnas_comunes <- columnas_comunes[!columnas_comunes %in% by]
  columnas_comunes <- columnas_comunes[!columnas_comunes %in% columnas_metricas]
  
  dif_Q <- anti_join(
    base_post, 
    base_ant %>% 
      select(-all_of(columnas_comunes)) %>% 
      select(all_of(c(by, columnas_metricas))),
    by = by
  )

  dif_Q2 <- anti_join(
    base_ant,
    base_post %>% 
      select(-all_of(columnas_comunes)) %>% 
      select(all_of(c(by, columnas_metricas))),
    by = by
  )
  
  compara_valor <- inner_join(
    base_post, 
    base_ant %>% 
      select(-all_of(columnas_comunes)) %>% 
      select(all_of(c(by, columnas_metricas))),
    by = by,
    suffix = c("", ".ant")
  ) 

  for(col in columnas_metricas){
    col_ant <- glue("{col}.ant")
    compara_valor <- compara_valor %>% 
      mutate(
        "{col}_dif" := replace_na(!!sym(col), 0) - replace_na(!!sym(col_ant), 0)
      )
  }

  columnas_metricas_dif <- glue("{columnas_metricas}_dif")
  
  dif_valor <- compara_valor %>% 
    filter_at(all_of(columnas_metricas_dif), any_vars(. != 0))

  if(incluir_sin_cambios){

    sin_cambios <- compara_valor %>% 
      filter_at(all_of(columnas_metricas_dif), all_vars(. == 0))
    
    list(
      altas = dif_Q,
      bajas = dif_Q2,
      dif_valor = dif_valor,
      sin_cambios = sin_cambios
    )
    
  } else {
    
    list(
      altas = dif_Q,
      bajas = dif_Q2,
      dif_valor = dif_valor
    )
    
  }
  
}
