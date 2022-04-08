#' completa_periodos_IBNER
#'
#' completa un df mensual(MES) con valores futuros de IBNER por MES.
#' devuelve las columnas del df más las columnas completas de
#' MES, d, t, {names}, siendo {names} las columnas a completar de IBNER.
#' 
#' Las columnas del df original son simplificadas a una por DENUNCIA,
#' de manera que la extensión por MES de liquidado/incurrido se sumariza: las columnas
#' terminadas en "_LIQ|_RVA|_INC" se suman por DENUNCIA.
#' 
#'
#' Si names es nulo, adopta los nombres de las columnas originales a completar {cols}
#'
#' {metodo} elige el metodo de IBNER, que se parametriza con los argumentos pasados
#' en {...}.
#' Los métodos son:
#'  -- BF: método de Bornhuetter-Fergusson
#'  -- BF_modificado: método de Bornhuetter-Fergusson normalizado por una variable
#'                    observada, generalmente por salario emitido,
#'  -- CL: método de chain Ladder. Aplica BF, pero los ultimates a priori son
#'         estimados con la misma información del df.
#'  -- Salario: método obsoleto.
#'  -- NO: No usa IBNER y devuelve el mismo df con las columnas de IBNER en 0.
#'
#' @param df
#' @param cols    character. Nombres de columnas a completar. 
#'                           Acepta fórmulas iniciando con ~
#' @param names   character. Nombres de las columnas ultimate a devolver
#' @param metodo
#' @param ...
#'
#' @return
#' @export
#' @note TODO: estudiar la posibilidad de pasar al objeto df todos los
#' parámetros de IBNER como atributos en el objeto de salida con clase
#' "stros_proyectados"
#' 
#' @examples
completa_periodos_IBNER <- function(
  df,
  cols,
  names = NULL,
  metodo = c("BF", "BF_modificado", "CL", "Salario", "NO"),
  ...  
){

  # fórmula para evaluar columnas en forma de fórmula
  eval_text_formula <- function(f, env){
    eval(parse(text=f), envir = env, enclos = parent.frame())
  } 
  
  ## evalúo método seleccionado
  metodo <- match.arg(metodo)
  
  args <- list(...)
  
  metodos_BF <- c("CL", "BF", "BF_modificado")
  #### tareas comunes a todos los modelos ----

  # si el df está vacío, devuelvo un df vacío pero con las columnas esperadas
  if(nrow(df) == 0L){
    df[, c("MES", "t", "d")] <- NA_integer_
    if(metodo %in% metodos_BF){
      df[, paste0(names, "_H")] <- NA_real_
      df[, do.call(paste0, expand.grid(
        c("ILT", "ESP", "ILP", "JUI"), 
        c("_LIQ_0", "_IBNER")
      ))] <- NA_real_
    }
    df[, names] <- NA_real_
    return(df)
  }  
  
  crea_names <- function(cols, names){
    ## verifico que se hayan pasado los nuevos nombres de columnas
    if(!is.null(names)){
      if(length(names) != length(cols)) stop(
        paste0(
          "completa_periodos_IBNER: la longitud de las columnas a generar IBNER ",
          "y la de sus nuevos nombres no coinciden.",
          paste(cols, names)
        )
      )
    } else {
      names <- paste0(cols, "_ULT")
    }
    return(names)
  }
  
  agrega_d_t <- function(df, mes_cierre){
    
    # agrego d y t
    df[, c("d", "t")] <-(
      cbind(df[, "MES"], df[, "MES"]) %/% 100L - cbind(df[, "MESACC"], mes_cierre) %/% 100L
    ) * 12L + (cbind(df[, "MES"], df[, "MES"]) %% 100L - cbind(df[, "MESACC"], mes_cierre) %% 100L)
    
    # detengo proceso si hallo liquidaciones posteriores al mes_cierre
    if(any(df$t > 0L)) stop(
      paste0(
        "Existen valores posteriores al mes_cierre := ",
        mes_cierre,
        " en denuncia := ", unique(df[df$t > 0L, "DENUNCIA"])
      )
    )
    
    # advierto y corrijo si hallo liquidaciones posteriores al mes_cierre
    if(any(df$d < 0)){
      
      i <- (df$d < 0)
      df[i, "t"] <- df[i, "t"] - df[i, "d"]
      df[i, "MES"] <- df[i, "MESACC"]
      df[i, "d"] <- 0L
      
      warning(
        paste0(
          "Existen liquidaciones anteriores al MESACC. ",
          "Se fijan con d = 0, t = -D  y MES = MESACC.",
          "DENUNCIAS: ",
          print(unique(df[i, "DENUNCIA"]))
        )
      )
    }
    
    return(df)
  }

  # no funciona, no sé xq: https://stackoverflow.com/questions/66605987/usemethod-common-tasks-all-s3-classes/66627287#66627287
  # NextMethod("completa_periodos_IBNER")
  UseMethod("completa_periodos_IBNER", df)
}


completa_periodos_IBNER.data.frame <- function(
  df,
  cols,
  names = NULL,
  metodo = c("BF", "BF_modificado", "CL", "Salario", "NO"),
  ...
){

  if (!inherits(df, "data.frame"))
    stop("el primer objeto debe ser de clase data.frame") # control de objeto

  # coerciono posible lista de cols con fórmulas
  cols <- as.character(cols)
  # repaso nombres de nuevas columnas
  names <- crea_names(cols, names)

  # chequeo llave DENUNCIA, MES
  if(nrow(df) != nrow(distinct(df, DENUNCIA, MES))){
    warning(paste(
      "completa_periodos_IBNER: ",
      "valores repetidos por MES y DENUNCIA. Se suman"
    ))
    df <- df %>%
      group_by(
        DENUNCIA, MES
      ) %>%
      mutate(
        across(
          all_of(cols),
          ~sum(.),
          .names = "{col}"
        )
      ) %>%
      ungroup() %>%
      distinct(DENUNCIA, MES, .keep_all = TRUE)
  }
  
  # agrego d y t. rectifico tiempos 
  df <- agrega_d_t(df, args$mes_cierre)
  
  # tareas comunes a métodos BF
  if(metodo %in% metodos_BF){

    # chequeo que se hayan pasado los elementos
    exist <- c("fda", "mes_cierre", "fct_inf_fut") %in% names(args)

    if(!any(exist)){
      stop(paste0(
        "falta en el cálculo de IBNER los argumentos ", args[!exist],
        collapse = ","
      ))
    }

    # verificación de fda
    if(length(cols) != length(args$fda) & length(args$fda) != 1L){
      if(
        all(c("fda_jui_2017", "fda_jui") %in% names(args$fda)) &
        "JUI_LIQ" %in% cols &
        length(cols) == length(args$fda) - 1L
      ){
        ## modificaciones AD HOC ----
        # si pasaron fda especiales para juicios previos 2017,
        # hacemos aquí la preparacióncde datos
        df <- df %>% mutate(
          JUI_LIQ_2017 = if_else(
            MESACC <= 201712, JUI_LIQ, 0
          ),
          JUI_LIQ = if_else(
            MESACC > 201712, JUI_LIQ, 0
          )
        )

        #reordeno columnas, fda y moneda_homogenea
        names <- c(
          names[-match("JUI_LIQ", cols)],
          names[match("JUI_LIQ", cols)], "JUI_ULT_2017"
        )

        cols <- c(
          cols[-match("JUI_LIQ", cols)],
          "JUI_LIQ", "JUI_LIQ_2017"
        )
        
        args$moneda_homogenea <- c(
          args$moneda_homogenea[-match("JUI_LIQ", cols)],
          rep(args$moneda_homogenea[match("JUI_LIQ", cols)],2)
        )

        # creo la columna ultimate a priori de juicios 2017
        if(metodo %in% c("BF", "BF-modificado")){
          sym_names_JUI <- sym(paste0(names[[length(names) - 1]],"_H"))

          df <- df %>% mutate(
            JUI_ULT_2017_H = !!sym_names_JUI
          )
        }

        args$fda <- args$fda[
          c(
            names(args$fda)[-match(c("fda_jui", "fda_jui_2017"), names(args$fda))],
            c("fda_jui", "fda_jui_2017")
          )
        ]

      } else {
        stop(paste0(
          "faltan factores de desarrollo para ", cols,
          collapse = ", "
        ))
      }
    }
    
    # verificación fct_inf_fut
    if(length(args$fct_inf_fut) == 1L){
      args$fct_inf_fut <- replicate(
        length(cols), args$fct_inf_fut, simplify = FALSE
      )
    }

    # sym_bolizo cols y names

    sym_cols <- lapply(cols, sym)
    sym_names <- lapply(names, sym)
    names_H <- paste0(names, "_H")
    sym_names_H <- lapply(names_H, sym)

    D <- max(df$d - df$t)

    # completo fdas

    for(i in 1:length(args$fda)){
      if(length(args$fda[[i]]) < D) {
        warning("no alcanzan los fda. Se extiende con unos")
        args$fda[[i]] <- c(args$fda[[i]], rep(1, D - length(args$fda[[i]]) ))
      }
    }

    # replico fda

    if(length(args$fda) == 1L){
      args$fda <- replicate(
        length(cols), args$fda, simplify = FALSE
      )
    }


  }

  #### método de Chain-Ladder ----

  if(metodo == "CL"){

    # agrupo por DENUNCIA + nest
    df <- df %>%
      group_by(
        DENUNCIA
      ) %>%
      nest()

    # realizo el IBNER
    for(i in 1:length(cols)){
      df <- df %>%
        mutate(
          !!sym_names_H[[i]] := map_dbl(
            .x = data,
            ~ultimate_DFM(
              o = `$`(.x, !!sym_cols[[i]]),
              d = .x$d, t = .x$t,
              fda_d = args$fda[[i]],
              fct_inf = args$fct_inf[[i]]
            )
          )
        )

    }

  }

  #### preparación método de Borhhuetter-Fergusson ----
  if(metodo %in% c("BF", "BF_modificado")){

    # chequeo que estén las columnas adicionales de ultimate esperado en el df
    chk_cols(
      df,
      names_H,
      stop = TRUE
    )

    # agrupo por DENUNCIA + nest
    df <- df %>%
      group_by(
        DENUNCIA
      ) %>%
      nest(data := -c(DENUNCIA, all_of(names_H)))
  }

  #### método de Borhhuetter-Fergusson generalizado----
  if(metodo %in% metodos_BF){
    # realizo el IBNER
    for(i in 1:length(cols)){

      df <- df %>%
        mutate(
          !!sym_names[[i]] := map(
            .x = data,
            ~ultimate_BF(
              o = `$`(.x, !!sym_cols[[i]]),
              d = .x$d, t = .x$t,
              fct_inf = args$fct_inf[[i]],
              ibner = map(
                .x = data,
                ~IBNER_DFM(
                  d = .x$d, t = .x$t,
                  fda_d = args$fda[[i]],
                  ult = !!sym_names_H[[i]],
                  fct_inf_fut = args$fct_inf_fut[[i]]
                )
              )
            )
          ),
          # como por cada vuelta del bucle se repiten las columnas d y t,
          # me veo obligado a buclearlas por separado, así se van pisando
          # y queda la final una sola vez
          celda = map(
            .x = !!sym_names[[i]],
            ~select(.x, c(d, t))
          ),
          # piso el df original con la selección de la columna de valores x
          !!sym_names[[i]] := map(
            .x = !!sym_names[[i]],
            # mutate + rename
            ~transmute(.x, !!sym_names[[i]] := x)
          )
        )
    }

    # desagrupo
    df <- df %>%
      ungroup() %>%
      mutate(
        data = map(
          .x = data,
          .f = slice_tail,
          n = 1
        )
      ) %>%
      unnest_wider(data)

    df <- df %>%
      select(
        -any_of(
          c("MES", "d", "t", cols)
        )
      ) %>%
      select(
        -ends_with("_ULT_S")
      )

    # unnest: habría que acelerarlo
    # https://www.r-bloggers.com/2019/10/much-faster-unnesting-with-data-table/
    df <- df %>%
      unnest(
        c(celda, all_of(names))
      ) %>%
      # hay que quedarse con una línea por siniestros (t = 0)
      # porque si no hay pagos te quedas sin siniestros con
      # cero Ultimate
      filter(
        if_any(
          all_of(names),
          ~.x > 0 | t == 0
        )
      ) %>%
      mutate(
        MES = AAAAMM_diferido(MESACC, d)
      )

  }

  #### Método del Salario (obsoleto) ----
  if(metodo == "Salario"){

    if("ibner_tabla_anual" %in% names(args)){
      stop(paste0(
        "falta en el cálculo de IBNER los argumentos ", args[!exist],
        collapse = ","
      ))
    }

    chk_cols(
      df,
      c(
        "MES", "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", # colsreserva actual
        "SALARIO", "EDOCONS", "F_ACC", #cols siniestros
      ),
      stop = TRUE
    )

    salario_minimo <- 8000

    df <- df %>%
      mutate (`AÑO_ACC` = year(F_ACC)) %>%
      left_join(
        ibner_tabla_anual %>% rename(
          `AÑO_ACC` = `AÑO`,
          ILP_ULT = IND_ULT
        ),
        by = c ("CLASE", "EDOCONS", "AÑO_ACC")
      ) %>%
      mutate(
        # ILP_ULT = if_else(
        #   ILP_RVA > 0,
        #   INC_ULT_sinBanda(EDOCONS, IND_RVA, IND_LIQ, PORINC, F_ACC, mes_cierre),
        #   0
        # ),
        across(
          c(ILT_ULT, ESP_ULT, ILP_ULT, JUI_ULT),
          ~pmax(SALARIO, !!salario_minimo) / SALARIO_CLASE * .,
          .names = "{col}"
        )
      )
    # falta re-distribuir el IBNER

  }

  #### Sin IBNER ----
  if(metodo == "NO"){

    # realizo el IBNER
    df <- df %>%
      rename(
        ILT_ULT = ILT_LIQ,
        ESP_ULT = ESP_LIQ,
        ILP_ULT = ILP_LIQ,
        JUI_ULT = JUI_LIQ
      )

  }

  #### final ----

  # AD-HOC: me fijo si se pasaron fda especiales de juicio por separado
  if(
    "JUI_ULT_2017" %in% names(df)
  ){
    sym_names_JUI <- sym_names[[length(names) - 1]]
    sym_names_H_JUI <- sym_names_H[[length(names) - 1]]

    df <- df %>% mutate(
      !!sym_names_JUI := !!sym_names_JUI + JUI_ULT_2017,
      !!sym_names_H_JUI := !!sym_names_H_JUI + JUI_ULT_2017_H,
      JUI_ULT_2017 = NULL,
      JUI_ULT_2017_H = NULL
    )

    names <- names[-match("JUI_ULT_2017", names)]
    names_H <- names_H[-match("JUI_ULT_2017_H", names_H)]
  }

  # reacomodo columnas
  df <- df %>%
    relocate(
      c(
        MES, d, t,
        all_of(c(names_H, names))
      ),
      .after = last_col()
    )

  return(df)

}

completa_periodos_IBNER.data.table <- function(
  df,
  cols,
  names = NULL,
  metodo = c("BF", "BF_modificado", "CL", "Salario", "NO"),
  ...
){

  # evalúo cualquier columna de datos que se pasó como fórmula
  if(any(unlist(lapply(cols, inherits, what = "formula")))){
    for(col in 1:length(cols)){
      if(inherits(cols[[col]], "formula")){
        #if(is_bare_formula(col_frm)){
        text_f <- tail(as.character(cols[[col]]), 1)
        name_f <- sub("^FDA_", "", names(args$fda[col]))
        text_f <- enquote(text_f)
        # se produce un warning
        # leer https://stackoverflow.com/questions/20687235/warning-invalid-internal-selfref-detected-when-adding-a-column-to-a-data-tab
        # Warning message:
        #   In `[.data.table`(df, , `:=`((name_f), eval(parse(text = text_f)))) :
        #   Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
        df[, 
           (name_f) := list(eval(parse(text = text_f), .SD))
        ]
        # quito la fórmula y le pongo el nombre asignado
        cols[[col]] <- name_f
      }
    }
  }
  
  # chequeo que df tengan las columnas de MES, MESACC y cols
  chk_cols(
    df,
    c("DENUNCIA", "MESACC", "MES", cols),
    stop = TRUE
  )

  # aplano cols, si vino como lista de characters y fórmulas
  cols <- unlist(cols)
  # repaso nombres de nuevas columnas
  names <- crea_names(cols, names)
  
  setkey(df, DENUNCIA, MES)

  # chequeo llave DENUNCIA, MES
  if(nrow(df) != nrow(unique(df[, .(DENUNCIA, MES)]))){
    warning(paste(
      "completa_periodos_IBNER: ",
      "valores repetidos por MES y DENUNCIA. Se suman"
    ))
    unique(df[,
      unlist(cols) := lapply(.SD, sum), keyby = .(DENUNCIA, MES),
      .SDcols = cols
    ],
    by = c("DENUNCIA", "MES")
    )
  }
  
  # agrego d y t. rectifico tiempos 
  df <- agrega_d_t(df, args$mes_cierre)
  
  # seteo la llave a DENUNCIA, t
  setkey(df, DENUNCIA, t)
  
  # tareas comunes a métodos BF
  if(metodo %in% metodos_BF){
    
    # chequeo que se hayan pasado los elementos
    exist <- c("fda", "mes_cierre", "fct_inf_fut", "moneda_homogenea") %in% names(args)
    if(any(args$moneda_homogenea) == TRUE){
      exist <- all(exist, "fct_inf" %in% names(args))
    }
    
    if(!any(exist)){
      stop(paste0(
        "falta en el cálculo de IBNER los argumentos ", args[!exist],
        collapse = ","
      ))
    }

    # verificación de fda
    if(length(cols) != length(args$fda) & length(args$fda) != 1L){
      if(
        any(grepl("JUI_2017", names(args$fda))) &
        "JUI_LIQ" %in% cols &
        length(cols) == length(args$fda) - 1L
      ){
        ## modificaciones AD HOC ----
        # si pasaron fda especiales para juicios previos 2017,
        # hacemos aquí la preparacióncde datos
        
        df[,`:=`(
          JUI_LIQ_2017 = fifelse(
            MESACC <= 201712L, JUI_LIQ, 0
          )
        )]
        df[,`:=`(
          JUI_LIQ = fifelse(
            MESACC > 201712L, JUI_LIQ, 0
          )
        )]
        
        #reordeno columnas y fda
        names <- c(
          names[-match("JUI_LIQ", cols)],
          names[match("JUI_LIQ", cols)], "JUI_ULT_2017"
        )
        
        cols <- c(
          cols[-match("JUI_LIQ", cols)],
          "JUI_LIQ", "JUI_LIQ_2017"
        )
        
        args$moneda_homogenea <- c(
          args$moneda_homogenea[-match("JUI_LIQ", cols)],
          rep(args$moneda_homogenea[match("JUI_LIQ", cols)],2)
        )
        
        # creo la columna ultimate a priori de juicios 2017
        if(metodo %in% c("BF", "BF_modificado")){
          names_JUI <- paste0(names[[length(names) - 1]],"_H")
          df[, JUI_ULT_2017_H := get(names_JUI)]
        }

        args$fda <- args$fda[
          c(
            names(args$fda)[-grep("JUI", names(args$fda))],
            "FDA_liq_JUI", "FDA_liq_JUI_2017" #mejorar esta línea para que
            # sólo tome _JUI y JUI_2017
          )
        ]
        
        if(is_list(args$fct_inf) && length(args$fct_inf) != 1L){
          args$fct_inf <- c(
            args$fct_inf[-match("JUI_LIQ", cols)],
            rep(args$fct_inf[match("JUI_LIQ", cols)],2)
          )
        }

      } else {
        stop(paste0(
          "faltan factores de desarrollo para ", cols,
          collapse = ", "
        ))
      }
    }
    
    # verificación fct_inf_fut
    if(length(args$fct_inf_fut) == 1L){
      args$fct_inf_fut <- replicate(
        length(cols), args$fct_inf_fut, simplify = FALSE
      )
    }

    # verificación fct_inf
    if(is_list(args$fct_inf) == FALSE && length(args$fct_inf) == 1L){
      args$fct_inf <- replicate(
        length(cols), args$fct_inf, simplify = FALSE
      )
    }
    
    names_H <- paste0(names, "_H")
    D <- max(df$d - df$t)
    
    # completo fdas
    
    for(i in 1:length(args$fda)){
      if(length(args$fda[[i]]) < D) {
        warning("no alcanzan los fda. Se extiende con unos")
        args$fda[[i]] <- c(args$fda[[i]], rep(1, D - length(args$fda[[i]]) ))
      }
    }
    
    # replico fda
    
    if(length(args$fda) == 1L){
      args$fda <- replicate(
        length(cols), args$fda, simplify = FALSE
      )
    }
  }
  
  #### método de Chain-Ladder ----
  
  if(metodo == "CL"){
    
    # agrupo por DENUNCIA + nest
    df_valores <- copy(df)[, 
       list(data = list(.SD)),
       .SDcols = c("d", "t", cols, names_H),
       by = "DENUNCIA"
    ]
    
    # realizo el IBNER (unlist es necesario porque sino sale en forma de lista)
    for(i in 1:length(cols)){
      set(df_valores, NULL, j = names_H[i],
        value = unlist(lapply(df_valores$data, function(x){
          ultimate_DFM(
            o = x[[cols[i]]], 
            d = x[["d"]], 
            t = x[["t"]],
            fct_inf = args[["fct_inf"]],
            fda_d = args[["fda"]][[i]]
          )
        }), recursive = FALSE)
      )
    }
    
  }
  
  #### preparación método de Borhhuetter-Fergusson ----
  if(metodo %in% c("BF", "BF_modificado")){
    # chequeo que estén las columnas adicionales de ultimate esperado en el df

    
    if(metodo == "BF_modificado"){
      
      # ajusto por variable
      
      chk_cols(df, c(paste0(args[["col_ajuste"]], "_0"), names_H), stop = TRUE)
      
      col_ajuste_0 <- paste0(args[["col_ajuste"]], "_0")

      df[,
         (names_H) :=lapply(.SD, function(x) { x * 
         get(args[["col_ajuste"]]) / get(col_ajuste_0)
        }),
       .SDcols = names_H
      ]
      
    } else {
      
      chk_cols(df, names_H, stop = TRUE)      
      
    }

    # agrupo por DENUNCIA + nest
    df_valores <- copy(df)[, 
      list(data = list(.SD)),
      .SDcols = c("d", "t", cols, names_H),
      by = "DENUNCIA"
    ]
  }

  #### método de Borhhuetter-Fergusson generalizado----
  if(metodo %in% metodos_BF){
    
    # realizo el IBNER
    for(i in 1:length(cols)){
      # options(datatable.verbose = TRUE)
      
      set(
        df_valores, NULL, j = "ibner",
        value = lapply(df_valores$data, function(x){
          IBNER_DFM(
            d = x[["d"]],
            t = x[["t"]],
            fda_d = args[["fda"]][[i]],
            ult = unique(x[[names_H[i]]]),
            fct_inf_fut = args[["fct_inf_fut"]][[i]]
          )
        })
      )

      set(
        df_valores, NULL, j = names[i],
        value = mapply(
          function(x, ibner){
            ultimate_BF(
              o = x[[cols[i]]],
              d = x[["d"]],
              t = x[["t"]],
              ibner = ibner,
              fct_inf = {
                if(args[["moneda_homogenea"]][[i]] == TRUE){
                  args[["fct_inf"]]
                } else {
                  1
                } 
              }
            )
          }, 
          x = df_valores$data,
          ibner = df_valores$ibner,
          SIMPLIFY = FALSE
        )
      )
      
      df_valores[, ibner := NULL]
      
    }

    # realizo una DT con el desagregado de los valores
    df_valores <- df_valores[, 
     do.call(cbind, unname(unlist(.SD, recursive = F))),
     by = "DENUNCIA", .SDcols = names
    ][,
      .SD, .SDcols = c(1:3, seq(from= 4, by = 3, length.out = length(names)))
    ]
    # ó
    # df_valores <- df[,  
    #    unlist(unname(unlist(.SD, recursive = FALSE)), recursive = FALSE), 
    #    by = "DENUNCIA", .SDcols = names
    # ]
    # https://stackoverflow.com/questions/66557354/unnest-many-columns-using-non-standard-evaluation/66557816#66557816
    
    colnames(df_valores) <- c("DENUNCIA", "d", "t", names)
    setkey(df_valores, DENUNCIA, t)
    
    #uno con df_valores nuevamente
    # remuevo columnas ya utilizadas
    df[, c("MES", "d") := NULL]
    
    # hago el join entre df_valores y df.
    # 
    # al haber columnas LIQ|RVA|INC, una parte se une por DENUNCIA y t,
    # y hay que completarlas con ceros para t > 0
    # y otra que se combina sólo por denuncia. 
  
    cols_0 <- grep("^(ILT|ILP|ESP|JUI)_(LIQ|RVA|INC)((_2017)?)$", names(df), value = TRUE)
    cols_0_key <- c("DENUNCIA", "t", cols_0)
    cols_0_orig <- paste0(cols, "_0")
    cols_0_t <- c("t", cols_0, cols_0_orig)
    
    # 1. hago el join con columnas que se unen por DENUNCIA y t.
    df_valores <- df[,..cols_0_key][df_valores]
    
    # 2. completo con ceros las columnas que se unen por DENUNCIA y t
    df_valores[,
     (cols_0) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
     .SDcols = cols_0
    ]
    
    # 3. filtro filas en cero, pero me quedo al menos con una fila x stro (t == 0)
    # Reduce va tomando de dos en dos los elementos, aplicando sucesivamente `|`;
    # y aprovechamos que 0 | 0 es FALSE. Por tanto cuando encuentre todo cero será
    # FALSE. Adicionalmente se considera si t = 0.
    # Nota: en el paso 2 se completó primero los NA, porque NA | 0 = NA.
    setcolorder(df_valores,c("DENUNCIA", "d", "t"))
    df_valores <- df_valores[
      Reduce(`|`, df_valores[,4:ncol(df_valores)]) | t == 0,
    ]

    # 4. identifico las observaciones que se usaron para hacer ibner
    df_valores[,
       (cols_0_orig) := lapply(.SD, function(x) fifelse(t <= 0, x, 0)), 
       .SDcols = names
    ]

    # 5. hago el join con las columnas que se unen sólo por DENUNCIA.
    
    cols_0_t <- cols_0_t[cols_0_t %in% names(df)]
    df <- merge(
      df_valores,
      unique(df[,!..cols_0_t], by = "DENUNCIA"), 
      by = "DENUNCIA"
    )    
    
    # Agrego MES
    df[,
      MES := AAAAMM_diferido(MESACC, d)
    ] 
    
  }
  
  #### Sin IBNER ----
  if(metodo == "NO"){
    
    # realizo el IBNER (revisar, no está preparado para names ni cols)
    # se supone no se pasan porque no se hace IBNR
    
    df[,`:=`(
      ILT_ULT = ILT_LIQ + ILT_RVA,
      ESP_ULT = ESP_LIQ + ESP_RVA,
      ILP_ULT = ILP_LIQ + ILP_RVA,
      JUI_ULT = JUI_LIQ + JUI_RVA
    )]
    
  }
  
  #### final ----
  
  # AD-HOC: me fijo si se pasaron fda especiales de juicio por separado
  if(
    "JUI_ULT_2017" %in% names(df)
  ){

    if("JUI_LIQ_2017" %in% colnames(df)){
      df[, JUI_LIQ := JUI_LIQ + JUI_LIQ_2017]
      df[, JUI_LIQ_2017 := NULL]
    }
    if("JUI_RVA_2017" %in% colnames(df)){
      df[, JUI_RVA := JUI_RVA + JUI_RVA_2017]
      df[, JUI_RVA_2017 := NULL]
    }
    if("JUI_INC_2017" %in% colnames(df)){
      df[, JUI_INC := JUI_INC + JUI_INC_2017]
      df[, JUI_INC_2017 := NULL]
    }
    if("JUI_ULT_2017" %in% colnames(df)){
      df[, JUI_ULT := fifelse(MESACC > 201712, JUI_ULT, JUI_ULT_2017)]
      df[, JUI_ULT_2017 := NULL]
    }
    if("JUI_LIQ_2017_0" %in% colnames(df)){
      df[, JUI_LIQ_0 := fifelse(MESACC > 201712, JUI_LIQ_0, JUI_LIQ_2017_0)]
      df[, JUI_LIQ_2017_0 := NULL]
    }
    if("JUI_INC_2017_0" %in% colnames(df)){
      df[, JUI_INC_0 := fifelse(MESACC > 201712, JUI_INC_0, JUI_INC_2017_0)]
      df[, JUI_INC_2017_0 := NULL]
    }
    
    names <- names[-match("JUI_ULT_2017", names)]
    # no sacar, es parte de los datos del siniestro
    # names_H_JUI <- names_H[[length(names) - 1]]
    # df[, (names_H_JUI) := get(names_H_JUI) + JUI_ULT_2017_H]
    # names_H <- names_H[-match("JUI_ULT_2017_H", names_H)]
    
  }
  
  # reacomodo columnas
  orden_cols_finales <- c("MES", "d", "t", names, names_H)
  
  if("ILT_INC_0" %in% colnames(df)){
    df[, ILT_IBNER := ILT_ULT - ILT_INC_0]
    df[, ILT_INC_0 := NULL]
  } else {
    df[, ILT_IBNER := ILT_ULT - ILT_LIQ_0 - ILT_RVA]
  }
  
  if("ESP_INC_0" %in% colnames(df)){
    df[, ESP_IBNER := ESP_ULT - ESP_INC_0]
    df[, ESP_INC_0 := NULL]
  } else {
    df[, ESP_IBNER := ESP_ULT - ESP_LIQ_0 - ESP_RVA]
  }

  if("ILP_INC_0" %in% colnames(df)){
    df[, ILP_IBNER := ILP_ULT - ILP_INC_0]
    df[, ILP_INC_0 := NULL]
  } else {
    df[, ILP_IBNER := ILP_ULT - ILP_LIQ_0 - ILP_RVA]
  }

  if("JUI_INC_0" %in% colnames(df)){
    df[, JUI_IBNER := JUI_ULT - JUI_INC_0]
    df[, JUI_INC_0 := NULL]
  } else {
    df[, JUI_IBNER := JUI_ULT - JUI_LIQ_0 - JUI_RVA]
  }
      
  df[, `:=`(
      ILT_LIQ_0 = ILT_ULT - ILT_IBNER - ILT_RVA,       
      ESP_LIQ_0 = ESP_ULT - ESP_IBNER - ESP_RVA,
      ILP_LIQ_0 = ILP_ULT - ILP_IBNER - ILP_RVA,
      JUI_LIQ_0 = JUI_ULT - JUI_IBNER - JUI_RVA
  )]

  setcolorder(
    df,
    c(
      setdiff(names(df), orden_cols_finales),
      orden_cols_finales
    )
  )
  
  return(df)
  
}

