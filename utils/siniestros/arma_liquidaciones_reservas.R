#' arma_liquidaciones_reservas
#'
#' Incorpora importes liquidados mensuales a siniestros.
#' opción: Reclasifica ILP por fecha juicio
#' opción: Agrega última reserva o variación de reservas
#'
#' @param siniestros           data.frame tabla de siniestros
#' @param liquidaciones        data.frame tabla de liquidaciones
#' @param agrega_reserva       character  se agregan reservas
#'                                        "actual" última
#'                                        "hist" variación histórica
#' @param presentar_reservas   logical    se muestran columnas de reserva y 
#'                                        reserva anterior. Obligatorio si
#'                                          @agrega_reserva = "hist"
#' @param ILP_x_notif_juicio   logical    se reclasifica ILP por 1ra notif juicio
#' @param juicios              data.frame juicios. Obligatorio si
#'                                          @ILP_x_notif_juicio = T
#' @param tipos_juicios        integer    vector con los tipos de juicio que se 
#'                                        consideran juicio.
#'                                        default = c(1, 2, 5)
#' @param reservas             data.frame reservas. Obligatorio si se utiliza
#'                                          @agrega_reserva
#' @param mes_corte            integer    mes de corte reservas. Obligatorio si
#'                                        se utiliza  @agrega_reserva
#'
#' @return
#' @export
#'
#' @note
#' En caso de no haber liquidaciones para un siniestro, se agrega
#' liquidaciones en cero para el último mes liquidado. Si todo el grupo no
#' tiene liquidaciones, se agrega el mes de accidente.
#'
#' Cuando se elige @agrega_reserva = "hist", se devuelve las variaciones de
#' reserva significativas, incluyendo la caída de reserva negativa luego de
#' finalizado el siniestro (siempre que no coincida con el mes_corte)
#' @examples
#'
arma_liquidaciones_reservas <- function(
  siniestros,
  liquidaciones,
  agrega_reserva = FALSE,
  presentar_reservas = FALSE,
  ILP_x_notif_juicio = FALSE,
  juicios = NULL,
  tipos_juicios = c(1, 2, 5),
  reservas = NULL,
  mes_corte = NULL
){

  # funciones compartidas por métodos ----

  # bit juicio
  BIT_JUI.f <- function(
    NOTIF, MES
  ){
    # as.integer(NOTIF < MES)
    as.integer(NOTIF <= MES)
  }

  # completar columnas con cero
  colpleta_0 <- function(x, cols){
    for(j in which(colnames(x) %in% cols)){
      if (is.numeric(x[[j]]) )
        set(x, i = which(is.na(x[[j]])), j = j, value = 0)
    }
  }

  UseMethod("arma_liquidaciones_reservas")
}

arma_liquidaciones_reservas.data.frame <- function(
  siniestros,
  liquidaciones,
  agrega_reserva = FALSE,
  presentar_reservas = FALSE, 
  ILP_x_notif_juicio = FALSE,
  juicios = NULL,
  tipos_juicios = c(1, 2, 5),
  reservas = NULL,
  mes_corte = NULL
){

  warning(
    "cuidado, se está usando arma_liquidaciones_reservas para data.frames 
     y no se terminó de revisar "
  )
  # verificaciones ----

  if(agrega_reserva %in% c("actual", "hist")){
    if(is_null(reservas)) stop(
      "Error en siniestros_liquidados: falta argumento reservas"
    )
    if(is_null(mes_corte)) stop(
      "Error en siniestros_liquidados: falta argumento mes_corte de reservas"
    )
    # renombramientos ----
    reservas <- reservas %>%
      rename_with(~gsub("IND_", "ILP_", .x)) %>%
      rename_with(~gsub("MES_RVA", "MES", .x))

  }else{
    if(!is_null(agrega_reserva)) stop(paste0(
      "Error en siniestros_liquidados: argumento agrega_reserva ",
      "sólo puede valer uno de los elementos {actual, hist, NULL}"
    ))
  }

  # renombramientos ----
  liquidaciones <- liquidaciones %>%
    rename_with(~gsub("IND_", "ILP_", .x)) %>%
    rename_with(~gsub("MESLIQ", "MES", .x))

  # inicio con base siniestros
  df <- siniestros %>% filter(
    MESACC <= mes_corte
  )

  # aviso si se filtraron siniestros
  if(nrow(df) < nrow(siniestros)) warning(
    "arma_liquidaciones_reservas: se filtraron denuncias con MESACC > mes_corte"
  )
#  rm(siniestros)

  # agrego columna con la primer notificación de juicio si se quiere
  # separar ILP y JUICIOS por fecha de notificación
  if(ILP_x_notif_juicio == TRUE){
    if(is_null(juicios)) stop(
      "Error en siniestros_liquidados: falta argumento juicios"
    )
    df <- df %>%
      left_join(
        juicios %>%
          filter(
            # filtro juicios ordinarios
            # TIPO_JUICIO %in% c(1, 2, 5)
            TIPO_JUICIO %in% tipos_juicios
          ) %>%
          mutate(
            AAAAMM_NOTIF_JUICIO = year(F_NOTIFICACION_JUICIO) * 100 +
              month(F_NOTIFICACION_JUICIO)
          ) %>%
          arrange(
            DENUNCIA, AAAAMM_NOTIF_JUICIO
          ) %>%
          group_by(
            DENUNCIA
          ) %>%
          slice(1L) %>%
          select(
            DENUNCIA, AAAAMM_NOTIF_JUICIO
          ) %>%
          ungroup(),
        by = "DENUNCIA"
      ) %>%
      mutate(
        AAAAMM_NOTIF_JUICIO = replace_na(
          AAAAMM_NOTIF_JUICIO, 209901L
        )
      )
  }

  # inserto reservas si se las pide
  if(!is_null(agrega_reserva)){

    # filtro posibles liquidaciones previas a la reserva
    liquidaciones <- liquidaciones %>%
      filter(
        MES <= mes_corte
      )

    if(agrega_reserva == "actual"){

      # inserto última reserva
      df <- df %>%
        left_join(
          reservas %>%
            filter(
              MES == mes_corte
            ) %>%
            select(
              DENUNCIA, ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA
            ),
          by = "DENUNCIA"
        )

      # inserto liquidaciones
      df <- df %>%
        left_join(
          liquidaciones %>%
            select(
              DENUNCIA, MES, ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ
            ),
          by = "DENUNCIA"
        )

    }else{

      # completo reservas
      reservas <- reservas %>%
        mutate(
          yearRVA = MES %/% 100L,
          monthRVA = MES %% 100L,
        ) %>%
        group_by(DENUNCIA) %>%
        complete(
          yearRVA = min(yearRVA):(mes_corte %/% 100L),
          monthRVA = 1L:12L
        ) %>%
        mutate(
          MES = yearRVA * 100L + monthRVA
        ) %>%
        filter(
          MES >= min(MES) & MES <= mes_corte
        ) %>%
        ungroup() %>%
        select(
          DENUNCIA, MES, ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA
        )

      rvas_liq <- full_join(
        reservas,
        liquidaciones %>%
          select(
            DENUNCIA, MES, ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ
          ),
        by = c("DENUNCIA", "MES")
      )

      df <- df %>%
        left_join(
          rvas_liq, by = "DENUNCIA"
        )
    }

  }else{
    # inserto liquidaciones
    df <- df %>%
      left_join(
        liquidaciones %>%
          select(
            DENUNCIA, MES, ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ
          ),
        by = "DENUNCIA"
      )
  }

  # completo columnas ----
  df <- df %>%
    mutate(
      across(
        c(ends_with("_RVA"), ends_with("_LIQ")),
        ~ replace_na(., 0)
      )
    )

  # obtengo último mes liquidado
  if(is_null(agrega_reserva)){
    if(nrow(df) != 0L)
      mes_corte <- max(df$MES, na.rm = TRUE)
    else
      mes_corte <- 209901L
  }

  # relleno mes
  # df <- df %>% mutate(
  #   MES = replace_na(MES, !!mes_corte)
  # )
  # cambio el tipo de relleno

  df <- df %>% mutate(
    MES = if_else(
      is.na(MES),
      MESACC,
      MES
    )
  )


  # en caso de separar ILP y JUICIOS por fecha de notificación
  if(ILP_x_notif_juicio){

    # las liquidaciones siempre las migro
    df <- df %>%
      mutate(
        BIT_JUI = BIT_JUI.f(AAAAMM_NOTIF_JUICIO, MES),
        ILPJUI_LIQ = ILP_LIQ + JUI_LIQ,
        ILP_LIQ = (1 - BIT_JUI) * ILPJUI_LIQ,
        JUI_LIQ = BIT_JUI * ILPJUI_LIQ,
        ILPJUI_LIQ = NULL
      )

    if(agrega_reserva == "hist"){
      df <- df %>%
        mutate(
          ILPJUI_RVA = ILP_RVA + JUI_RVA,
          ILP_RVA = (1 - BIT_JUI) * ILPJUI_RVA,
          JUI_RVA = BIT_JUI * ILPJUI_RVA,
          ILPJUI_RVA = NULL
        )
    }

    # para reserva actual, dejo la reserva en el lado actual
    if(agrega_reserva == "actual"){
      df <- df %>%
        mutate(
          BIT_JUI2 = BIT_JUI.f(AAAAMM_NOTIF_JUICIO, mes_corte),
          ILPJUI_RVA = ILP_RVA + JUI_RVA,
          ILP_RVA = (1 - BIT_JUI2) * ILPJUI_RVA,
          JUI_RVA = BIT_JUI2 * ILPJUI_RVA,
          ILPJUI_RVA = NULL,
          BIT_JUI2 = NULL
        )
    }

  }

  # en el caso de reservas históricas, dejo las diferencias
  if(agrega_reserva == "hist"){

    tipo <- class(df)
    if(!any(tipo %in% "data.table"))
      setDT(df)

    nm1 <- grep("_RVA$", colnames(df), value=TRUE)
    nm2 <- paste0(nm1, "_lag")
    df[, (nm2) := shift(.SD, fill = 0), by=DENUNCIA, .SDcols=nm1]

    if(!any(tipo %in% "data.table"))
      df <- as_tibble(df)
    
    if(presentar_reservas == TRUE){
      df <- df %>%
        mutate(
          ILT_RVA_var = ILT_RVA - ILT_RVA_lag,
          ESP_RVA_var = ESP_RVA - ESP_RVA_lag,
          ILP_RVA_var = ILP_RVA - ILP_RVA_lag,
          JUI_RVA_var = JUI_RVA - JUI_RVA_lag
        )
    } else {
      df <- df %>%
        mutate(
          ILT_RVA = ILT_RVA - ILT_RVA_lag,
          ESP_RVA = ESP_RVA - ESP_RVA_lag,
          ILP_RVA = ILP_RVA - ILP_RVA_lag,
          JUI_RVA = JUI_RVA - JUI_RVA_lag
        ) %>%
        select(
          -ends_with("_lag")
        )
    }

  }

  return(df)

}

arma_liquidaciones_reservas.data.table <- function(
  siniestros,
  liquidaciones,
  agrega_reserva = FALSE,
  presentar_reservas = FALSE, 
  ILP_x_notif_juicio = FALSE,
  juicios = NULL,
  tipos_juicios = c(1, 2, 5),
  reservas = NULL,
  mes_corte = NULL
){

  precision <- 1e-10
  # verificaciones ----

  if(agrega_reserva %in% c("actual", "hist")){
    if(is_null(reservas)) stop(
      "Error en arma_liquidaciones_reservas: falta argumento reservas"
    )
    if(is_null(mes_corte)) stop(
      "Error en arma_liquidaciones_reservas: falta argumento mes_corte de reservas"
    )
    # renombramientos
    setnames(
      reservas,
      c("IND_RVA", "MES_RVA"),
      c("ILP_RVA", "MES"),
      skip_absent = TRUE
    )

    # selección
    # aprovecho y filtro las reservas que no están en el listado de siniestros
    reservas <- reservas[
      siniestros, 
      .(DENUNCIA, MES, ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA),
      nomatch = 0,  # inner join
      on = .(DENUNCIA) # no es necesario porque se fijó key, pero es mas claro así
    ]

  }else{
    if(!is_null(agrega_reserva)) stop(paste0(
      "Error en siniestros_liquidados: argumento agrega_reserva ",
      "sólo puede valer uno de los elementos {actual, hist, NULL}"
    ))
  }

  # renombramientos

  setnames(
    liquidaciones,
    c("IND_LIQ", "MESLIQ"),
    c("ILP_LIQ", "MES"),
    skip_absent = TRUE
  )
  
  # selección
  # aprovecho y filtro las liquidaciones que no estpan en el listado de siniestros
  liquidaciones <- liquidaciones[
    siniestros,
    .(DENUNCIA, MES, ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ),
    nomatch = 0,
    on = .(DENUNCIA)
  ]

  # agrego columna con la primer notificación de juicio si se quiere
  # separar ILP y JUICIOS por fecha de notificación
  if(ILP_x_notif_juicio){
    if(is_null(juicios)) stop(
      "Error en siniestros_liquidados: falta argumento juicios"
    )

    setnames(
      juicios,
      "MES_NOTIFICACION_JUICIO",
      "AAAAMM_NOTIF_JUICIO",
      skip_absent = TRUE
    )

    juicios <- juicios[
      # TIPO_JUICIO %in% c(1, 2, 5),
      TIPO_JUICIO %in% tipos_juicios,
      .(DENUNCIA, AAAAMM_NOTIF_JUICIO)
    ]

    setkey(juicios, DENUNCIA, AAAAMM_NOTIF_JUICIO)

    # tomo la primer fila de cada juicio, por denuncia.
    # el argumento interno es un filtro, I[n] da retorma el índice en forma
    # de variable V1. Luego lo usamos como filtro en el df[] contenedor
    juicios <- juicios[
      juicios[,.I[1], by = DENUNCIA]$V1
    ]

    siniestros <- juicios[siniestros[MESACC <= mes_corte,],
      on = .(DENUNCIA), allow.cartesian = TRUE
    ][,
     `:=`(
      AAAAMM_NOTIF_JUICIO = replace_na(AAAAMM_NOTIF_JUICIO, 209901L)
     )
    ]

  }

  df <- siniestros

  # inserto reservas si se las pide
  if(!is_null(agrega_reserva)){

    # filtro posibles liquidaciones previas a la reserva
    liquidaciones <- liquidaciones [MES <= mes_corte]

    if(agrega_reserva == "actual"){

      # inserto última reserva
      df <- reservas[
        MES == mes_corte, !"MES"
      ][df, on = .(DENUNCIA), allow.cartesian = TRUE]

      # inserto liquidaciones
      df <- liquidaciones[df, on = .(DENUNCIA), allow.cartesian = TRUE]

    } else {

      # combino liquidaciones y reservas mensuales
      rvas_liq <- merge(
        reservas,
        liquidaciones,
        all = TRUE,
        by = c("DENUNCIA", "MES"),
        allow.cartesian = TRUE,
        sort = TRUE
      )

      rvas_liq <- completa_huecos_MES(
        rvas_liq, 
        MES,
        mes_max = mes_corte
      )
      
      # inserto liquidaciones
      df <- rvas_liq[df, on = .(DENUNCIA), allow.cartesian = TRUE]
    
    }

  } else {
    # inserto liquidaciones
    df <- liquidaciones[df, on = .(DENUNCIA), allow.cartesian = TRUE]

  }

  # completo columnas ----

  # obtengo último mes liquidado
  if(is_null(agrega_reserva)){
    if(nrow(df) != 0L)
      mes_corte <- max(df$MES, na.rm = TRUE)
    else
      mes_corte <- 209901L
  }

  # relleno mes
  # set(df, i = which(is.na(df[["MES"]])), j = "MES", value = mes_corte)
  # cambio el tipo de relleno
  df[is.na(MES), MES := MESACC]

  # relleno *_LIQ y *_RVA
  colpleta_0(
    df,
    grep("[^?!MES]_((RVA)|(LIQ))$", colnames(df), value = T)
  )

  # en caso de separar ILP y JUICIOS por fecha de notificación
  if(ILP_x_notif_juicio){

    # las liquidaciones siempre las migro
    df[, `:=`(
      BIT_JUI = BIT_JUI.f(AAAAMM_NOTIF_JUICIO, MES),
      ILPJUI_LIQ = ILP_LIQ + JUI_LIQ
    )]
    df[, `:=`(
      ILP_LIQ = (1 - BIT_JUI) * ILPJUI_LIQ,
      JUI_LIQ = BIT_JUI * ILPJUI_LIQ
    )]
    df[, "ILPJUI_LIQ" := NULL]


    if(agrega_reserva == "hist"){
      df[, `:=`(
        BIT_JUI = BIT_JUI.f(AAAAMM_NOTIF_JUICIO, MES),
        ILPJUI_RVA = ILP_RVA + JUI_RVA
      )]
      df[, `:=`(
        ILP_RVA = (1 - BIT_JUI) * ILPJUI_RVA,
        JUI_RVA = BIT_JUI * ILPJUI_RVA
      )]
      df[, "ILPJUI_RVA" := NULL]

    }

    # para reserva actual, dejo la reserva en el lado actual
    if(agrega_reserva == "actual"){
      df[, `:=`(
        BIT_JUI2 = BIT_JUI.f(AAAAMM_NOTIF_JUICIO, mes_corte),
        ILPJUI_RVA = ILP_RVA + JUI_RVA
      )]
      df[, `:=`(
        ILP_RVA = (1 - BIT_JUI2) * ILPJUI_RVA,
        JUI_RVA = BIT_JUI2 * ILPJUI_RVA
      )]
      df[, c("ILPJUI_RVA", "BIT_JUI2") := NULL]

    }

  }

  # en el caso de reservas históricas, dejo las diferencias
  if(agrega_reserva == "hist"){

    nm1 <- grep("_RVA$", colnames(df), value=TRUE)
    nm2 <- paste0(nm1, "_lag")
    df[, (nm2) := shift(.SD, fill = 0), by = DENUNCIA, .SDcols=nm1]

    if(presentar_reservas == TRUE){
      df[, `:=`(
        ILT_RVA_var = round(ILT_RVA - ILT_RVA_lag, 2),
        ESP_RVA_var = round(ESP_RVA - ESP_RVA_lag, 2),
        ILP_RVA_var = round(ILP_RVA - ILP_RVA_lag, 2),
        JUI_RVA_var = round(JUI_RVA - JUI_RVA_lag, 2)
      )]
    }
    else {
      df[, `:=`(
        ILT_RVA = round(ILT_RVA - ILT_RVA_lag, 2),
        ESP_RVA = round(ESP_RVA - ESP_RVA_lag, 2),
        ILP_RVA = round(ILP_RVA - ILP_RVA_lag, 2),
        JUI_RVA = round(JUI_RVA - JUI_RVA_lag, 2)
      )]
      df[, grep("_lag$", colnames(df)) := NULL]
    }

    # unused ----
    # error sacar variaciones nulas. La reserva existe al mes siguiente 
    # filtro vacíos - unused 
    # cols_to_sum <- df[,grep("[^?!MES]_((RVA)|(LIQ))$", colnames(df))]
    # df$TOT_INC <- rowSums(abs(df[,..cols_to_sum]))
    # df <- df[abs(TOT_INC) > precision]
    # df[, TOT_INC := NULL]
    # ----

  }

  return(df)

}
