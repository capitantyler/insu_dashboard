#' genera_reportes_siniestros
#'
#' Se retorna un listado de reportes
#'    (rpt0): detalle mensual de siniestros con variables, liquidado, RSP, IBNER, Ult.
#'    (rpt1): agrupado por siniestros con variables, liquidado, RSP, IBNER, Ult.
#'    (rpt2): agrupado por CONTRATO y MES accidente
#'              (liquidado, RSP, IBNR, IBNER, Ultimate)
#'    (rpt3): agrupado por CONTRATO y periodo accidente anualizado
#'              (liquidado, RSP, IBNR, IBNER, Ultimate)
#'    (rpt4): agrupado por DENUNCIA y período contable anualizado
#'              (liquidado, Variación de Rvas y devengado)
#'    (rpt5): agrupado por CONTRATO y período contable anualizado
#'              (liquidado, Variación de Rvas y devengado)
#'    (rpt6): estadísticas por CONTRATO, Estado-consecuencia 
#'               y período anual. Incluye IBNR
#'
#' @param reportes     character list, lista "rpt{0-6}" con los reportes deseados
#' @param siniestros    df o dt,   siniestros e info asociada
#' @param liquidaciones df o dt,  liquidaciones por DENUNCIA y MES
#' @param reservas      df o dt,  reservas por DENUNCIA y MES
#' @param juicios       df o dt,  juicios
#' @param contratos     df o dt,  contratos
#' @param factorContrato  character,  factorde agrupamiento. No se usa.
#' @param mesMin       integer, mes de accidente en que inician los reportes
#' @param mesMax       integer, mes de accidente en que finalizan los reportes
#' @param mes_cierre   integer, último mes datos disponible.
#' si mes_cierre = NULL, se asume que el último mes disponible es mesMax
#' @param cierreDeMes  integer, nro de mes en que se hace el rolling sum.
#'                     para ejercicio económico, usar 6 (junio)
#' @param ILP_x_notif_juicio   logical, bit de si se separa o no liquis por
#'                                      notificació JUI.
#' @param metodo_IBNER  character  método IBNER seleccionado (default = "NO")
#' @param ibnr_puro  tabla con los valores de ibner por período
#'                    CONTRATO: número de contrato
#'                    PER: integer periodo
#'                    N_IBNR: número de siniestros IBNR
#'                    ILT_IBNR: valor ultimate ILT de siniestros IBNR
#'                    ESP_IBNR: valor ultimate ESP de siniestros IBNR
#'                    ILP_IBNR: valor ultimate ILP de siniestros IBNR
#'                    JUI_IBNR: valor ultimate JUI de siniestros IBNR
#' @param denuncias_x_cluster  integer  Número de siniestros por iteración
#' de clúster
#' @param ...        parámetros de los métodos de IBNER
#'
#' @return
#' @export
#'
#' @note
#' Si mes_cierre = NULL, se asume que el último mes disponible es mesMax
#'
#' TODO: sustituir IBNR_siniestros_tasa por los valores pasados en ibnr_puro.
#' TODO: factorContrato no tiene definido cómo devolver los reportes si
#' es distinto de "CONTRATO".
#'
#' @examples

genera_reportes_siniestros <- function(
  reportes = c("rpt0", "rpt1", "rpt2", "rpt3", "rpt4", "rpt5", "rpt6"),
  siniestros,
  liquidaciones = NULL,
  reservas = NULL,
  juicios,
  contratos,
  mesMin,
  mesMax,
  mes_cierre = NULL,
  cierreDeMes = 12L,
  ILP_x_notif_juicio = TRUE,
  factorContrato = "CONTRATO", # no se está usando
  metodo_IBNER = "NO",
  ibnr_puro = NULL, # tabla con los valores de Q y valores por PERíodo y CTO
  denuncias_x_cluster = 10000,
  ...  # argumentos asociados a funciones externas de ibner e inflación
){
  
  # Librerías ----
  
  library(assertthat, quietly = T)
  library(tidyverse, quietly = T)
  library(lubridate, quietly = T)
  library(readxl, quietly = T)
  
  # Parámetros por default ----
  reportes <- match.arg(reportes, several.ok = TRUE)
  args <- list(...)
  metodos_BF <- c("CL", "BF", "BF_modificado")
  
  # constantes ----
  
  # funciones ----
  fx <- c(
    "%!in%", "AAAAMM_diferido", "AAAAMM_ejercicio", "chk_cols"
  )
  
  # assert_that. Produce el sig error
  # Error in gregexpr(calltext, singleline, fixed = TRUE) : 
  #  regular expression is invalid UTF-8
  assert_that(
    {
      exist <- sapply(fx, function(x) exists(x, mode = "function"))
      all(exist) == TRUE
    },
    msg = paste(
      "faltan cargar alguna las siguientes funciones auxiliares en genera_reportes_siniestros.R: ",
      paste(fx[!exist], collapse = "\n"),
      collapse = "\n"
    )
  )
  
  if(metodo_IBNER == "NO" & any(c(6, "rpt6") %in% reportes)){
    if(!exists("IBNR_siniestros_tasa", mode = "function")) stop(
      "genera_reportes_siniestros: faltan cargar la función auxiliar IBNR_siniestros_tasa"
    )
  }
  
  # chequeo argumentos ----
  
  if (cierreDeMes > 12L) {
    stop("El argumento cierreDeMes debe ser un mes entre 1 y 12")
  }
  
  # se asume que el último mes disponible es mesMax
  if (is.null(mes_cierre)) {mes_cierre <- mesMax}
  
  ## filtros de siniestros, liquidaciones y reservas ----
  
  # aplicacion filtro siniestros. Sólo filtro si no es un reporte contable
  if(!all(reportes %in% c(4, "rpt4", 5, "rpt5"))){
    siniestros <- siniestros[
      which(
        siniestros$MESACC >= mesMin &
          siniestros$MESACC <= mesMax
      ), ]
  }

  # aplicacion filtro liquidaciones
  liquidaciones <- liquidaciones[
    which(
      liquidaciones$MESLIQ >= mesMin &
        liquidaciones$MESLIQ <= mes_cierre
    ), ]
  
  # decido si saco reportes con reservas históricas o actuales (performance).
  flag_rvas_hist_actual <- if_else(
    reportes %in% c(4, "rpt4", 4, "rpt5") ||
      # condición tener algún método de IBNER que aplique en columnas incurridas
      (
        any(reportes %in% c(0, 1, 2, 3, "rpt0", "rpt1", "rpt2", "rpt3")) & 
          any(grepl("_INC", args$cols_fda))
      ),
    "hist", 
    "actual"
  )  
  
  # aplicacion filtro reservas
  if(flag_rvas_hist_actual != "hist"){
    reservas <- reservas[
      which(
        reservas$MES >= mesMin &
          reservas$MES <= mes_cierre
      ), ]
  } else {
    # sólo si saco unicamente rpt4/5
    # mes_cierre <- mesMax  # no necesito reservas ni IBNER
    reservas <- reservas[
      which(
        reservas$MES >= AAAAMM_diferido(mesMin, -1L) &
          reservas$MES <= mes_cierre
      ), ]
  }
  
  ## factor de agrupamiento (sin usarse)----
  
  # if (factorContrato != "CONTRATO"){
  #   if (is.null(factorContrato)) {
  #     warning("sin factor, agrupando por contrato")
  #     factor <- "CONTRATO"
  #   } else {
  #     factor <- names(factorContrato)[
  #       grep("CONTRATO", names(factorContrato), invert = T)
  #     ]
  #
  #     siniestros <- left_join (
  #       siniestros,
  #       factorContrato,
  #       by = "CONTRATO"
  #     )
  #
  #     reservas <- left_join (
  #       reservas,
  #       factorContrato,
  #       by = "CONTRATO"
  #     )
  #
  #     liquidaciones <- left_join (
  #       liquidaciones,
  #       factorContrato,
  #       by = "CONTRATO"
  #     )
  #
  #     if(any(reportes %in% c(6,7, "rpt6", "rpt7"))){
  #       juicios <- left_join (
  #         juicios,
  #         factorContrato,
  #         by = "CONTRATO"
  #       )
  #     }
  #   }
  # } else {
  #   factor <- "CONTRATO"
  # }
  # factor <- sym(factor)
  #----
  
  # chequeo argumentos de métodos de IBNR
  if(metodo_IBNER != "NO"){
    if(metodo_IBNER %in% metodos_BF){
      
      lista_args <- c("cols_fda", "fda", "fct_inf_fut", "moneda_homogenea")

      # agrego chequeo de fct_inf en dos casos:
      if(any(args$moneda_homogenea) == TRUE || metodo_IBNER == "CL"){
        lista_args <- c(lista_args, "fct_inf")
      }
      if(metodo_IBNER == "BF_modificado"){
        lista_args <- c(lista_args, "col_ajuste")
      }
      
      if(!any(lista_args %in% names(args))){
        stop(paste0(
          "faltan los argumentos para el método de IBNER ",
          metodo_IBNER, " ",
          paste(args[!(lista_args %in% names(args))], collapse = ",")
        ))
      }
      
    }
  }

  # combino reservas y liquidaciones, organizo juicios y calculo variaciones de rva.
  stros_liqui_rvas <- arma_liquidaciones_reservas(
    # en el argumento siniestros pueden agregarse columnas de contrato
    # (ej CLIENTE, CUIT, SUC, etc).
    siniestros = siniestros,
    liquidaciones = liquidaciones,
    agrega_reserva = flag_rvas_hist_actual,
    ILP_x_notif_juicio = ILP_x_notif_juicio,
    juicios = juicios,
    reservas = reservas,
    mes_corte = mes_cierre
  )

  # quito mes adicional si es "hist"
  if(flag_rvas_hist_actual == "hist"){
    stros_liqui_rvas <- stros_liqui_rvas[stros_liqui_rvas$MES >= mesMin,]
  }
  
  if(
    any(reportes %in% c(0, 1, 2, 3, "rpt0", "rpt1", "rpt2", "rpt3")) &&
    metodo_IBNER %in% metodos_BF &&
    any(grepl("_INC", args$cols_fda))
  ){
    # genero liquidaciones y reservas
    # chequeo si hay alguna columna con siniestralidad incurrida
    # para saber si se usa el parámetro de agrega_reserva "hist",
    # que es más costoso en tiempos.
    # agrego $*_INC. Escrito en R base para no perder class
    stros_liqui_rvas[["ILT_INC"]] <- stros_liqui_rvas[["ILT_LIQ"]] + 
      stros_liqui_rvas[["ILT_RVA"]]
    stros_liqui_rvas[["ESP_INC"]] <- stros_liqui_rvas[["ESP_LIQ"]] + 
      stros_liqui_rvas[["ESP_RVA"]]
    stros_liqui_rvas[["ILP_INC"]] <- stros_liqui_rvas[["ILP_LIQ"]] + 
      stros_liqui_rvas[["ILP_RVA"]]
    stros_liqui_rvas[["JUI_INC"]] <- stros_liqui_rvas[["JUI_LIQ"]] + 
      stros_liqui_rvas[["JUI_RVA"]]
    
    # falta filtro para eliminar incurridos negativos
    
  }

  if(metodo_IBNER != "NO"){
    
    if(length(unique(stros_liqui_rvas$DENUNCIA)) < denuncias_x_cluster){

      stros_liqui_rvas <- do.call(
        completa_periodos_IBNER,
        args = c(
          list(
            df = stros_liqui_rvas,
            mes_cierre = mes_cierre,
            metodo = metodo_IBNER,
            cols = args$cols_fda,
            moneda_homogenea = args$moneda_homogenea,
            names = args$cols_names_fda
          ),
          args
        )
      )
      
    } else {
      
      clusters <- split(
        stros_liqui_rvas$DENUNCIA, 
        ceiling(seq_along(stros_liqui_rvas$DENUNCIA)/denuncias_x_cluster)
      )
      n_cluster <- length(clusters)
      df_list <- vector(mode = "list", length = n_cluster)

      # separo de stros_liqui_rvas las columnas relevantes para completa_periodos_IBNER,
      cols_por_denuncia_MES <- grep(
        "^(ILT|ILP|ESP|JUI)_(LIQ|RVA|INC)((_2017)?)$", 
        names(stros_liqui_rvas), 
        value = TRUE
      )

      cols_completa_periodos_IBNER <- c(
        "DENUNCIA", "MESACC", "MES", # llave
        "SALARIO_EMI", "SALARIO_EMI_0",
        unique(
          c(
            cols_por_denuncia_MES,
            args$cols_fda,
            paste0(args$cols_names_fda, "_H")
          )
        )
      )
      
      for(cluster in seq_along(clusters)){
        df_list[[cluster]] <- do.call(
          completa_periodos_IBNER,
          args = c(
            list(
              df = stros_liqui_rvas[
                DENUNCIA %in% clusters[[cluster]],
                ..cols_completa_periodos_IBNER
              ],
              mes_cierre = mes_cierre,
              metodo = metodo_IBNER,
              cols = args$cols_fda,
              moneda_homogenea = args$moneda_homogenea,
              names = args$cols_names_fda
            ),
            args
          )
        )
        # message(paste0(
        #   "completa_periodos_IBNER: ",
        #   " clúster ", cluster, " de ", n_cluster, " realizado."
        # ))
      }

      # forma anterior. Requere mucha memoria
      # stros_liqui_rvas <- rbindlist(df_list)
      # setkey(stros_liqui_rvas, DENUNCIA, MES)
      
      # esta línea puede llegar a consumir mucha memoria aún.
      # probar hacerlo recursivo de a grupos
      if(object.size(df_list) > 20000 * 1048576){
        memory_limit_0 <- memory.limit()
        memory.limit(40000)
        df_list <- rbindlist(df_list)
        memory.limit(memory_limit_0)
      } else {
        df_list <- rbindlist(df_list)
      }
      
      setkey(df_list, DENUNCIA, MES)
      
      cols_por_denuncia <- c(
        "DENUNCIA",
        setdiff(
          setdiff(
            names(stros_liqui_rvas),
            names(df_list)
          ),
          cols_por_denuncia_MES
        )
      )

      ## agrego las columnas que unen sólo por DENUNCIA
      stros_liqui_rvas <- unique(stros_liqui_rvas[,
        ..cols_por_denuncia
      ])
      setkey(stros_liqui_rvas, DENUNCIA)
        
      stros_liqui_rvas <- stros_liqui_rvas[df_list, allow.cartesian=TRUE]
      
      rm(df_list)
      
    }
    
  } else {

    # parche: debería entrar en la función completa_periodos_IBNER 
    # con metodo_IBNER == "NO"
    stros_liqui_rvas[,`:=`(
      d = (MES %/% 100L - MESACC %/% 100L) * 12L + (MES %% 100L - MESACC %% 100L),
      t = (MES %/% 100L - mes_cierre %/% 100L) * 12L +
        (MES %% 100L - mes_cierre %% 100L),
      ILT_INC = ILT_LIQ + ILT_RVA,
      ESP_INC = ESP_LIQ + ESP_RVA,
      ILP_INC = ILP_LIQ + ILP_RVA,
      JUI_INC = JUI_LIQ + JUI_RVA
    )]
    
    stros_liqui_rvas[,`:=`(
      ILT_IBNER = 0,
      ESP_IBNER = 0,
      ILP_IBNER = 0,
      JUI_IBNER = 0,
      ILT_ULT = ILT_INC,
      ESP_ULT = ESP_INC,
      ILP_ULT = ILP_INC,
      JUI_ULT = JUI_INC
    )]
    
  }

  # Columnas de siniestros, a remover de los reportes por contrato.
  # Recordar que siniestros, puede venir con datos contrato.
  cols_agrupar <- unique(c(
    "CONTRATO", 
    names(siniestros), 
    "d", "t", "MES"
    #"d", "t", "MES", "BIT_JUI", "AAAAMM_NOTIF_JUICIO"
  ))

  cols_sumar <- setdiff(
    names(stros_liqui_rvas), 
    cols_agrupar
  )
  
  
  ## ** rpt0 ----
  
  if (any(c(0,"rpt0") %in% reportes)) {
    
    rpt0 <- arma_siniestros_rpt0(
      copy(stros_liqui_rvas),
      cierreDeMes
    )
    
  }
  
  
  ## ** rpt1 ----
  
  if (any(c(1,"rpt1") %in% reportes)) {
    
    # me quedo con las columnas definidas en siniestro, pertenecientes a
    # contratos. Aseguro exista DENUNCIA y MESACC

    cols_agrupar_rpt1 <- unique(
      c(
        names(contratos),
        names(siniestros)
        # names(siniestros), "AAAAMM_NOTIF_JUICIO"
      )
    )

    rpt1 <- arma_siniestros_rpt_agrupa_pesos(
      stros_liqui_rvas[contratos, on = .(CONTRATO), nomatch = NULL],
      cierreDeMes,
      columnas_agrupar = cols_agrupar_rpt1,
      columnas_sumar = cols_sumar
    )    

  }
  
  ## ** rpt2 ----
  # importe agrupado por CONTRATO y MESACC (liquidado, RSP e incurrido)
  
  if (any(c(2,"rpt2") %in% reportes)) {
    
    # me quedo con las columnas definidas en siniestro, pertenecientes a
    # contratos. Aseguro exista MESACC
    cols_agrupar_rpt2 <- unique(
      c(
        intersect(cols_agrupar, names(contratos)), 
        "MESACC"
      )
    )
    rpt2 <- arma_siniestros_rpt_agrupa_pesos(
      copy(stros_liqui_rvas),
      cierreDeMes,
      columnas_agrupar = cols_agrupar_rpt2,
      columnas_sumar = cols_sumar
    )
    
  }
  
  ## ** rpt3 ----
  # importe agrupado por CONTRATO y  PERIODO anual (liquidado, RSP e incurrido)
  
  if (any(c(3,"rpt3") %in% reportes)) {
    
    # me quedo con las columnas definidas en siniestro, pertenecientes a
    # contratos. 
    cols_agrupar_rpt3 <- unique(
      c(
        "CONTRATO", "PER",
        intersect(cols_agrupar, names(contratos))
      )
    )
    
    rpt3 <- arma_siniestros_rpt3(
      copy(stros_liqui_rvas),
      cierreDeMes,
      columnas_agrupar = cols_agrupar_rpt3,
      columnas_sumar = cols_sumar
    )
    
  }
  
  ## ** rpt4 ----
  # importe contable agrupado por DENUNCIA y período mensual (liquidado, varRSP y devengado)
  
  if (any(c(4,"rpt4") %in% reportes)) {
    
    rpt4 <- arma_siniestros_rpt4(
      copy(stros_liqui_rvas),
      cierreDeMes,
      columnas_agrupar = cols_agrupar,
      columnas_sumar = cols_sumar
    )
    
  }
  
  ## ** rpt5 ----
  # importe contable agrupado por CONTRATO y período mensual (liquidado, varRSP y devengado)
  
  if (any(c(5,"rpt5") %in% reportes)) {
    
    # me quedo con las columnas definidas en siniestro, pertenecientes a
    # contratos. 
    cols_agrupar_rpt5 <- unique(
      c(
        "CONTRATO", "PER",
        intersect(cols_agrupar, names(contratos))
      )
    )
    
    rpt5 <- arma_siniestros_rpt5(
      copy(stros_liqui_rvas),
      cierreDeMes,
      columnas_agrupar = cols_agrupar_rpt5,
      columnas_sumar = cols_sumar
    )
    
  }
  
  
  ## ** rpt6 ----
  # estadísticas por CONTRATO, Estado-consecuencia por PERIODO anual
  
  if (any(c(6,"rpt6") %in% reportes)) {
    
    # me quedo con las columnas definidas en siniestro, pertenecientes a
    # contratos. Aseguro exista MESACC
    cols_agrupar_rpt6 <- unique(
      c(
        intersect(cols_agrupar, names(contratos)), 
        "CONTRATO", "PER"
      )
    )

    rpt6 <- arma_siniestros_rpt6(
      siniestros,
      juicios,
      mes_cierre,
      cierreDeMes,
      columnas_agrupar = cols_agrupar_rpt6
    )
    
  }
  
  ## final ----
  out <-  mget(ls(pattern = "^rpt[0-9]$"))
  
  return (out)
  
}

arma_siniestros_rpt0 <- function(
  ...
){
  UseMethod("arma_siniestros_rpt0")
}

arma_siniestros_rpt0.data.frame <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes
){
  
  rpt0 <- stros_liqui_rvas_ibner %>% 
    mutate(
      PER = AAAAMM_ejercicio(MESACC, cierreDeMes),
      # totales
      TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
      TOT_LIQ_0 = ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0,
      TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA,
      TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT,
      # incurrido
      ILT_INC = ILT_LIQ + ILT_RVA,
      ESP_INC = ESP_LIQ + ESP_RVA,
      ILP_INC = ILP_LIQ + ILP_RVA,
      JUI_INC = JUI_LIQ + JUI_RVA,
      TOT_INC = TOT_LIQ + TOT_RVA,
      TOT_IBNER = ILT_IBNER + ESP_IBNER + ILP_IBNER + JUI_IBNER
    ) %>%
    mutate(
      #redondeo
      across(
        c(ends_with("_IBNER"), ends_with("_ULT")),
        round, 2L
      )
    ) %>%
    relocate(
      c(
        PER,
        ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ, TOT_LIQ,
        ILT_LIQ_0, ESP_LIQ_0, ILP_LIQ_0, JUI_LIQ_0, TOT_LIQ_0,
        ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA, TOT_RVA,
        ILT_IBNER, ESP_IBNER, ILP_IBNER, JUI_IBNER, TOT_IBNER,
        ILT_ULT, ESP_ULT, ILP_ULT, JUI_ULT, TOT_ULT
      ),
      .after = last_col()
    )
  
  return(rpt0)
}

arma_siniestros_rpt0.data.table <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes
){
  
  rpt0 <- stros_liqui_rvas_ibner[, 
  #   which(
  #     grepl("^(d|t|MES)$", colnames(stros_liqui_rvas_ibner))
  #   ):= NULL
  # ][,
    PER := AAAAMM_ejercicio(MESACC, cierreDeMes)
  ][,
    `:=`(
      # otra manera de hacerlo. Detallo los nombres de las variables
      # finales, abro un proceso {...} en cuyo paso final envío tales columnas.
      c(
        "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC", 
        "TOT_LIQ", "TOT_LIQ_0", "TOT_RVA", "TOT_IBNER", "TOT_ULT"
      ),
      {

        TOT_LIQ <- ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ
        TOT_LIQ_0 <- ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0
        TOT_RVA <- ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA
        TOT_IBNER <- ILT_IBNER + ESP_IBNER + ILP_IBNER + JUI_IBNER
        TOT_ULT <- ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT
        ILT_INC <- ILT_LIQ + ILT_RVA
        ESP_INC <- ESP_LIQ + ESP_RVA
        ILP_INC <- ILP_LIQ + ILP_RVA
        JUI_INC <- JUI_LIQ + JUI_RVA
        TOT_INC <- TOT_LIQ + TOT_RVA
        
        .(
          TOT_LIQ, TOT_LIQ_0, TOT_RVA, TOT_IBNER, TOT_ULT,
          ILT_INC, ESP_INC, ILP_INC, JUI_INC, TOT_INC
        )
      })
  ]
  
  # redondeo parece ser que no hizo falta
  
  # reordeno las columnas
  vars <- c(
    "MES", "d", "t", "PER",
    "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
    "ILT_LIQ_0", "ESP_LIQ_0", "ILP_LIQ_0", "JUI_LIQ_0", "TOT_LIQ_0",    
    "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
    "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
    "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
    "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
  )
  
  setcolorder(
    rpt0,
    c(setdiff(names(rpt0), vars), vars)
  )
  
  return(rpt0)
}

arma_siniestros_rpt_agrupa_pesos <- function(
  ...
){
  UseMethod("arma_siniestros_rpt_agrupa_pesos")
}

arma_siniestros_rpt_agrupa_pesos.data.frame <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes,
  columnas_agrupar,
  columnas_sumar
){
  
  if(is_missing(columnas_agrupar)){
    stop(
      "genera_reporte_siniestros: error interno. Falta definir columnas_agrupar"
    )
  }
  
  columnas_total <- unique(c(columnas_agrupar, columnas_sumar))
  
  rpt <- stros_liqui_rvas_ibner %>%
    # agrupo por contrato y sumo por mes
    group_by(
      across(columnas_agrupar)
    ) %>%
    mutate(
      across(
        c(
          columnas_sumar        
        ),
        ~sum(.),
        .names = "{col}"
      )
    ) %>%
    ungroup() %>%
    distinct(across(columnas_agrupar), .keep_all = TRUE) %>%
    mutate(
      # totales
      TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
      TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA,
      TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT,
      # incurrido
      ILT_INC = ILT_LIQ + ILT_RVA,
      ESP_INC = ESP_LIQ + ESP_RVA,
      ILP_INC = ILP_LIQ + ILP_RVA,
      JUI_INC = JUI_LIQ + JUI_RVA,
      TOT_INC = TOT_LIQ + TOT_RVA
    )
  
  if("ILT_LIQ_0" %in% columnas_sumar){
    rpt <- rpt %>% 
      mutate(
        TOT_LIQ_0 = ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0
      )
  }
  
  # agrego ibnr puro en pesos
  # la inflación y tipo moneda debe ser previamente cargada en el número
  if(!is.null(ibnr_puro)){
    rpt <- rpt %>%
      full_join(
        ibnr_puro %>%
          select(CONTRATO, MESACC, ILT_IBNR, ESP_IBNR, ILP_IBNR, JUI_IBNR),
        by = c("CONTRATO", "MESACC")
      ) %>%
      mutate_all(
        ~replace_na(., 0)
      ) %>%
      mutate(
        ILT_ULT = ILT_ULT + ILT_IBNR,
        ESP_ULT = ESP_ULT + ESP_IBNR,
        ILP_ULT = ILP_ULT + ILP_IBNR,
        JUI_ULT = JUI_ULT + JUI_IBNR,
        TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT
      )
  }
  
  # #redondeo
  # rpt2 <-  rpt2 %>%
  #   mutate(
  #     ~round, 2L
  #   )
  
  return(rpt)
}

arma_siniestros_rpt_agrupa_pesos.data.table <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes,
  columnas_agrupar,
  columnas_sumar
){

  if(is_missing(columnas_agrupar)){
    stop(
      "genera_reporte_siniestros: error interno. Falta definir columnas_agrupar"
    )
  }

  columnas_total <- unique(c(columnas_agrupar, columnas_sumar))

  rpt <- stros_liqui_rvas_ibner[,
    lapply(.SD, function(x) round(sum(x), 2L)), keyby = columnas_agrupar,
    .SDcols = columnas_sumar
  ]

  # totales
  rpt[, `:=`(
    TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
    TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA,
    TOT_INC = ILT_INC + ESP_INC + ILP_INC + JUI_INC,
    TOT_IBNER = ILT_IBNER + ESP_IBNER + ILP_IBNER + JUI_IBNER,
    TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT
  )]
  
  if("ILT_LIQ_0" %in% columnas_sumar){
    rpt[, `:=`(
      TOT_LIQ_0 = ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0
    )]
  }

  # reordeno las columnas
  setcolorder(
    rpt,
    unique(c(
      columnas_total,
      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER",
      {
        intersect("ILT_LIQ_0", columnas_sumar)
      }, 
      "TOT_RVA", "TOT_INC", "TOT_IBNER", "TOT_ULT"
    ))
  )
  
  return(rpt)
}

arma_siniestros_rpt_agrupa_pesos.default <- function(...){
  warning(paste(
    "no se programó la clase", class(stros_liqui_rvas_ibner),
    "en arma_siniestros_rpt_agrupa_pesos"
  ))
  return(NA)
}


arma_siniestros_rpt3 <- function(
  ...
){
  UseMethod("arma_siniestros_rpt3")
}

arma_siniestros_rpt3.data.frame <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes,
  columnas_agrupar = c("CONTRATO", "PER")
){
  
  columnas_agrupar <- unique(c(columnas_agrupar, "PER"))
  # remuevo posibles columnas que refieran a mes
  columnas_agrupar <- columnas_agrupar[
    ! columnas_agrupar %in% c("d", "t", "MES", "BIT_JUI")
  ]
  
  rpt3 <- stros_liqui_rvas_ibner %>%
    # mutate(
    #   PER = AAAAMM_ejercicio(MESACC, cierreDeMes),
    #   ILT_LIQ = if_else(t <= 0, ILT_ULT, 0),
    #   ESP_LIQ = if_else(t <= 0, ESP_ULT, 0),
    #   ILP_LIQ = if_else(t <= 0, ILP_ULT, 0),
    #   JUI_LIQ = if_else(t <= 0, JUI_ULT, 0)
    # ) %>%
    group_by(
      CONTRATO, PER
    ) %>%
    mutate(
      # agrupo por contrato y sumo por mes
      across(
        c(
          #  "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ",
          "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT"
        ),
        ~sum(.),
        .names = "{col}"
      )
    ) %>%
    ungroup() %>%
    distinct(CONTRATO, PER, .keep_all = TRUE) %>%
    mutate(
      # totales
      TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
      TOT_LIQ_0 = ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0,
      TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA,
      TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT,
      # incurrido
      ILT_INC = ILT_LIQ + ILT_RVA,
      ESP_INC = ESP_LIQ + ESP_RVA,
      ILP_INC = ILP_LIQ + ILP_RVA,
      JUI_INC = JUI_LIQ + JUI_RVA,
      TOT_INC = TOT_LIQ + TOT_RVA,
      # IBNER
      ILT_IBNER = ILT_ULT - ILT_INC,
      ESP_IBNER = ESP_ULT - ESP_INC,
      ILP_IBNER = ILP_ULT - ILP_INC,
      JUI_IBNER = JUI_ULT - JUI_INC,
      TOT_IBNER = ILT_IBNER + ESP_IBNER + ILP_IBNER + JUI_IBNER
    ) %>%
    group_by(
      CONTRATO, PER
    ) %>%
    summarise(
      across(
        c(
          ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ, TOT_LIQ,
          ILT_LIQ_0, ESP_LIQ_0, ILP_LIQ_0, JUI_LIQ_0, TOT_LIQ_0,
          ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA, TOT_RVA,
          ILT_IBNER, ESP_IBNER, ILP_IBNER, JUI_IBNER, TOT_IBNER,
          ILT_ULT, ESP_ULT, ILP_ULT, JUI_ULT, TOT_ULT
        ),
        ~sum(.)
      ),
      .groups = "drop"
    )
  
  # agrego ibnr puro en pesos
  # la inflación y tipo moneda debe ser previamente cargada en el número
  if(!is.null(ibnr_puro)){
    rpt3 <- rpt3 %>%
      full_join(
        ibnr_puro %>%
          select(CONTRATO, PER, ILT_IBNR, ESP_IBNR, ILP_IBNR, JUI_IBNR),
        by = c("CONTRATO", "PER")
      ) %>%
      mutate_all(
        ~replace_na(., 0)
      ) %>%
      mutate(
        ILT_ULT = ILT_ULT + ILT_IBNR,
        ESP_ULT = ESP_ULT + ESP_IBNR,
        ILP_ULT = ILP_ULT + ILP_IBNR,
        JUI_ULT = JUI_ULT + JUI_IBNR,
        TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT
      )
  }
  
  #redondeo
  rpt3 <-  rpt3 %>%
    mutate_all(
      ~round, 2L
    )
  
  return(rpt3)
}

arma_siniestros_rpt3.data.table <- function(
  stros_liqui_rvas_ibner,
  cierreDeMes,
  columnas_agrupar = c("CONTRATO", "PER"),
  columnas_sumar
){

  columnas_agrupar <- unique(c("CONTRATO", columnas_agrupar, "PER"))
  # remuevo posibles columnas que refieran a mes
  columnas_agrupar <- columnas_agrupar[
    ! columnas_agrupar %in% c("d", "t", "MES", "BIT_JUI")
  ]
  
  columnas_total <- unique(c(columnas_agrupar, columnas_sumar))
  
  stros_liqui_rvas_ibner <- stros_liqui_rvas_ibner[,
    PER := AAAAMM_ejercicio(MESACC, cierreDeMes)
  ][,
    columnas_total, with = FALSE
  ]

  stros_liqui_rvas_ibner <- unique(
    stros_liqui_rvas_ibner[,
     lapply(.SD, sum), keyby = columnas_agrupar,
     .SDcols = columnas_sumar
    ],
    by = columnas_agrupar
  )
  
  # totales
  
  stros_liqui_rvas_ibner[, `:=`(
    TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
    TOT_LIQ_0 = ILT_LIQ_0 + ESP_LIQ_0 + ILP_LIQ_0 + JUI_LIQ_0,
    TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA,
    TOT_INC = ILT_INC + ESP_INC + ILP_INC + JUI_INC,
    TOT_IBNER = ILT_IBNER + ESP_IBNER + ILP_IBNER + JUI_IBNER,
    TOT_ULT = ILT_ULT + ESP_ULT + ILP_ULT + JUI_ULT
  )]  
  
  # reordeno las columnas
  
  setcolorder(
    stros_liqui_rvas_ibner,
    unique(c(
      columnas_total,
      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER",
      "TOT_LIQ", "TOT_LIQ_0", "TOT_RVA", "TOT_INC", "TOT_IBNER", "TOT_ULT"
    ))
  )
  
  return(stros_liqui_rvas_ibner)
  
}


arma_siniestros_rpt3.default <- function(...){
  warning(paste(
    "no se programó la clase", class(stros_liqui_rvas_ibner),
    "en arma_siniestros_rpt3"
  ))
  return(NA)
}

arma_siniestros_rpt4 <- function(
  ...
){
  UseMethod("arma_siniestros_rpt4")
}

arma_siniestros_rpt4.data.frame <- function(
  stros_liqui_rvas_hist,
  cierreDeMes,
  columnas_agrupar = c("DENUNCIA", "PER"),
  columnas_sumar
){
  
  rpt4 <- stros_liqui_rvas_hist %>%
    mutate(
      PER = AAAAMM_ejercicio(MES, cierreDeMes),
      TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
      TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA
    ) %>%
    group_by(
      DENUNCIA, PER
    ) %>%
    summarise(
      across(
        c(
          ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ, TOT_LIQ,
          ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA, TOT_RVA
        ),
        ~round(sum(.), digits = 2)
      ),
      .groups = "drop"
    )
  
  return(rpt4)
}

arma_siniestros_rpt4.data.table <- function(
  stros_liqui_rvas_hist,
  cierreDeMes,
  columnas_agrupar = c("DENUNCIA", "PER"),
  columnas_sumar
){
  
  # columnas por default
  stros_liqui_rvas_hist[,`:=`(
    c("PER", "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC",
      "TOT_LIQ", "TOT_RVA", "TOT_INC"
    ),
    {
      PER <- AAAAMM_ejercicio(MES, cierreDeMes)
      ILT_INC <- ILT_LIQ + ILT_RVA
      ESP_INC <- ESP_LIQ + ESP_RVA
      ILP_INC <- ILP_LIQ + ILP_RVA
      JUI_INC <- JUI_LIQ + JUI_RVA
      TOT_LIQ <- ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ
      TOT_RVA <- ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA
      TOT_INC <- TOT_LIQ + TOT_RVA
      
      .(PER, ILT_INC, ESP_INC, ILP_INC, JUI_INC,
        TOT_LIQ, TOT_RVA, TOT_INC
      )
    }
  )]
  
  # columnas agrupar
  columnas_agrupar <- unique(c("DENUNCIA", "PER", columnas_agrupar))
  #remuevo columnas asociadas a movimientos mensuales
  columnas_agrupar <- columnas_agrupar[!grepl(
    "^d|t|MES|BIT_JUI$",
    columnas_agrupar
  )]
  
  columnas_sumar <- intersect(
    unique(c(
      columnas_sumar,
      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC"
    )),
    names(stros_liqui_rvas_hist)
  )
  
  columnas_total <- unique(c(columnas_agrupar, columnas_sumar))
  
  rpt4 <- stros_liqui_rvas_hist[,
    columnas_total, with = FALSE
  ]
  
  rpt4 <- unique(
    rpt4[,
      (columnas_sumar) := lapply(.SD, sum), keyby = columnas_agrupar,
      .SDcols = columnas_sumar
    ],
    by = columnas_agrupar
  )
  
  # redondeo parece ser que no hizo falta
  
  return(rpt4)
}

arma_siniestros_rpt4.default <- function(...){
  warning(paste(
    "no se programó la clase", class(stros_liqui_rvas_hist),
    "en arma_siniestros_rpt4"
  ))
  return(NA)
}


arma_siniestros_rpt5 <- function(
  ...
){
  UseMethod("arma_siniestros_rpt5")
}

arma_siniestros_rpt5.data.frame <- function(
  stros_liqui_rvas_hist,
  cierreDeMes,
  columnas_agrupar = c("CONTRATO", "PER"),
  columnas_sumar
){
  
  rpt5 <- stros_liqui_rvas_hist %>%
    mutate(
      PER = AAAAMM_ejercicio(MES, cierreDeMes),
      TOT_LIQ = ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ,
      TOT_RVA = ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA
    ) %>%
    group_by(
      CONTRATO, PER
    ) %>%
    summarise(
      across(
        c(
          ILT_LIQ, ESP_LIQ, ILP_LIQ, JUI_LIQ, TOT_LIQ,
          ILT_RVA, ESP_RVA, ILP_RVA, JUI_RVA, TOT_RVA
        ),
        ~round(sum(.), digits = 2)
      ),
      .groups = "drop"
    )
  
  return(rpt5)
}

arma_siniestros_rpt5.data.table <- function(
  stros_liqui_rvas_hist,
  cierreDeMes,
  columnas_agrupar = c("CONTRATO", "PER"),
  columnas_sumar
){
  
  # columnas por default
  stros_liqui_rvas_hist[,`:=`(
    c("PER", "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC",
      "TOT_LIQ", "TOT_RVA", "TOT_INC"
    ),
    {
      PER <- AAAAMM_ejercicio(MES, cierreDeMes)
      ILT_INC <- ILT_LIQ + ILT_RVA
      ESP_INC <- ESP_LIQ + ESP_RVA
      ILP_INC <- ILP_LIQ + ILP_RVA
      JUI_INC <- JUI_LIQ + JUI_RVA
      TOT_LIQ <- ILT_LIQ + ESP_LIQ + ILP_LIQ + JUI_LIQ
      TOT_RVA <- ILT_RVA + ESP_RVA + ILP_RVA + JUI_RVA
      TOT_INC <- TOT_LIQ + TOT_RVA
      
      .(PER, ILT_INC, ESP_INC, ILP_INC, JUI_INC,
        TOT_LIQ, TOT_RVA, TOT_INC
      )
    }
  )]
  
  # columnas agrupar
  columnas_agrupar <- unique(c("CONTRATO", "PER", columnas_agrupar))
  #remuevo columnas asociadas a movimientos mensuales
  columnas_agrupar <- columnas_agrupar[!grepl(
    "^d|t|MES|BIT_JUI$",
    columnas_agrupar
  )]
  
  columnas_sumar <- intersect(
    unique(c(
      columnas_sumar,
      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC"
    )),
    names(stros_liqui_rvas_hist)
  )
  
  columnas_total <- c(columnas_agrupar, columnas_sumar)
  
  rpt5 <- stros_liqui_rvas_hist[,
    columnas_total, with = FALSE
  ]
  
  rpt5 <- unique(
    rpt5[,
      lapply(.SD, sum), keyby = columnas_agrupar,
      .SDcols = columnas_sumar
    ],
    by = columnas_agrupar
  )
  
  # redondeo parece ser que no hizo falta
  
  return(rpt5)
}

arma_siniestros_rpt5.default <- function(...){
  warning(paste(
    "no se programó la clase", class(stros_liqui_rvas_hist),
    "en arma_siniestros_rpt5"
  ))
  return(NA)
}

arma_siniestros_rpt6 <- function(
  ...
){
  UseMethod("arma_siniestros_rpt6")
}

arma_siniestros_rpt6.data.frame <- function(
  siniestros,
  juicios,
  mes_cierre,
  cierreDeMes
){
  
  rpt6 <- siniestros %>%
    mutate (
      PER = AAAAMM_ejercicio(MESACC, cierreDeMes),
      I = 1L  # contador
    )
  
  if(!is.null(ibnr_puro)){
    rpt6 <- rpt6 %>%
      full_join(
        ibnr_puro %>% select(CONTRATO, PER, N_IBNR),
        by = c("CONTRATO", "PER")
      ) %>%
      mutate_all(
        ~replace_na(., 0L)
      ) %>%
      mutate(
        N_ULT = I + N_IBNR
      )
    
  } else {
    # borrar, debe hacerlo sólo si se pasa el IBNR
    rpt6 <- rpt6 %>%
      mutate(
        N_ULT = IBNR_siniestros_tasa(MESACC, mes_cierre)
      )
  }
  
  rpt6 <- rpt6 %>%
    complete(
      ORIGEN = c("AT", "EP", "IT"),  #antes TIPO
      TERMINACION = c("SB", "CB", "GR", "MU", "RE")
    ) %>%
    pivot_wider(
      names_from = ORIGEN, #TIPO
      values_from = I,
      values_fill = 0L
    ) %>%
    mutate(
      I = 1L
    ) %>%
    pivot_wider(
      names_from = TERMINACION,
      values_from = I,
      values_fill = 0L
    ) %>%
    filter(
      # elimino filas superfluas una vez terminado el pivot
      !is.na(DENUNCIA)
    ) %>%
    mutate ( # siniestros abiertos
      ESTADO_STRO = (ESTADO_STRO == 2) | (ESTADO_STRO == 4)
    ) %>%
    group_by (
      CONTRATO, PER
    ) %>%
    summarise(
      DIAS_CBs = sum (DIAS * CB, na.rm = TRUE),
      DIAS_GRs = sum (DIAS * GR, na.rm = TRUE),
      PORINCs = sum (PORINC * GR, na.rm = TRUE),
      PORINC_TOTs = sum (PORINC [GR == 1 & PORINC > 66], na.rm = TRUE),
      GR_T = sum (GR [PORINC > 66], na.rm = TRUE),
      SALARIOs = sum(SALARIO, na.rm = TRUE),
      EDADs = sum(EDAD, na.rm = TRUE),
      N = n (),
      A = sum (ESTADO_STRO),
      across(
        c(N_ULT, AT, EP, IT, SB, CB, GR, MU, RE),
        .fns = ~sum(.),
        .names = "{col}"
      ),
      JU = sum (JUI, na.rm = TRUE),
      C = N - A,
      .groups = "drop"
    ) %>%
    mutate(
      IBNR = N_ULT - N,
      IBNR_fc = if_else(N == 0, 1, N_ULT / N),
      DIAS_CB = if_else(CB == 0, 0, DIAS_CBs / CB),
      DIAS_GR = if_else(GR == 0, 0, DIAS_GRs / GR),
      PORINC = if_else(GR == 0, 0, PORINCs / GR),
      PORINC_66s = PORINCs - PORINC_TOTs,
      GR_66 = GR - GR_T,
      PORINC_66 = if_else(GR_66 == 0, 0, PORINC_66s / GR_66),
      SALARIO = if_else(N == 0, 0, SALARIOs / N),
      EDAD = if_else(N == 0, 0, EDADs / N)
    ) %>%
    left_join( #agrego juicios notificados
      juicios %>%
        mutate(
          PER = AAAAMM_ejercicio(AAAAMM_NOTIF_JUICIO, cierreDeMes),
          I = 1L,  # contador
        ) %>%
        group_by(
          CONTRATO, PER
        ) %>%
        summarise(
          JN = n(),
          .groups = "drop"
        ),
      by = c("CONTRATO", "PER")
    ) %>%
    mutate(
      across(
        c(JN),
        .fns = ~replace_na(., 0L),
        .names = "{col}"
      )
    ) %>%
    select ( # reordeno variables
      CONTRATO, PER,
      N, IBNR, N_ULT, IBNR_fc,
      A, C, AT, IT, EP, SB, CB, GR, GR_66, GR_T, MU, RE, JU, JN,
      DIAS_CB, DIAS_GR, PORINC, PORINC_66, EDAD, SALARIO,
      DIAS_CBs, DIAS_GRs, PORINCs, PORINC_66s, EDADs, SALARIOs
    )
  
  return(rpt6)
}

arma_siniestros_rpt6.data.table <- function(
  siniestros,
  juicios,
  mes_cierre,
  cierreDeMes,
  columnas_agrupar = c("CONTRATO", "PER")
){

  columnas_agrupar <- unique(c("CONTRATO", "PER", columnas_agrupar))
  columnas_sumar <- c(
    "N", "IBNR", "N_ULT", "IBNR_fc",
    "A", "C", "AT", "IT", "EP", "SB", "CB", "GR", "GR_66", "GR_T", "MU", "RE", "JU", "JN",
    "DIAS_CB", "DIAS_GR", "PORINC", "PORINC_66", "EDAD", "SALARIO",
    "DIAS_CBs", "DIAS_GRs", "PORINCs", "PORINC_66s", "EDADs", "SALARIOs"
  )
  
  if(nrow(siniestros) == 0L){
    siniestros[, c("CONTRATO", "PER")] <- NA_integer_
    siniestros[, columnas_sumar] <- NA_real_
    siniestros <- siniestros[, 
      match(c(columnas_agrupar, columnas_sumar), names(siniestros)),
      with = FALSE 
    ]
    
  } else {
    
    siniestros[,`:=`(
      PER = AAAAMM_ejercicio(MESACC, cierreDeMes),
      # borrar, debe hacerlo sólo si se pasa el IBNR
      N_ULT = IBNR_siniestros_tasa(MESACC, mes_cierre),
      I = 1
    )]
    
    siniestros[,`:=`(
      TERMINACION = factor(
        TERMINACION, levels = c("SB", "CB", "GR", "MU", "RE")
      ),
      ORIGEN = factor( #antes TIPO
        ORIGEN, levels = c("AT", "EP", "IT")
      ),
      # siniestros abiertos
      A = fifelse((ESTADO_STRO == 2) | (ESTADO_STRO == 4), 1L, 0L)
    )]
    
    # pivot-wider
    if(siniestros[,.N]>0){
      siniestros <- dcast(
        siniestros, ... ~ TERMINACION,
        # un drop para cada lado de la fórmula RHS ~ LHS ,
        # no queremos columnas eliminados de TERMINACION, ponemos entonces
        drop = c(TRUE, FALSE),
        fun.aggregate = length,
        value.var = "I",
        fill = 0L
      )
      
      columnas_agrupar <- columnas_agrupar[columnas_agrupar != "TERMINACION"]
      
      siniestros[,`:=`(
        I = 1L
      )]
      
      siniestros <- dcast(
        siniestros, ... ~ ORIGEN, # ~ TIPO,
        # un drop para cada lado de la fórmula RHS ~ LHS ,
        # no queremos columnas eliminados de TIPO, ponemos entonces
        drop = c(TRUE, FALSE),
        value.var = "I",
        fun.aggregate = length,
        fill = 0L
      )
      
      columnas_agrupar <- columnas_agrupar[columnas_agrupar != "ORIGEN"]
      
    }
    
    # siniestros[,`:=`(
    #   N = 1L
    # )]
    
    columnas_agrupar <- columnas_agrupar[
      !columnas_agrupar %in% setdiff(columnas_agrupar, names(siniestros))
    ]
    
    siniestros <- siniestros[,
      .(
        DIAS_CBs = sum (DIAS * CB, na.rm = TRUE),
        DIAS_GRs = sum (DIAS * GR, na.rm = TRUE),
        PORINCs = sum (PORINC * GR, na.rm = TRUE),
        PORINC_TOTs = sum (PORINC [GR == 1 & PORINC > 66], na.rm = TRUE),
        GR_T = sum (GR [PORINC > 66], na.rm = TRUE),
        SALARIOs = sum(SALARIO, na.rm = TRUE),
        EDADs = sum(EDAD, na.rm = TRUE),
        A = sum(A, na.rm = TRUE),
        N = .N,
        N_ULT = sum(N_ULT, na.rm = TRUE),
        AT = sum(AT, na.rm = TRUE),
        EP = sum(EP, na.rm = TRUE),
        IT = sum(IT, na.rm = TRUE),
        SB = sum(SB, na.rm = TRUE),
        CB = sum(CB, na.rm = TRUE),
        GR = sum(GR, na.rm = TRUE),
        MU = sum(MU, na.rm = TRUE),
        RE = sum(RE, na.rm = TRUE),
        JU = sum(JUI, na.rm = TRUE)
      ),
     keyby = columnas_agrupar
    ]
    
    siniestros[,
     `:=`(
       c("C", "IBNR", "IBNR_fc", "DIAS_CB", 
         "DIAS_GR", "PORINC", "PORINC_66s", "GR_66", "PORINC_66", "SALARIO", 
         "EDAD"
        ),
       {
         C <- N - A
         IBNR <- N_ULT - N
         IBNR_fc <- fifelse(N == 0, 1, N_ULT / N)
         DIAS_CB <- fifelse(CB == 0, 0, DIAS_CBs / CB)
         DIAS_GR <- fifelse(GR == 0, 0, DIAS_GRs / GR)
         PORINC <- fifelse(GR == 0, 0, PORINCs / GR)
         PORINC_66s <- PORINCs - PORINC_TOTs
         GR_66 <- GR - GR_T
         PORINC_66 <- fifelse(
           GR - GR_T == 0,
           0,
           (PORINCs - PORINC_TOTs) / (GR - GR_T)
         )
         SALARIO <- fifelse(N == 0, 0, SALARIOs / N)
         EDAD <- fifelse(N == 0, 0, EDADs / N)
         
         .(C, IBNR, IBNR_fc, DIAS_CB, 
           DIAS_GR, PORINC, PORINC_66s, GR_66, PORINC_66, SALARIO, 
           EDAD
         )
         
       }
     )
    ][,
      PORINC_TOTs := NULL   # molestó en un script
    ]
    
    siniestros <- merge(
      siniestros,
      juicios[, `:=`(
        PER = AAAAMM_ejercicio(AAAAMM_NOTIF_JUICIO, cierreDeMes)
      )
      ][, 
        .(JN = .N), keyby = columnas_agrupar #c("DENUNCIA", "PER")
      ],
      by = columnas_agrupar,  #c("DENUNCIA", "PER"),
      all.x = TRUE
    )
    
    siniestros[is.na(JN), JN := 0L]
    
  }  
  
  # reordeno las columnas
  
  setcolorder(
    siniestros,
    unique(c(
      columnas_agrupar,
      columnas_sumar
    ))
  )
  
  return(siniestros)
  
}

arma_siniestros_rpt6.default <- function(...){
  warning(paste(
    "no se programó la clase", class(siniestros),
    "en arma_siniestros_rpt6"
  ))
  return(NA)
}
